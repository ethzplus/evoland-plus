// CLUMPY allocation backend.
//
// This file implements the full CLUMPY-style allocation routine in C++, so that
// the hot pivot-selection + patch-growth loop runs without crossing into R per
// patch.  The R layer (R/alloc_clumpy.R) only prepares numeric inputs (the
// landscape vectors, per-transition probability columns and patch parameters)
// and calls `allocate_clumpy_cpp()` once per period.
//
// Two allocation methods are provided, selected by `method`:
//
//   method = 0  (uSAM, Unbiased Simple Allocation Method, thesis sec. 3.4.1 /
//                App. 3.D.1):  a single GART pass per anterior class; every
//                pixel that draws a change becomes a pivot and is grown into a
//                patch.  Quantity of change is enforced only in expectation,
//                via the (rarefied) transition potentials.  Cheapest.
//
//   method = 1  (uPAM, Unbiased Patch Allocation Method, thesis sec. 3.4.2,
//                Fig. 3.2):  iterative.  GART is run over the remaining pool,
//                a batch of pivots is drawn and grown, the allocated pixels are
//                removed from the pool (sampling without replacement) and the
//                per-transition quota is decremented; the loop repeats until
//                the quota is met or no further patch can be formed.
//
//                Crucially, because evoland's transition potentials come from a
//                fixed fitted model (not pool-dependent empirical KDE densities
//                as in Mazy's reference clumpy), the marginal rho(z|u) does NOT
//                need to be re-estimated between patches -- the potentials are
//                pool-independent by construction.  So the only per-patch work
//                is cheap bookkeeping (quota decrement + pool removal + re-run
//                of GART), which is why uPAM is affordable here.
//
//                `batch_size` is the speed/accuracy dial: 1 == strict uPAM (one
//                pivot per GART draw, all others dismissed -- thesis Fig. 3.2);
//                larger values process more pivots between GART re-draws; <= 0
//                means "all candidates", i.e. one GART pass per quota loop.
//
// Pivot rarefaction (`rarefy`): the per-cell pivot probability fed to GART is
// P(v|u,z) / E(sigma) (thesis Fig. 3.2, factor 1/E(sigma)), because each pivot
// grows into a patch of mean area E(sigma).  Without it the allocated quantity
// of change is inflated by ~mean patch size.
//
// All RNG goes through R's generator (R::unif_rand / R::rlnorm), so set.seed()
// in R makes a run reproducible.

#include "clumpy_geometry.h"
#include <Rcpp.h>
#include <algorithm>
#include <climits>
#include <cmath>
#include <unordered_set>
#include <vector>

using namespace Rcpp;

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

// Rook-adjacency neighbour indices (0-based, -1 == no neighbour / edge) for a
// row-major (nrow x ncol) raster.
static void build_neighbors(int nrow, int ncol, std::vector<int> &up,
                            std::vector<int> &down, std::vector<int> &left,
                            std::vector<int> &right) {
  const int n = nrow * ncol;
  up.assign(n, -1);
  down.assign(n, -1);
  left.assign(n, -1);
  right.assign(n, -1);
  for (int idx = 0; idx < n; ++idx) {
    const int r = idx / ncol;
    const int c = idx % ncol;
    if (r > 0) up[idx] = idx - ncol;
    if (r < nrow - 1) down[idx] = idx + ncol;
    if (c > 0) left[idx] = idx - 1;
    if (c < ncol - 1) right[idx] = idx + 1;
  }
}

// Log-normal patch-area draw, parameterised by the area mean E and variance V
// (in cell units).  Returns an integer >= 1.  NA / non-positive mean -> 1.
static int draw_lognorm_area(double area_mean, double area_var) {
  if (ISNAN(area_mean) || area_mean <= 0.0) return 1;
  const double E = area_mean;
  const double V = (ISNAN(area_var) || area_var <= 0.0) ? 1.0 : area_var;
  const double mu = std::log(E * E / std::sqrt(V + E * E));
  const double sigma = std::sqrt(std::log(V / (E * E) + 1.0));
  const double draw = R::rlnorm(mu, sigma);
  long a = std::lround(draw);
  return a < 1 ? 1 : (int)a;
}

// In-place Fisher-Yates shuffle of `v` using R's RNG (so set.seed() applies).
static void shuffle_in_place(std::vector<int> &v) {
  for (int i = (int)v.size() - 1; i > 0; --i) {
    int j = (int)std::floor(R::unif_rand() * (i + 1));
    if (j > i) j = i; // guard against unif_rand() == 1.0 edge
    std::swap(v[i], v[j]);
  }
}

// Shuffle two parallel vectors with the same permutation.
static void shuffle_pair(std::vector<int> &a, std::vector<int> &b) {
  for (int i = (int)a.size() - 1; i > 0; --i) {
    int j = (int)std::floor(R::unif_rand() * (i + 1));
    if (j > i) j = i;
    std::swap(a[i], a[j]);
    std::swap(b[i], b[j]);
  }
}

// Grow a single patch from `pivot0` (0-based) into `land` (modified in place:
// allocated cells set to `to_class`).  Greedy: at each step pick the eligible
// border cell maximising prob / (|elongation_if_added - target| + eps), where
// elongation uses the shared clumpy::elongation_from_raw_moments via running
// moment accumulators (O(1) per candidate evaluation).
//
// `probs` points to the per-cell transition potential column for this
// transition (raw, length == land.size()), used to weight border cells.
//
// Returns the number of cells allocated (>= 1 on success; 0 if the pivot is not
// an available `from_class` cell).  Allocated 0-based indices are written to
// `out_cells`.
static int grow_one_patch(std::vector<int> &land, const std::vector<int> &ant,
                          const double *probs, const std::vector<int> &up,
                          const std::vector<int> &down,
                          const std::vector<int> &left,
                          const std::vector<int> &right, int pivot0,
                          int target_area, int from_class, int to_class,
                          double elong_target, int ncol,
                          std::vector<int> &out_cells) {
  out_cells.clear();
  const int n = (int)land.size();
  if (pivot0 < 0 || pivot0 >= n) return 0;
  if (land[pivot0] == NA_INTEGER) return 0;
  if (land[pivot0] != from_class) return 0;

  // Running raw spatial moments of the patch (row/col coordinates).
  double m00 = 0, s_r = 0, s_c = 0, s_rr = 0, s_cc = 0, s_rc = 0;
  auto add_moments = [&](int idx) {
    const double r = idx / ncol;
    const double c = idx % ncol;
    m00 += 1.0;
    s_r += r;
    s_c += c;
    s_rr += r * r;
    s_cc += c * c;
    s_rc += r * c;
  };

  // Eligible border cells, kept in insertion order for deterministic tie-breaks
  // (consumed entries are skipped lazily once they leave `from_class`).
  std::vector<int> eligible;
  std::unordered_set<int> seen;
  auto add_neighbors = [&](int cell) {
    const int nbrs[4] = {up[cell], down[cell], left[cell], right[cell]};
    for (int nb : nbrs) {
      if (nb < 0) continue;
      if (land[nb] == NA_INTEGER) continue;
      if (ant[nb] != from_class) continue;  // only originally-from_class cells
      if (land[nb] != from_class) continue; // already converted this step
      if (seen.count(nb)) continue;
      seen.insert(nb);
      eligible.push_back(nb);
    }
  };

  land[pivot0] = to_class;
  out_cells.push_back(pivot0);
  add_moments(pivot0);
  add_neighbors(pivot0);

  while ((int)out_cells.size() < target_area) {
    int best = -1;
    double best_score = -1.0;
    for (int cand : eligible) {
      if (land[cand] != from_class) continue; // consumed / no longer eligible
      double prob = probs ? probs[cand] : 0.0;
      if (ISNAN(prob) || prob < 0.0) prob = 0.0;

      const double r = cand / ncol;
      const double c = cand % ncol;
      const double e = clumpy::elongation_from_raw_moments(
          m00 + 1.0, s_r + r, s_c + c, s_rr + r * r, s_cc + c * c,
          s_rc + r * c);
      const double score = prob / (std::abs(e - elong_target) + 1e-6);
      if (score > best_score) {
        best_score = score;
        best = cand;
      }
    }
    if (best < 0) break; // no eligible cell left -> patch smaller than target

    land[best] = to_class;
    out_cells.push_back(best);
    add_moments(best);
    add_neighbors(best);
  }

  return (int)out_cells.size();
}

// Inverse-CDF (MuST / GART) draw for one cell over the active transitions.
// `cum_prob(q)` must yield the (already non-negative) probability of the q-th
// active transition.  Returns the selected active-transition index, or -1 for
// "stay" (no change).  Consumes exactly one uniform.
template <typename F> static int gart_draw_one(int k, F cum_prob) {
  const double u = R::unif_rand();
  double cs = 0.0;
  for (int q = 0; q < k; ++q) {
    cs += cum_prob(q);
    if (u < cs) return q;
  }
  return -1; // stay
}

// ---------------------------------------------------------------------------
// Exported: small building blocks (also individually unit-tested)
// ---------------------------------------------------------------------------

//' Rook-adjacency neighbour indices for a raster (C++)
//'
//' @param nrow,ncol Raster dimensions.
//' @return Named list `above`/`below`/`left`/`right`, each a 1-based cell index
//'   per cell (row-major) with 0 meaning "no neighbour" (edge).
//' @keywords internal
// [[Rcpp::export]]
List raster_neighbors_cpp(int nrow, int ncol) {
  std::vector<int> up, down, left, right;
  build_neighbors(nrow, ncol, up, down, left, right);
  const int n = nrow * ncol;
  IntegerVector above(n), below(n), lft(n), rgt(n);
  for (int i = 0; i < n; ++i) {
    above[i] = up[i] < 0 ? 0 : up[i] + 1;
    below[i] = down[i] < 0 ? 0 : down[i] + 1;
    lft[i] = left[i] < 0 ? 0 : left[i] + 1;
    rgt[i] = right[i] < 0 ? 0 : right[i] + 1;
  }
  return List::create(_["above"] = above, _["below"] = below, _["left"] = lft,
                      _["right"] = rgt);
}

//' Generalized Allocation Rejection Test (GART / MuST) in C++
//'
//' Inverse-CDF multinomial draw of a final state per cell.  NaN and negative
//' probabilities are clamped to 0 (matching the reference `clumpy` Python).
//'
//' @param P Numeric matrix (n_cells x n_states); each row should sum to ~1
//'   (include the "stay" column).
//' @param states Integer vector of length `ncol(P)` giving the state id of each
//'   column.
//' @return Integer vector of length `nrow(P)` with the sampled state per cell.
//' @keywords internal
// [[Rcpp::export]]
IntegerVector gart_cpp(NumericMatrix P, IntegerVector states) {
  const int n = P.nrow();
  const int k = P.ncol();
  if ((int)states.size() != k) {
    stop("length(states) must equal ncol(P)");
  }
  IntegerVector y(n);
  for (int i = 0; i < n; ++i) {
    int sel = gart_draw_one(k, [&](int q) {
      double p = P(i, q);
      if (ISNAN(p) || p < 0.0) p = 0.0;
      return p;
    });
    y[i] = states[sel < 0 ? k - 1 : sel]; // sel<0 only if row sums < u
  }
  return y;
}

//' Log-normal patch-area sampler (C++)
//'
//' @param area_mean Mean patch area (cells); NA / <= 0 returns 1.
//' @param area_var  Patch-area variance (cells^2); NA / <= 0 treated as 1.
//' @return Integer >= 1.
//' @keywords internal
// [[Rcpp::export]]
int sample_lognorm_area_cpp(double area_mean, double area_var) {
  return draw_lognorm_area(area_mean, area_var);
}

//' Grow a single land-use patch from a pivot cell (C++)
//'
//' Thin wrapper around the internal patch grower, kept for direct use / unit
//' testing.  `landscape` is modified in place (allocated cells set to
//' `to_class`).  Neighbour vectors are 1-based with 0 == no neighbour, as
//' produced by [raster_neighbors_cpp()].
//'
//' @param landscape IntegerVector of current LULC values (NA_INTEGER = no-data).
//' @param ant_landscape IntegerVector of anterior (immutable) LULC values.
//' @param probs NumericVector of transition probabilities (length == landscape).
//' @param nbr_above,nbr_below,nbr_left,nbr_right Neighbour index vectors.
//' @param pivot 1-based pivot cell index.
//' @param target_area Target patch size (cells).
//' @param from_class,to_class Source/target LULC classes.
//' @param eccentricity Target elongation in \[0, 1\] (0 = isometric).
//' @param ncol Raster column count.
//' @return 1-based integer vector of allocated cell indices (incl. pivot), or
//'   empty if the pivot is not an available `from_class` cell.
//' @keywords internal
// [[Rcpp::export]]
IntegerVector grow_patch_cpp(IntegerVector landscape,
                             const IntegerVector &ant_landscape,
                             const NumericVector &probs,
                             const IntegerVector &nbr_above,
                             const IntegerVector &nbr_below,
                             const IntegerVector &nbr_left,
                             const IntegerVector &nbr_right, int pivot,
                             int target_area, int from_class, int to_class,
                             double eccentricity, int ncol) {
  const int n = landscape.size();
  std::vector<int> land(landscape.begin(), landscape.end());
  std::vector<int> ant(ant_landscape.begin(), ant_landscape.end());
  std::vector<int> up(n), down(n), left(n), right(n);
  for (int i = 0; i < n; ++i) {
    up[i] = nbr_above[i] - 1; // 1-based (0 = none) -> 0-based (-1 = none)
    down[i] = nbr_below[i] - 1;
    left[i] = nbr_left[i] - 1;
    right[i] = nbr_right[i] - 1;
  }
  std::vector<double> pr(probs.begin(), probs.end());

  std::vector<int> out;
  grow_one_patch(land, ant, pr.data(), up, down, left, right, pivot - 1,
                 target_area, from_class, to_class, eccentricity, ncol, out);

  // Reflect the allocation back into the caller's landscape (in place).
  for (int idx : out) landscape[idx] = to_class;

  IntegerVector res(out.size());
  for (size_t i = 0; i < out.size(); ++i) res[i] = out[i] + 1;
  return res;
}

// ---------------------------------------------------------------------------
// Exported: full allocation routine
// ---------------------------------------------------------------------------

//' Run the CLUMPY allocation routine (C++)
//'
//' @description
//' Allocates LULC change for a single period.  See the file header for the
//' uSAM vs uPAM methods and the meaning of `batch_size` / `rarefy`.
//'
//' @param landscape IntegerVector of the anterior LULC state (row-major,
//'   1-based class ids, NA_INTEGER for no-data).  Not modified; a copy is
//'   returned with the allocated changes applied.
//' @param ant_landscape IntegerVector of the anterior state (immutable
//'   reference; a cell is only eligible as a pivot while both `landscape` and
//'   `ant_landscape` still equal its source class).
//' @param nrow,ncol Raster dimensions.
//' @param from_classes IntegerVector of anterior classes to process.
//' @param trans_from,trans_to IntegerVectors (length T) of the source/target
//'   class for each transition column of `probs`.
//' @param probs NumericMatrix (n_cells x T) of per-cell transition potentials,
//'   already adjusted/closed; column t corresponds to transition t.
//' @param area_mean,area_var,elongation NumericVectors (length T) of patch
//'   parameters per transition.
//' @param target_rate NumericVector (length T) of the target transition rate
//'   P(v|u) per transition (fraction of source pixels that change).  Used only
//'   by uPAM to set the per-transition pixel quota.
//' @param method 0 = uSAM (single pass), 1 = uPAM (iterative with quota).
//' @param batch_size uPAM only: pivots processed per GART re-draw (1 = strict
//'   uPAM, <= 0 = all candidates).
//' @param rarefy If TRUE, divide pivot probabilities by `area_mean` (the
//'   1/E(sigma) factor) so the allocated quantity of change matches the target.
//' @param shuffle If TRUE, randomise pivot processing order.
//' @return IntegerVector (length n_cells) of the posterior LULC state.
//' @keywords internal
// [[Rcpp::export]]
IntegerVector allocate_clumpy_cpp(
    IntegerVector landscape, IntegerVector ant_landscape, int nrow, int ncol,
    IntegerVector from_classes, IntegerVector trans_from, IntegerVector trans_to,
    NumericMatrix probs, NumericVector area_mean, NumericVector area_var,
    NumericVector elongation, NumericVector target_rate, int method,
    int batch_size, bool rarefy, bool shuffle) {
  const int n = landscape.size();
  const int T = trans_from.size();

  std::vector<int> land(landscape.begin(), landscape.end());
  std::vector<int> ant(ant_landscape.begin(), ant_landscape.end());

  std::vector<int> up, down, left, right;
  build_neighbors(nrow, ncol, up, down, left, right);

  // Effective (clamped, optionally rarefied) pivot-selection probability used
  // by GART.  The patch grower uses the *raw* potential column instead.
  auto pivot_prob = [&](int cell, int t) -> double {
    double p = probs(cell, t);
    if (ISNAN(p) || p < 0.0) p = 0.0;
    if (rarefy) {
      const double am = area_mean[t];
      if (!ISNAN(am) && am > 1.0) p /= am; // 1/E(sigma); skip mono-pixel patches
    }
    return p;
  };

  for (int fc_i = 0; fc_i < (int)from_classes.size(); ++fc_i) {
    const int fc = from_classes[fc_i];

    // Active transition columns for this anterior class.
    std::vector<int> at; // global transition indices with trans_from == fc
    for (int t = 0; t < T; ++t) {
      if (trans_from[t] == fc) at.push_back(t);
    }
    if (at.empty()) continue;
    const int k = (int)at.size();

    // Pool of available source cells (originally fc and not yet changed).
    std::vector<int> pool;
    for (int idx = 0; idx < n; ++idx) {
      if (land[idx] == fc && ant[idx] == fc) pool.push_back(idx);
    }
    if (pool.empty()) continue;

    if (method == 0) {
      // ---- uSAM: single GART pass over the whole pool -------------------
      const int m = (int)pool.size();
      std::vector<int> sampled(m); // chosen global transition idx, or -1 = stay
      for (int i = 0; i < m; ++i) {
        const int cell = pool[i];
        const int sel =
            gart_draw_one(k, [&](int q) { return pivot_prob(cell, at[q]); });
        sampled[i] = (sel < 0) ? -1 : at[sel];
      }

      for (int q = 0; q < k; ++q) {
        const int t = at[q];
        const int to = trans_to[t];
        std::vector<int> pivots;
        for (int i = 0; i < m; ++i) {
          if (sampled[i] == t) pivots.push_back(pool[i]);
        }
        if (pivots.empty()) continue;
        if (shuffle) shuffle_in_place(pivots);

        const double *col = &probs(0, t);
        std::vector<int> out;
        for (int pv : pivots) {
          if (land[pv] != fc || ant[pv] != fc) continue; // taken by a neighbour
          const int area = draw_lognorm_area(area_mean[t], area_var[t]);
          grow_one_patch(land, ant, col, up, down, left, right, pv, area, fc, to,
                         elongation[t], ncol, out);
        }
      }
    } else {
      // ---- uPAM: iterative GART + quota + without-replacement ------------
      std::vector<double> remaining(k); // target pixel count per active trans
      const double m0 = (double)pool.size();
      for (int q = 0; q < k; ++q) {
        double rt = target_rate[at[q]];
        if (ISNAN(rt) || rt < 0.0) rt = 0.0;
        remaining[q] = rt * m0;
      }
      const int bs = (batch_size <= 0) ? INT_MAX : batch_size;

      while (true) {
        // Stop once every transition's quota is filled.
        bool any_quota = false;
        for (int q = 0; q < k; ++q) {
          if (remaining[q] > 0.0) {
            any_quota = true;
            break;
          }
        }
        if (!any_quota) break;

        // Compact the pool to cells still available.
        pool.erase(std::remove_if(pool.begin(), pool.end(),
                                  [&](int idx) {
                                    return !(land[idx] == fc && ant[idx] == fc);
                                  }),
                   pool.end());
        if (pool.empty()) break;

        // GART over the current pool, restricted to transitions still in quota.
        std::vector<int> cand_cell, cand_q;
        for (int idx : pool) {
          const int sel = gart_draw_one(k, [&](int q) {
            return remaining[q] > 0.0 ? pivot_prob(idx, at[q]) : 0.0;
          });
          if (sel >= 0) {
            cand_cell.push_back(idx);
            cand_q.push_back(sel);
          }
        }
        if (cand_cell.empty()) break;
        if (shuffle) shuffle_pair(cand_cell, cand_q);

        int processed = 0;
        bool progress = false;
        std::vector<int> out;
        for (size_t c = 0; c < cand_cell.size() && processed < bs; ++c) {
          const int q = cand_q[c];
          if (remaining[q] <= 0.0) continue;
          const int pv = cand_cell[c];
          if (land[pv] != fc || ant[pv] != fc) continue; // taken in this batch
          const int t = at[q];
          const int to = trans_to[t];
          const double *col = &probs(0, t);
          const int area = draw_lognorm_area(area_mean[t], area_var[t]);
          const int s = grow_one_patch(land, ant, col, up, down, left, right, pv,
                                       area, fc, to, elongation[t], ncol, out);
          if (s > 0) {
            remaining[q] -= (double)s;
            ++processed;
            progress = true;
          }
        }
        if (!progress) break; // no patch could be formed -> avoid infinite loop
      }
    }
  }

  IntegerVector res(n);
  for (int i = 0; i < n; ++i) res[i] = land[i];
  return res;
}
