// CLUMPY allocation backend.
//
// This file implements the full CLUMPY-style allocation routine in C++, so that
// the hot pivot-selection + patch-growth loop runs without crossing into R per
// patch.  The R layer (R/alloc_clumpy.R) prepares numeric inputs (the landscape
// vector, per-transition probability columns and patch parameters) and calls
// `allocate_clumpy_cpp()` once per period.
//
// Two allocation methods are provided, selected by `method`:
//
//   method = 0  (uSAM, Unbiased Simple Allocation Method, thesis sec. 3.4.1):
//                a single GART pass per anterior class; every pixel that draws a
//                change is allocated as a *mono-pixel* patch.  This is the
//                bias-free simple method and is only meaningful for mono-pixel
//                patches (patch area == 1); quantity of change is enforced in
//                expectation.  Patch parameters (area, elongation,
//                avoid_aggregation) are ignored.
//
//   method = 1  (uPAM, Unbiased Patch Allocation Method, thesis sec. 3.4.2,
//                Fig. 3.2):  iterative.  GART is run over the remaining pool, a
//                batch of pivots is drawn and grown into patches, allocated (or
//                failed) pixels are removed from the pool (sampling without
//                replacement) and the per-transition quota is decremented; the
//                loop repeats until the quota is met or the pool is exhausted.
//                This is the general multi-pixel method (and also covers
//                mono-pixel patches, with a quota).
//
// The R layer auto-selects the method from the patch parameters (all mono-pixel
// => uSAM, otherwise uPAM); the explicit `method` flag here is kept so the
// backend can be exercised directly (unit tests / cross-tool comparison).
//
// Pivot rarefaction (`rarefy`): the per-cell pivot probability fed to GART is
// P(v|u,z) / E(sigma) (thesis Fig. 3.2, factor 1/E(sigma)), because each pivot
// grows into a patch of mean area E(sigma).  Without it the allocated quantity
// of change is inflated by ~mean patch size.
//
// Aggregation avoidance (`avoid_aggregation`, uPAM only): replicates clumpy's
// GaussianPatcher.  Patch growth is all-or-nothing and writes nothing until the
// whole patch is built: if at any growth step (or at the final step) a border
// cell already belongs to another patch of the same transition, or the patch
// cannot reach its sampled area, the patch fails and allocates nothing.  Failed
// (attempted) cells are removed from the pool for the rest of the time step
// (sampling without replacement), so patches never merge.
//
// Patch area distribution (`area_dist`): 0 = log-normal, 1 = normal.  Both are
// parameterised by (area_mean, area_var); the normal uses sd = sqrt(area_var)
// and clamps to >= 1 (matching clumpy's GaussianPatcher, whose `area_cov` was
// the standard deviation).
//
// All RNG goes through R's generator (R::unif_rand / R::rlnorm / R::rnorm), so
// set.seed() in R makes a run reproducible.

#include "clumpy_geometry.h"
#include <Rcpp.h>
#include <algorithm>
#include <climits>
#include <cmath>
#include <set>
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
// (cell units).  Returns an integer >= 1.  NA / non-positive mean -> 1.
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

// Normal patch-area draw: N(area_mean, sd = sqrt(area_var)), clamped to >= 1
// (matches clumpy's GaussianPatcher).  NA / non-positive mean -> 1.
static int draw_normal_area(double area_mean, double area_var) {
  if (ISNAN(area_mean) || area_mean <= 0.0) return 1;
  const double sd = (ISNAN(area_var) || area_var <= 0.0) ? 0.0 : std::sqrt(area_var);
  double draw = R::rnorm(area_mean, sd);
  if (draw < 1.0) draw = 1.0;
  long a = std::lround(draw);
  return a < 1 ? 1 : (int)a;
}

// Dispatch on area_dist: 0 = log-normal, 1 = normal.
static int draw_area(double area_mean, double area_var, int area_dist) {
  return area_dist == 1 ? draw_normal_area(area_mean, area_var)
                        : draw_lognorm_area(area_mean, area_var);
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

// Grow a single patch from `pivot0` (0-based).  Patch cells are accumulated in a
// local list and only committed to `land` (set to `to_class`) on success, so a
// failed patch leaves the landscape untouched (deferred-write rollback, like
// clumpy's GaussianPatcher).  Greedy growth: at each step pick the eligible
// border cell maximising prob / (|elongation_if_added - target| + eps), using
// running moments and the shared clumpy::elongation_from_raw_moments.
//
// `avoid_aggregation`:
//   * if a border cell already belongs to another patch of this transition
//     (ant == from_class && land == to_class), the patch FAILS (returns 0);
//   * if no eligible cell remains before reaching the target area, the patch
//     FAILS (all-or-nothing);
//   * a final check is applied to the last added cell's neighbours.
//   With avoid_aggregation == false, the patch grows greedily up to the target
//   and a short patch is accepted (no merge checks).
//
// Returns the number of cells committed (>= 1 on success; 0 on failure / invalid
// pivot).  `out_cells` always receives the cells that were *attempted* (0-based),
// so the caller can remove them from the pool on failure (without replacement).
static int grow_one_patch(std::vector<int> &land, const std::vector<int> &ant,
                          const double *probs, const std::vector<int> &up,
                          const std::vector<int> &down,
                          const std::vector<int> &left,
                          const std::vector<int> &right, int pivot0,
                          int target_area, int from_class, int to_class,
                          double elong_target, int ncol, bool avoid_aggregation,
                          std::vector<int> &out_cells) {
  out_cells.clear();
  const int n = (int)land.size();
  if (pivot0 < 0 || pivot0 >= n) return 0;
  if (land[pivot0] == NA_INTEGER) return 0;
  if (land[pivot0] != from_class || ant[pivot0] != from_class) return 0;

  std::vector<int> patch;
  patch.reserve(target_area > 0 ? target_area : 1);
  std::unordered_set<int> in_patch;
  patch.push_back(pivot0);
  in_patch.insert(pivot0);

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
  add_moments(pivot0);

  auto neighbours = [&](int c, int nb[4]) {
    nb[0] = up[c];
    nb[1] = down[c];
    nb[2] = left[c];
    nb[3] = right[c];
  };
  auto is_foreign = [&](int c) {
    return ant[c] == from_class && land[c] == to_class; // another committed patch
  };

  while ((int)patch.size() < target_area) {
    // Border of the current patch (ordered set => deterministic tie-breaks).
    std::set<int> border;
    for (int pc : patch) {
      int nb[4];
      neighbours(pc, nb);
      for (int x : nb) {
        if (x < 0 || in_patch.count(x)) continue;
        border.insert(x);
      }
    }

    if (avoid_aggregation) {
      for (int b : border) {
        if (is_foreign(b)) { // would merge with another patch -> fail
          out_cells.assign(patch.begin(), patch.end());
          return 0;
        }
      }
    }

    int best = -1;
    double best_score = -1.0;
    for (int b : border) {
      if (ant[b] != from_class || land[b] != from_class) continue; // not available
      double prob = probs ? probs[b] : 0.0;
      if (ISNAN(prob) || prob < 0.0) prob = 0.0;
      const double r = b / ncol;
      const double c = b % ncol;
      const double e = clumpy::elongation_from_raw_moments(
          m00 + 1.0, s_r + r, s_c + c, s_rr + r * r, s_cc + c * c, s_rc + r * c);
      const double score = prob / (std::abs(e - elong_target) + 1e-6);
      if (score > best_score) {
        best_score = score;
        best = b;
      }
    }

    if (best < 0) {
      if (avoid_aggregation) { // could not reach target area -> all-or-nothing
        out_cells.assign(patch.begin(), patch.end());
        return 0;
      }
      break; // partial patch accepted when not avoiding aggregation
    }

    patch.push_back(best);
    in_patch.insert(best);
    add_moments(best);
  }

  if (avoid_aggregation) {
    int nb[4];
    neighbours(patch.back(), nb);
    for (int x : nb) {
      if (x >= 0 && is_foreign(x)) {
        out_cells.assign(patch.begin(), patch.end());
        return 0;
      }
    }
  }

  for (int c : patch) land[c] = to_class; // commit
  out_cells.assign(patch.begin(), patch.end());
  return (int)patch.size();
}

// Inverse-CDF (MuST / GART) draw for one cell over the active transitions.
// `cum_prob(q)` yields the (non-negative) probability of the q-th active
// transition.  Returns the selected active-transition index, or -1 for "stay".
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

//' Normal patch-area sampler (C++)
//'
//' @param area_mean Mean patch area (cells); NA / <= 0 returns 1.
//' @param area_var  Patch-area variance (cells^2); sd = sqrt(area_var); the
//'   draw is clamped to >= 1.
//' @return Integer >= 1.
//' @keywords internal
// [[Rcpp::export]]
int sample_normal_area_cpp(double area_mean, double area_var) {
  return draw_normal_area(area_mean, area_var);
}

//' Grow a single land-use patch from a pivot cell (C++)
//'
//' Low-level patch grower (the building block where the mutable working layer
//' and the immutable anterior reference are distinct), kept for direct use /
//' unit testing.  On success the allocated cells are written back into
//' `landscape` (set to `to_class`); on failure nothing is committed.  Neighbour
//' vectors are 1-based with 0 == no neighbour, as produced by
//' [raster_neighbors_cpp()].
//'
//' @param landscape IntegerVector of current LULC values (NA_INTEGER = no-data).
//' @param ant_landscape IntegerVector of anterior (immutable) LULC values.
//' @param probs NumericVector of transition probabilities (length == landscape).
//' @param nbr_above,nbr_below,nbr_left,nbr_right Neighbour index vectors.
//' @param pivot 1-based pivot cell index.
//' @param target_area Target patch size (cells).
//' @param from_class,to_class Source/target LULC classes.
//' @param elongation Target elongation in \[0, 1\] (0 = isometric).
//' @param ncol Raster column count.
//' @param avoid_aggregation If TRUE, the patch is all-or-nothing and fails if it
//'   would merge with another patch or cannot reach `target_area`.
//' @return 1-based integer vector of allocated cell indices (incl. pivot), or
//'   empty if the patch failed / the pivot is not an available `from_class` cell.
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
                             double elongation, int ncol,
                             bool avoid_aggregation = false) {
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
  int s = grow_one_patch(land, ant, pr.data(), up, down, left, right, pivot - 1,
                         target_area, from_class, to_class, elongation, ncol,
                         avoid_aggregation, out);

  if (s > 0) {
    for (int idx : out) landscape[idx] = to_class; // reflect commit in caller
    IntegerVector res(out.size());
    for (size_t i = 0; i < out.size(); ++i) res[i] = out[i] + 1;
    return res;
  }
  return IntegerVector(0); // failed / invalid pivot
}

// ---------------------------------------------------------------------------
// Exported: full allocation routine
// ---------------------------------------------------------------------------

//' Run the CLUMPY allocation routine (C++)
//'
//' @description
//' Allocates LULC change for a single period.  See the file header for the
//' uSAM vs uPAM methods and the meaning of `rarefy` / `avoid_aggregation` /
//' `area_dist`.  The anterior reference is snapshotted internally from
//' `landscape`, so a cell is eligible as a pivot only while it still equals its
//' original source class (prevents a cell changing twice in one time step).
//'
//' @param landscape IntegerVector of the anterior LULC state (row-major,
//'   1-based class ids, NA_INTEGER for no-data).  Not modified; a copy is
//'   returned with the allocated changes applied.
//' @param nrow,ncol Raster dimensions.
//' @param trans_from,trans_to IntegerVectors (length T) of the source/target
//'   class for each transition column of `probs`.  The set of anterior classes
//'   is derived from `trans_from`.
//' @param probs NumericMatrix (n_cells x T) of per-cell transition potentials,
//'   already adjusted/closed; column t corresponds to transition t.
//' @param area_mean,area_var,elongation NumericVectors (length T) of patch
//'   parameters per transition.
//' @param target_rate NumericVector (length T) of the target transition rate
//'   P(v|u) per transition (fraction of source pixels that change).  Used only
//'   by uPAM to set the per-transition pixel quota.
//' @param method 0 = uSAM (mono-pixel single pass), 1 = uPAM (iterative, quota).
//' @param batch_size uPAM only: pivots attempted per GART re-draw (1 = strict
//'   uPAM, <= 0 = all candidates).
//' @param rarefy If TRUE, divide pivot probabilities by `area_mean` (the
//'   1/E(sigma) factor) so the allocated quantity of change matches the target.
//' @param shuffle If TRUE, randomise pivot processing order.
//' @param avoid_aggregation uPAM only: if TRUE, patches that would merge fail
//'   and allocate nothing (clumpy GaussianPatcher semantics).
//' @param area_dist Patch-area distribution: 0 = log-normal, 1 = normal.
//' @return IntegerVector (length n_cells) of the posterior LULC state.
//' @keywords internal
// [[Rcpp::export]]
IntegerVector allocate_clumpy_cpp(
    IntegerVector landscape, int nrow, int ncol, IntegerVector trans_from,
    IntegerVector trans_to, NumericMatrix probs, NumericVector area_mean,
    NumericVector area_var, NumericVector elongation, NumericVector target_rate,
    int method, int batch_size, bool rarefy, bool shuffle,
    bool avoid_aggregation, int area_dist) {
  const int n = landscape.size();
  const int T = trans_from.size();

  std::vector<int> land(landscape.begin(), landscape.end());
  const std::vector<int> ant(land); // immutable anterior snapshot

  std::vector<int> up, down, left, right;
  build_neighbors(nrow, ncol, up, down, left, right);

  // Anterior classes to process, derived from trans_from.
  std::set<int> from_set(trans_from.begin(), trans_from.end());

  // Effective (clamped, optionally rarefied) pivot-selection probability.
  auto pivot_prob = [&](int cell, int t) -> double {
    double p = probs(cell, t);
    if (ISNAN(p) || p < 0.0) p = 0.0;
    if (rarefy) {
      const double am = area_mean[t];
      if (!ISNAN(am) && am > 1.0) p /= am; // 1/E(sigma); skip mono-pixel patches
    }
    return p;
  };

  for (int fc : from_set) {
    std::vector<int> at; // transition indices with trans_from == fc
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
      // ---- uSAM: single GART pass, mono-pixel allocation ----------------
      const int m = (int)pool.size();
      std::vector<int> sampled(m);
      for (int i = 0; i < m; ++i) {
        const int cell = pool[i];
        const int sel =
            gart_draw_one(k, [&](int q) { return pivot_prob(cell, at[q]); });
        sampled[i] = (sel < 0) ? -1 : at[sel];
      }
      for (int i = 0; i < m; ++i) {
        if (sampled[i] < 0) continue;
        const int pv = pool[i];
        if (land[pv] == fc && ant[pv] == fc) land[pv] = trans_to[sampled[i]];
      }
    } else {
      // ---- uPAM: iterative GART + quota + without-replacement ------------
      std::vector<double> remaining(k);
      const double m0 = (double)pool.size();
      for (int q = 0; q < k; ++q) {
        double rt = target_rate[at[q]];
        if (ISNAN(rt) || rt < 0.0) rt = 0.0;
        remaining[q] = rt * m0;
      }
      std::vector<char> blocked(n, 0); // attempted-but-failed cells (removed)
      const int bs = (batch_size <= 0) ? INT_MAX : batch_size;

      int guard = 0;
      const int guard_max = n + 16;
      while (guard++ < guard_max) {
        bool any_quota = false;
        for (int q = 0; q < k; ++q) {
          if (remaining[q] > 0.0) {
            any_quota = true;
            break;
          }
        }
        if (!any_quota) break;

        // Compact the pool to cells still available and not blocked.
        pool.erase(std::remove_if(pool.begin(), pool.end(),
                                  [&](int idx) {
                                    return blocked[idx] ||
                                           !(land[idx] == fc && ant[idx] == fc);
                                  }),
                   pool.end());
        if (pool.empty()) break;

        // GART over the current pool, restricted to in-quota transitions.
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
        std::vector<int> out;
        for (size_t c = 0; c < cand_cell.size() && processed < bs; ++c) {
          const int q = cand_q[c];
          if (remaining[q] <= 0.0) continue;
          const int pv = cand_cell[c];
          if (blocked[pv] || land[pv] != fc || ant[pv] != fc) continue;
          const int t = at[q];
          const int to = trans_to[t];
          const int area = draw_area(area_mean[t], area_var[t], area_dist);
          const double *col = &probs(0, t);
          const int s =
              grow_one_patch(land, ant, col, up, down, left, right, pv, area, fc,
                             to, elongation[t], ncol, avoid_aggregation, out);
          ++processed; // every attempt counts toward the batch
          if (s > 0) {
            remaining[q] -= (double)s;
          } else {
            for (int b : out) blocked[b] = 1; // remove attempted cells
          }
        }
        // Each iteration removes >= 1 cell from the pool (a success commits
        // to_class, a failure blocks the attempted cells), so the loop is
        // guaranteed to terminate.
      }
    }
  }

  IntegerVector res(n);
  for (int i = 0; i < n; ++i) res[i] = land[i];
  return res;
}
