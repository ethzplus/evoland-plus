// Patch growth algorithm for the CLUMPY allocation backend.
//
// grow_patch_cpp: Grows a single patch from a pivot cell.
//
// The patch grower works on a flat (row-major) raster representation:
// - landscape: 1D vector of current LULC values (NA_INTEGER for missing)
// - probs    : 1D vector of transition probabilities for this transition
// - nbr_*    : pre-computed 1D neighbor indices (0 = no neighbor / edge)
//              stored as 1-based R indices; 0 means "no neighbor"
//
// Patch growth uses a greedy strategy: at each step, among all eligible
// border cells, pick the one with the highest transition probability.
// Eccentricity targeting (CLUMPY section 2.5) is approximated by using
// the second central moments of the current patch shape and choosing the
// neighbor that brings the eccentricity closest to the target.
//
// Returns a 1-based integer vector of allocated cell indices.

#include <Rcpp.h>
#include <cmath>
#include <queue>
#include <unordered_set>
#include <vector>

using namespace Rcpp;

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

// Compute current patch eccentricity from the set of allocated cell indices.
// Cells are stored as 1-based flat indices in a (nrow x ncol) row-major grid.
// Eccentricity = 1 - sqrt(lambda2 / lambda1) where lambda1 >= lambda2 are the
// eigenvalues of the 2x2 second-central-moment matrix of the patch.
static double patch_eccentricity(
    const std::vector<int> &allocated, int ncol
) {
  if (allocated.size() <= 1) return 0.0;

  int n = (int)allocated.size();
  double sum_r = 0.0, sum_c = 0.0;

  for (int cell : allocated) {
    int r = (cell - 1) / ncol; // 0-based row
    int c = (cell - 1) % ncol; // 0-based col
    sum_r += r;
    sum_c += c;
  }

  double mean_r = sum_r / n;
  double mean_c = sum_c / n;

  double mu20 = 0.0, mu02 = 0.0, mu11 = 0.0;
  for (int cell : allocated) {
    double dr = (cell - 1) / ncol - mean_r;
    double dc = (cell - 1) % ncol - mean_c;
    mu20 += dr * dr;
    mu02 += dc * dc;
    mu11 += dr * dc;
  }
  mu20 /= n;
  mu02 /= n;
  mu11 /= n;

  double trace = mu20 + mu02;
  if (trace <= 0.0) return 0.0;

  double delta = std::sqrt(
      std::pow(mu20 - mu02, 2.0) + 4.0 * mu11 * mu11
  );

  double lambda1 = (trace + delta) / 2.0;
  double lambda2 = (trace - delta) / 2.0;

  if (lambda1 <= 0.0) return 0.0;

  double ecc = 1.0 - std::sqrt(std::max(0.0, lambda2) / lambda1);
  return ecc;
}

// ---------------------------------------------------------------------------
// Exported function
// ---------------------------------------------------------------------------

//' Grow a single land-use patch from a pivot cell
//'
//' @description
//' Greedy patch grower for CLUMPY-style allocation.  Starting from `pivot`
//' (1-based flat cell index in a row-major raster), the algorithm grows the
//' patch to `target_area` pixels by repeatedly selecting the eligible border
//' cell whose addition would minimise the deviation from `eccentricity` while
//' weighting by transition probability.
//'
//' @param landscape IntegerVector of current LULC values (NA_INTEGER = no-data).
//'   Modified in place.
//' @param ant_landscape IntegerVector of anterior (immutable) LULC values.
//' @param probs NumericVector of transition probabilities (same length as landscape).
//' @param nbr_above IntegerVector – 1-based index of the cell above each cell,
//'   or 0 if at the top edge.
//' @param nbr_below IntegerVector – cell below, or 0.
//' @param nbr_left  IntegerVector – cell to the left,  or 0.
//' @param nbr_right IntegerVector – cell to the right, or 0.
//' @param pivot 1-based cell index of the pivot (kernel) cell.
//' @param target_area Target number of pixels for the patch.
//' @param from_class The LULC class that can be converted.
//' @param to_class   The LULC class to convert to.
//' @param eccentricity Target eccentricity in [0, 1].
//' @param ncol Number of columns in the raster (needed for eccentricity calc).
//'
//' @return 1-based integer vector of cell indices that form the grown patch
//'   (including the pivot).  Returns an empty vector if the pivot is not
//'   `from_class` or is already converted.
//'
//' @keywords internal
// [[Rcpp::export]]
IntegerVector grow_patch_cpp(
    IntegerVector landscape,
    const IntegerVector &ant_landscape,
    const NumericVector &probs,
    const IntegerVector &nbr_above,
    const IntegerVector &nbr_below,
    const IntegerVector &nbr_left,
    const IntegerVector &nbr_right,
    int pivot,
    int target_area,
    int from_class,
    int to_class,
    double eccentricity,
    int ncol
) {
  int n_cells = landscape.size();

  // Validate pivot
  if (pivot < 1 || pivot > n_cells) return IntegerVector(0);
  if (landscape[pivot - 1] == NA_INTEGER) return IntegerVector(0);
  if (landscape[pivot - 1] != from_class) return IntegerVector(0);

  // Track allocated cells
  std::vector<int> allocated;
  allocated.push_back(pivot);
  landscape[pivot - 1] = to_class;

  // Set of cells currently in the patch (for O(1) membership test)
  std::unordered_set<int> in_patch;
  in_patch.insert(pivot);

  // Eligible border cells (candidates for expansion)
  // We keep them in a vector and rebuild lazily.
  std::unordered_set<int> eligible_set;

  // Add initial neighbors of pivot
  auto add_neighbors = [&](int cell) {
    int nbrs[4] = {nbr_above[cell - 1], nbr_below[cell - 1],
                   nbr_left[cell - 1],  nbr_right[cell - 1]};
    for (int nb : nbrs) {
      if (nb == 0) continue;
      if (in_patch.count(nb)) continue;
      if (landscape[nb - 1] == NA_INTEGER) continue;
      if (ant_landscape[nb - 1] != from_class) continue;
      if (landscape[nb - 1] != from_class) continue; // already converted
      eligible_set.insert(nb);
    }
  };

  add_neighbors(pivot);

  while ((int)allocated.size() < target_area && !eligible_set.empty()) {
    // Pick the best candidate by: prob / (|eccentricity_if_added - target| + eps)
    int best_cell = -1;
    double best_score = -1.0;

    for (int cand : eligible_set) {
      double prob = probs[cand - 1];
      if (ISNAN(prob) || prob < 0.0) prob = 0.0;

      // Compute eccentricity deviation if we were to add this cell
      allocated.push_back(cand);
      double ecc_if_added = patch_eccentricity(allocated, ncol);
      allocated.pop_back();

      double de = std::abs(ecc_if_added - eccentricity) + 1e-6;
      double score = prob / de;

      if (score > best_score) {
        best_score = score;
        best_cell = cand;
      }
    }

    if (best_cell < 0) break;

    eligible_set.erase(best_cell);
    allocated.push_back(best_cell);
    in_patch.insert(best_cell);
    landscape[best_cell - 1] = to_class;
    add_neighbors(best_cell);
  }

  return IntegerVector(allocated.begin(), allocated.end());
}
