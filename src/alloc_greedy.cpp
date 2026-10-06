#include <Rcpp.h>
using namespace Rcpp;

//' Greedy rank-and-fill walk
//'
//' Walks candidate (cell, transition) pairs in the given order and accepts a pair when its
//' cell has not been claimed yet and its transition still has quota left. The caller decides
//' the order (by potential across all transitions, or by transition priority, then potential),
//' so this kernel only enforces "one change per cell" and "no more than the demanded count".
//'
//' @param cell Integer vector, cell (or coordinate) index of each candidate, 1-based and
//'   dense (at most `n_cells`).
//' @param trans Integer vector, transition index of each candidate, 1-based into `quota`.
//' @param quota Integer vector, number of cells each transition may claim.
//' @param n_cells Integer, the largest cell index.
//' @return Logical vector, `TRUE` for the accepted candidates.
//' @keywords internal
// [[Rcpp::export]]
LogicalVector greedy_fill_cpp(IntegerVector cell, IntegerVector trans, IntegerVector quota,
                              int n_cells) {
  const R_xlen_t n = cell.size();
  if (trans.size() != n) stop("cell and trans must have the same length");
  std::vector<bool> claimed(static_cast<size_t>(n_cells) + 1, false);
  std::vector<int> remaining(quota.begin(), quota.end());
  LogicalVector accepted(n, false);
  int open_quota = 0;
  for (int q : remaining) open_quota += q > 0 ? q : 0;

  for (R_xlen_t i = 0; i < n && open_quota > 0; ++i) {
    const int c = cell[i];
    const int t = trans[i] - 1;
    if (c < 1 || c > n_cells) stop("cell index out of range");
    if (t < 0 || t >= static_cast<int>(remaining.size())) stop("transition index out of range");
    if (claimed[c] || remaining[t] <= 0) continue;
    claimed[c] = true;
    --remaining[t];
    --open_quota;
    accepted[i] = true;
  }
  return accepted;
}
