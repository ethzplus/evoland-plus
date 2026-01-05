#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <map>
#include <vector>

using namespace Rcpp;

// Helper for aggregation index max_g calculation
// see https://www.fragstats.org/index.php/fragstats-metrics/patch-based-metrics/aggregation-metrics/c3-aggregation-index
// a: area in number of cells
double calculate_max_g(long long a) {
  if (a == 0)
    return 0.0;
  long long n = std::trunc(std::sqrt((double)a));
  long long m = a - n * n;
  long long maxg = 0;

  if (m == 0)
    maxg = 2 * n * (n - 1);
  else if (m <= n)
    maxg = 2 * n * (n - 1) + 2 * m - 1;
  else
    maxg = 2 * n * (n - 1) + 2 * m - 2;

  return (double)maxg;
}

// Union-Find implementation for Connected Component Labeling
struct UnionFind {
  std::vector<int> parent;
  std::vector<int> size;

  UnionFind(int n) {
    parent.resize(n);
    size.resize(n, 1);
    for (int i = 0; i < n; ++i)
      parent[i] = i;
  }

  int find(int i) {
    if (parent[i] == i)
      return i;
    return parent[i] = find(parent[i]);
  }

  void unite(int i, int j) {
    int root_i = find(i);
    int root_j = find(j);
    if (root_i != root_j) {
      if (size[root_i] < size[root_j])
        std::swap(root_i, root_j);
      parent[root_j] = root_i;
      size[root_i] += size[root_j];
    }
  }
};

/**
 * @brief Calculate class statistics (patch metrics) for a raster matrix.
 *
 * This function computes patch statistics similar to SDMTools::ClassStat.
 * It identifies connected components (patches) using 8-connectivity and calculates:
 * - Mean patch area
 * - Standard deviation of patch area
 * - Aggregation index
 *
 * NA values in the input matrix are treated as background and ignored.
 *
 * @param mat IntegerMatrix representing the landscape/raster.
 * @param cellsize The side length of a cell (resolution).
 * @return DataFrame containing class-level statistics.
 */
// [[Rcpp::export]]
DataFrame calculate_class_stats_cpp(IntegerMatrix mat, double cellsize) {
  int nrow = mat.nrow();
  int ncol = mat.ncol();
  size_t n_total = (size_t)nrow * ncol;

  // 1. Connected Component Labeling using Union-Find
  // We map 2D coordinates (r, c) to 1D index: r + c * nrow
  // Note: R matrices are column-major.

  UnionFind uf(n_total);

  // We also need to track internal edges for aggregation index
  // Using map to handle arbitrary class IDs.
  std::map<int, double> class_internal_edges;
  std::map<int, int> class_total_area_cells; // Total cells per class

  // Iterate to build components and count edges
  for (int c = 0; c < ncol; ++c) {
    for (int r = 0; r < nrow; ++r) {
      int val = mat(r, c);

      if (val == NA_INTEGER)
        continue;

      int idx = r + c * nrow;

      // Update total area for class
      class_total_area_cells[val]++;

      // Check Down (r+1, c)
      if (r + 1 < nrow) {
        int val_down = mat(r + 1, c);
        if (val == val_down) {
          int idx_down = (r + 1) + c * nrow;
          uf.unite(idx, idx_down);
          class_internal_edges[val]++;
        }
      }

      // Check Right (r, c+1)
      if (c + 1 < ncol) {
        int val_right = mat(r, c + 1);
        if (val == val_right) {
          int idx_right = r + (c + 1) * nrow;
          uf.unite(idx, idx_right);
          class_internal_edges[val]++;
        }
      }

      // Check Down-Right (r+1, c+1) - Diagonal for 8-connectivity
      if (r + 1 < nrow && c + 1 < ncol) {
        int val_dr = mat(r + 1, c + 1);
        if (val == val_dr) {
          int idx_dr = (r + 1) + (c + 1) * nrow;
          uf.unite(idx, idx_dr);
        }
      }

      // Check Up-Right (r-1, c+1) - Diagonal for 8-connectivity
      if (r - 1 >= 0 && c + 1 < ncol) {
        int val_ur = mat(r - 1, c + 1);
        if (val == val_ur) {
          int idx_ur = (r - 1) + (c + 1) * nrow;
          uf.unite(idx, idx_ur);
        }
      }
    }
  }

  // 2. Gather Patch Statistics
  // Map: class_id -> vector of patch areas
  std::map<int, std::vector<double>> class_patch_areas;

  // We need to find the root of each cell to identify its patch.
  // To avoid iterating all cells again and finding roots, we can just iterate
  // the cells that are not background.
  // However, we only want to record each patch once.
  // The root of the component holds the correct size.

  // We track which roots we have already processed to avoid duplicates.
  // Since root indices can be up to n_total, we use a vector<bool> or similar.
  // Given n_total can be large, vector<bool> is space efficient.
  std::vector<bool> root_processed(n_total, false);

  for (int c = 0; c < ncol; ++c) {
    for (int r = 0; r < nrow; ++r) {
      int val = mat(r, c);
      if (val == NA_INTEGER)
        continue;

      int idx = r + c * nrow;
      int root = uf.find(idx);

      if (!root_processed[root]) {
        int patch_cells = uf.size[root];
        double area = patch_cells * cellsize * cellsize;
        class_patch_areas[val].push_back(area);
        root_processed[root] = true;
      }
    }
  }

  // 3. Compute Final Statistics per Class
  // Using maps ensures we iterate in increasing order of class ID
  std::vector<int> out_class;
  std::vector<double> out_mean_area;
  std::vector<double> out_sd_area;
  std::vector<double> out_agg_index;

  for (auto const &[cls, areas] : class_patch_areas) {
    out_class.push_back(cls);

    // Mean and SD
    double sum = 0;
    double sq_sum = 0;
    int n = areas.size();

    for (double a : areas) {
      sum += a;
      sq_sum += a * a;
    }

    double mean = sum / n;
    double variance = (n > 1) ? (sq_sum - (sum * sum) / n) / (n - 1) : NA_REAL;
    double sd = (n > 1) ? std::sqrt(variance) : NA_REAL;

    out_mean_area.push_back(mean);
    out_sd_area.push_back(sd);

    // Aggregation Index
    double g = class_internal_edges[cls];
    long long total_a = class_total_area_cells[cls];
    double max_g = calculate_max_g(total_a);

    double agg_idx = (max_g > 0) ? (g / max_g) * 100.0 : 0.0;
    out_agg_index.push_back(agg_idx);
  }

  return DataFrame::create(Named("class") = out_class,
                           Named("mean.patch.area") = out_mean_area,
                           Named("sd.patch.area") = out_sd_area,
                           Named("aggregation.index") = out_agg_index);
}
