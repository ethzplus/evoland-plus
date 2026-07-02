#include "clumpy_geometry.h"
#include <Rcpp.h>
#include <algorithm>
#include <map>
#include <vector>

using namespace Rcpp;

// Union-Find implementation for Connected Component Labeling
// Augmented to track moments for elongation calculation
struct UnionFind {
  std::vector<int> parent;
  std::vector<int> size;

  // Moment accumulators
  // x corresponds to row index, y corresponds to col index
  std::vector<double> sum_x;
  std::vector<double> sum_y;
  std::vector<double> sum_sq_x;
  std::vector<double> sum_sq_y;
  std::vector<double> sum_xy;

  int nrow;

  UnionFind(int n, int nrow_in) : nrow(nrow_in) {
    parent.resize(n);
    size.resize(n, 1);

    sum_x.resize(n);
    sum_y.resize(n);
    sum_sq_x.resize(n);
    sum_sq_y.resize(n);
    sum_xy.resize(n);

    // Initialize stats for each pixel based on its position
    for (int i = 0; i < n; ++i) {
      parent[i] = i;

      // Calculate coordinates from index
      // Column-major order: idx = r + c * nrow
      double r = (double)(i % nrow);
      double c = (double)(i / nrow);

      sum_x[i] = r;
      sum_y[i] = c;
      sum_sq_x[i] = r * r;
      sum_sq_y[i] = c * c;
      sum_xy[i] = r * c;
    }
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

      // Merge j into i
      parent[root_j] = root_i;
      size[root_i] += size[root_j];

      sum_x[root_i] += sum_x[root_j]; // eq. 3.I.4
      sum_y[root_i] += sum_y[root_j]; // eq. 3.I.5
      sum_sq_x[root_i] += sum_sq_x[root_j]; //
      sum_sq_y[root_i] += sum_sq_y[root_j];
      sum_xy[root_i] += sum_xy[root_j];
    }
  }
};

/**
 * @brief Calculate patch elongation from moments of inertia.
 *
 * Based on François-Rémi Mazy (2022)
 * Towards a coherent algorithmic theory and implementation of pattern-based
 * land use and land cover change modeling
 * https://theses.hal.science/tel-04382012, eq 3.I.12.
 * Calculates elongation e = 1 - (lambda1 / lambda2)^(1/2)
 * where lambda1 and lambda2 are eigenvalues of the normalized inertia tensor.
 *
 * @param m00 Number of pixels (0th moment)
 * @param m10 Sum of x coordinates (1st moment x)
 * @param m01 Sum of y coordinates (1st moment y)
 * @param s20 Sum of x^2
 * @param s02 Sum of y^2
 * @param s11 Sum of x*y
 * @return double Elongation index in [0, 1] (0 = circular, 1 = linear)
 */
double calculate_elongation(double m00, double m10, double m01, double s20,
                            double s02, double s11) {
  // Single shared definition lives in clumpy_geometry.h so that the shape
  // metric measured here during calibration is identical to the one the patch
  // grower (alloc_clumpy.cpp) targets during allocation.
  return clumpy::elongation_from_raw_moments(m00, m10, m01, s20, s02, s11);
}

/**
 * @brief Calculate class statistics (patch metrics) for a raster matrix.
 *
 * This function identifies connected components (patches) using 8-connectivity
 * and calculates:
 * - Mean patch area
 * - Standard deviation of patch area
 * - Mean patch elongation (based on moments of inertia)
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

  // 1. Connected Component Labeling
  UnionFind uf(n_total, nrow);

  // Iterate to build components
  for (int c = 0; c < ncol; ++c) {
    for (int r = 0; r < nrow; ++r) {
      int val = mat(r, c);

      if (val == NA_INTEGER)
        continue;

      int idx = r + c * nrow;

      // Check Down (r+1, c)
      if (r + 1 < nrow) {
        int val_down = mat(r + 1, c);
        if (val == val_down) {
          int idx_down = (r + 1) + c * nrow;
          uf.unite(idx, idx_down);
        }
      }

      // Check Right (r, c+1)
      if (c + 1 < ncol) {
        int val_right = mat(r, c + 1);
        if (val == val_right) {
          int idx_right = r + (c + 1) * nrow;
          uf.unite(idx, idx_right);
        }
      }

      // Check Down-Right (r+1, c+1) - Diagonal
      if (r + 1 < nrow && c + 1 < ncol) {
        int val_dr = mat(r + 1, c + 1);
        if (val == val_dr) {
          int idx_dr = (r + 1) + (c + 1) * nrow;
          uf.unite(idx, idx_dr);
        }
      }

      // Check Up-Right (r-1, c+1) - Diagonal
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
  std::map<int, std::vector<double>> class_patch_areas;
  std::map<int, std::vector<double>> class_patch_elongations;

  std::vector<bool> root_processed(n_total, false);

  for (int c = 0; c < ncol; ++c) {
    for (int r = 0; r < nrow; ++r) {
      int val = mat(r, c);
      if (val == NA_INTEGER)
        continue;

      int idx = r + c * nrow;
      int root = uf.find(idx);

      if (!root_processed[root]) {
        // Area
        double n_pixels = (double)uf.size[root];
        double area = n_pixels * cellsize * cellsize;
        class_patch_areas[val].push_back(area);

        // Elongation
        double elongation = calculate_elongation(
            /*m00=*/n_pixels,
            /*m10=*/uf.sum_x[root],
            /*m01=*/uf.sum_y[root],
            /*s20=*/uf.sum_sq_x[root],
            /*s02=*/uf.sum_sq_y[root],
            /*s11=*/uf.sum_xy[root]
        );
        class_patch_elongations[val].push_back(elongation);

        root_processed[root] = true;
      }
    }
  }

  // 3. Compute Final Statistics per Class
  std::vector<int> out_class;
  std::vector<int> out_count;
  std::vector<double> out_mean_area;
  std::vector<double> out_variance_area;
  std::vector<double> out_mean_elongation;

  // Using maps ensures we iterate in increasing order of class ID
  for (auto const &[cls, areas] : class_patch_areas) {
    out_class.push_back(cls);

    // Mean and SD Area
    double sum_area = 0;
    double sq_sum_area = 0;
    int n = areas.size();

    for (double a : areas) {
      sum_area += a;
      sq_sum_area += a * a;
    }

    double mean_area = sum_area / n;
    double variance_area =
        (n > 1) ? (sq_sum_area - (sum_area * sum_area) / n) / (n - 1) : NA_REAL;

    out_mean_area.push_back(mean_area);
    out_variance_area.push_back(variance_area);

    // Mean Elongation
    const std::vector<double> &elongs = class_patch_elongations[cls];
    double sum_elong = 0;
    for (double e : elongs) {
      sum_elong += e;
    }
    double mean_elong = sum_elong / n;
    out_mean_elongation.push_back(mean_elong);
    out_count.push_back((int)areas.size());
  }

  return DataFrame::create(
      Named("class") = out_class, Named("patch_count") = out_count,
      Named("patch_area_mean") = out_mean_area,
      Named("patch_area_variance") = out_variance_area,
      Named("patch_elongation_mean") = out_mean_elongation);
}
