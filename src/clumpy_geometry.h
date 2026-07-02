#ifndef EVOLAND_CLUMPY_GEOMETRY_H
#define EVOLAND_CLUMPY_GEOMETRY_H

#include <cmath>

// Shared patch-shape geometry for the CLUMPY allocation backend.
//
// `elongation_from_raw_moments()` is the SINGLE definition of the patch shape
// metric used both during calibration (`calculate_class_stats_cpp` in
// patch_stats.cpp, which measures the elongation of observed patches) and
// during allocation (`grow_patch_cpp` / `allocate_clumpy_cpp` in
// alloc_clumpy.cpp, which grows patches towards a target elongation).  Keeping
// one definition guarantees the shape that allocation optimises is exactly the
// shape calibration reports -- previously these lived in two functions
// (`calculate_elongation` and `patch_eccentricity`) that computed the same
// quantity with different variable names and indexing conventions.
//
// Definition (Mazy 2022, https://theses.hal.science/tel-04382012, eq. 3.I.12):
//
//   e = 1 - sqrt(lambda_min / lambda_max)
//
// where lambda_min <= lambda_max are the eigenvalues of the 2x2 matrix of
// second central moments of the cell coordinates.  e = 0 is circular /
// isometric; e -> 1 is increasingly elongated (linear).
//
// The eigenvalues of the (symmetric) second-moment matrix are invariant under
// the choice of axes, so callers may pass row/col or x/y coordinates, and it
// does not matter whether the raster is stored row- or column-major.
//
// Arguments are raw (un-centred) moment accumulators over the cell set:
//   m00 = number of cells
//   s10 = sum of first-axis coordinates       (e.g. sum of row indices)
//   s01 = sum of second-axis coordinates      (e.g. sum of col indices)
//   s20 = sum of first-axis coordinates^2
//   s02 = sum of second-axis coordinates^2
//   s11 = sum of (first-axis * second-axis)
namespace clumpy {

inline double elongation_from_raw_moments(double m00, double s10, double s01,
                                          double s20, double s02, double s11) {
  if (m00 <= 1.0) {
    return 0.0; // single cell or empty: isometric by convention
  }

  // Second central moments about the centre of mass:
  //   mu_pq = (sum x^p y^q)/n - mean_x^p mean_y^q
  const double mean_1 = s10 / m00;
  const double mean_2 = s01 / m00;

  const double mu20 = s20 / m00 - mean_1 * mean_1;
  const double mu02 = s02 / m00 - mean_2 * mean_2;
  const double mu11 = s11 / m00 - mean_1 * mean_2;

  const double trace = mu20 + mu02;
  if (trace <= 0.0) {
    return 0.0;
  }

  const double delta =
      std::sqrt((mu20 - mu02) * (mu20 - mu02) + 4.0 * mu11 * mu11);

  const double lambda_max = 0.5 * (trace + delta);
  double lambda_min = 0.5 * (trace - delta);

  if (lambda_max <= 0.0) {
    return 0.0;
  }
  if (lambda_min < 0.0) {
    lambda_min = 0.0; // guard against floating-point round-off
  }

  return 1.0 - std::sqrt(lambda_min / lambda_max);
}

} // namespace clumpy

#endif // EVOLAND_CLUMPY_GEOMETRY_H
