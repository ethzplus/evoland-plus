#include <Rcpp.h>
#include <unordered_map>
#include <vector>
#include <cmath>

using namespace Rcpp;

/**
 * @brief Find neighboring coordinates within specified distance using raster-based spatial indexing
 *
 * This function identifies neighboring points for each coordinate within a specified maximum distance
 * using a rasterization and convolution approach. It divides the spatial extent into a grid,
 * places coordinates into cells, and uses distance kernels to efficiently find neighbors.
 *
 * @param coords_t DataFrame containing coordinate data with columns:
 *   - id_coord: Integer vector of unique coordinate identifiers
 *   - lon: Numeric vector of longitude values
 *   - lat: Numeric vector of latitude values
 * @param max_distance Maximum distance to search for neighbors (in same units as coordinates)
 * @param resolution Grid cell size for rasterization (default: 100.0, in same units as coordinates)
 *
 * @return List (data.table) with columns:
 *   - id_coord_origin: ID of the origin coordinate
 *   - id_coord_neighbor: ID of the neighboring coordinate
 *   - distance_approx: Approximate distance between origin and neighbor
 *
 * @note The function uses an approximate distance calculation based on grid cells.
 *   If multiple points fall into the same cell, a warning is issued and only the first
 *   point is retained for that cell.
 *
 * @warning Distance calculations are approximate and based on Euclidean distance in
 *   coordinate space. For geographic coordinates, this may not reflect true geodesic distance.
 */
// [[Rcpp::export]]
List distance_neighbors_cpp(DataFrame coords_t,
                                 double max_distance,
                                 double resolution = 100.0) {

  // Extract columns
  IntegerVector id_coord = coords_t["id_coord"];
  NumericVector lon = coords_t["lon"];
  NumericVector lat = coords_t["lat"];
  int n_points = id_coord.size();

  // 1. Create distance kernel
  int radius_cells = (int)std::ceil(max_distance / resolution);
  int kernel_size = 2 * radius_cells + 1;

  // 2. Count cells within circular search area for pre-allocation
  int cells_in_circle = 0;
  for (int i = 0; i < kernel_size; i++) {
    for (int j = 0; j < kernel_size; j++) {
      double x = (j - radius_cells) * resolution;
      double y = (i - radius_cells) * resolution;
      double dist = std::sqrt(x * x + y * y);
      if (dist <= max_distance) {
        cells_in_circle++;
      }
    }
  }

  // 3. Rasterize coordinates
  double lon_min = min(lon) - max_distance;
  double lon_max = max(lon) + max_distance;
  double lat_min = min(lat) - max_distance;
  double lat_max = max(lat) + max_distance;

  int n_cols = (int)std::ceil((lon_max - lon_min) / resolution);
  int n_rows = (int)std::ceil((lat_max - lat_min) / resolution);

  std::vector<std::vector<int>> raster_ids(n_rows, std::vector<int>(n_cols, NA_INTEGER));
  std::vector<int> row_indices(n_points);
  std::vector<int> col_indices(n_points);

  int duplicate_count = 0;

  for (int i = 0; i < n_points; i++) {
    int col_idx = std::min((int)std::ceil((lon[i] - lon_min) / resolution), n_cols) - 1;
    int row_idx = std::min((int)std::ceil((lat[i] - lat_min) / resolution), n_rows) - 1;

    row_indices[i] = row_idx;
    col_indices[i] = col_idx;

    if (raster_ids[row_idx][col_idx] == NA_INTEGER) {
      raster_ids[row_idx][col_idx] = id_coord[i];
    } else {
      duplicate_count++;
    }
  }

  if (duplicate_count > 0) {
    double pct = 100.0 * duplicate_count / n_points;
    Rcpp::warning("%.0f points fell into cells already occupied (%.1f%% of total).", 
                  duplicate_count, pct);
  }

  // 4. Pre-allocate result vectors based on density estimate
  // Calculate density: occupied cells / total cells
  int occupied_cells = n_points - duplicate_count;
  double density = (double)occupied_cells / (n_rows * n_cols);
  
  // Estimate total neighbors: points × circular area × density × safety margin
  // Safety margin of 1.3 accounts for spatial clustering and edge effects
  size_t estimated_total = (size_t)(n_points * cells_in_circle * density * 1.3);
  
  std::vector<int> origin_ids, neighbor_ids;
  std::vector<double> distances_approx;
  
  origin_ids.reserve(estimated_total);
  neighbor_ids.reserve(estimated_total);
  distances_approx.reserve(estimated_total);
  
  // 5. Find neighbors within max_distance

  for (int pt_idx = 0; pt_idx < n_points; pt_idx++) {
    int origin_id = id_coord[pt_idx];
    int origin_row = row_indices[pt_idx];
    int origin_col = col_indices[pt_idx];

    int row_start = std::max(0, origin_row - radius_cells);
    int row_end = std::min(n_rows - 1, origin_row + radius_cells);
    int col_start = std::max(0, origin_col - radius_cells);
    int col_end = std::min(n_cols - 1, origin_col + radius_cells);

    std::unordered_map<int, double> found_neighbors;

    for (int r = row_start; r <= row_end; r++) {
      for (int c = col_start; c <= col_end; c++) {
        if (raster_ids[r][c] != NA_INTEGER) {
          int neighbor_id = raster_ids[r][c];
          
          // Skip self
          if (neighbor_id == origin_id) {
            continue;
          }

          // Calculate approximate distance
          double row_offset = (r - origin_row) * resolution;
          double col_offset = (c - origin_col) * resolution;
          double dist = std::sqrt(row_offset * row_offset + col_offset * col_offset);

          // Only include if within max_distance
          if (dist <= max_distance) {
            found_neighbors[neighbor_id] = dist;
          }
        }
      }
    }

    // Add to results
    for (const auto& pair : found_neighbors) {
      origin_ids.push_back(origin_id);
      neighbor_ids.push_back(pair.first);
      distances_approx.push_back(pair.second);
    }
  }

  List res = List::create(
    Named("id_coord_origin") = origin_ids,
    Named("id_coord_neighbor") = neighbor_ids,
    Named("distance_approx") = distances_approx
  );

  res.attr("class") = CharacterVector::create("data.table", "data.frame");

  return res;
}
