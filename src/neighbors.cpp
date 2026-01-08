#include <Rcpp.h>
#include <unordered_map>
#include <vector>
#include <cmath>

using namespace Rcpp;

// Helper struct for grid keys
struct GridKey {
  long long x;
  long long y;

  bool operator==(const GridKey &other) const {
    return x == other.x && y == other.y;
  }
};

// Hasher for GridKey
struct GridKeyHash {
  std::size_t operator()(const GridKey &k) const {
    // Simple hash combine
    std::size_t h1 = std::hash<long long>{}(k.x);
    std::size_t h2 = std::hash<long long>{}(k.y);
    return h1 ^ (h2 + 0x9e3779b9 + (h1 << 6) + (h1 >> 2));
  }
};

/**
 * @brief Find neighboring coordinates within specified distance using spatial hashing
 *
 * This function identifies neighboring points for each coordinate within a specified maximum distance.
 * It uses a spatial hash map to index points into grid cells, allowing for efficient O(1) lookups
 * of potential neighbors. It handles exact coordinate distances and reciprocal relationships efficiently.
 *
 * @param coords_t DataFrame containing coordinate data with columns:
 *   - id_coord: Integer vector of unique coordinate identifiers
 *   - lon: Numeric vector of longitude values
 *   - lat: Numeric vector of latitude values
 * @param max_distance Maximum distance to search for neighbors (in same units as coordinates)
 * @param quiet If true, suppresses progress output.
 *
 * @return List (data.table compatible) with columns:
 *   - id_coord_origin: ID of the origin coordinate
 *   - id_coord_neighbor: ID of the neighboring coordinate
 *   - distance: Euclidean distance between points
 */
// [[Rcpp::export]]
List distance_neighbors_cpp(DataFrame coords_t, double max_distance,
                            bool quiet = false) {

  // Extract columns
  IntegerVector id_coord = coords_t["id_coord"];
  NumericVector lon = coords_t["lon"];
  NumericVector lat = coords_t["lat"];
  int n_points = id_coord.size();

  double resolution = max_distance;
  double max_dist_sq = max_distance * max_distance;

  // 1. Build Spatial Index (Hash Map)
  // Maps (grid_x, grid_y) -> vector of point indices (0 to n_points-1)
  std::unordered_map<GridKey, std::vector<int>, GridKeyHash> grid_map;
  
  // Reserve roughly based on n_points to avoid initial rehashes, 
  // though actual load factor depends on spatial distribution
  grid_map.reserve(n_points); 

  for (int i = 0; i < n_points; ++i) {
    long long gx = static_cast<long long>(std::floor(lon[i] / resolution));
    long long gy = static_cast<long long>(std::floor(lat[i] / resolution));
    grid_map[{gx, gy}].push_back(i);
  }

  // 2. Prepare for search
  std::vector<int> res_origin;
  std::vector<int> res_neighbor;
  std::vector<double> res_distance;
  
  // Heuristic reservation: average 8 neighbors per point? 
  // It's a guess, but helps reduce reallocations.
  size_t estimated_size = n_points * 8; 
  res_origin.reserve(estimated_size);
  res_neighbor.reserve(estimated_size);
  res_distance.reserve(estimated_size);

  int radius_cells = static_cast<int>(std::ceil(max_distance / resolution));

  // Progress setup
  int progress_interval = std::max(1000, n_points / 20);
  if (!quiet) {
    Rcpp::Rcout << "Indexing complete. Searching neighbors..." << std::endl;
  }

  // 3. Search for neighbors
  for (int i = 0; i < n_points; ++i) {
    // Progress check
    if (i % 1000 == 0) Rcpp::checkUserInterrupt();
    if (!quiet && i > 0 && i % progress_interval == 0) {
      Rcpp::Rcout << "\rProgress: " << int(100.0 * i / n_points) << "%" << std::flush;
    }

    double xi = lon[i];
    double yi = lat[i];
    
    long long gx = static_cast<long long>(std::floor(xi / resolution));
    long long gy = static_cast<long long>(std::floor(yi / resolution));

    // Check all cells within the radius
    for (int dx = -radius_cells; dx <= radius_cells; ++dx) {
      for (int dy = -radius_cells; dy <= radius_cells; ++dy) {
        
        GridKey key = {gx + dx, gy + dy};
        auto it = grid_map.find(key);
        
        if (it != grid_map.end()) {
          const std::vector<int> &candidates = it->second;
          
          for (int j : candidates) {
            // Optimization: Only calculate if j > i.
            // This avoids:
            // 1. Self-comparison (i == j)
            // 2. Double calculation (calculating A->B and B->A separately)
            if (j > i) {
              double xj = lon[j];
              double yj = lat[j];
              
              double dist_sq = (xi - xj) * (xi - xj) + (yi - yj) * (yi - yj);
              
              if (dist_sq <= max_dist_sq) {
                double d = std::sqrt(dist_sq);
                
                // Push A -> B
                res_origin.push_back(id_coord[i]);
                res_neighbor.push_back(id_coord[j]);
                res_distance.push_back(d);
                
                // Push B -> A
                res_origin.push_back(id_coord[j]);
                res_neighbor.push_back(id_coord[i]);
                res_distance.push_back(d);
              }
            }
          }
        }
      }
    }
  }

  if (!quiet) {
    Rcpp::Rcout << "\rProgress: 100%" << std::endl;
  }

  // 4. Construct result
  List res = List::create(
    Named("id_coord_origin") = res_origin,
    Named("id_coord_neighbor") = res_neighbor,
    Named("distance") = res_distance
  );

  res.attr("class") = CharacterVector::create("data.table", "data.frame");
  
  return res;
}