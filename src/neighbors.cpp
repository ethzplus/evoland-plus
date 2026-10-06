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

// Distance class of d under cut(breaks, right = FALSE, include.lowest = TRUE): intervals
// [b_k, b_k+1), the last one closed, 1-based; NA_INTEGER outside the breaks.
static int distance_class_code(double d, const std::vector<double> &breaks) {
  const size_t n = breaks.size();
  if (n < 2 || d < breaks[0] || d > breaks[n - 1]) return NA_INTEGER;
  for (size_t k = 0; k + 1 < n; ++k) {
    if (d < breaks[k + 1]) return static_cast<int>(k) + 1;
  }
  return static_cast<int>(n) - 1; // d == last break, closed by include.lowest
}

/**
 * @brief Neighbours within a distance, handed out in chunks of complete neighbourhoods
 *
 * Points are indexed in a spatial hash map with cells of size max_distance, so the
 * candidates of a point are in its own and the eight adjacent cells. Instead of returning
 * the whole edge list, it calls `callback` with a chunk (a data.table compatible List) as
 * soon as at least `chunk_rows` pairs have accumulated, and only after an origin's
 * neighbourhood is complete. To get the whole table in memory, pass an accumulating callback.
 * Every chunk therefore holds all neighbours of its origins, in both directions, and chunks
 * are disjoint in id_coord_origin. Memory is bounded by the chunk, not by the table.
 *
 * @param coords_t DataFrame with id_coord, lon, lat
 * @param max_distance Maximum distance (coordinate units)
 * @param breaks Distance class breaks; with at least two, chunks get an integer
 *   distance_class column (codes of cut(right = FALSE, include.lowest = TRUE)).
 * @param chunk_rows Pairs per chunk (at least; a chunk ends with a complete neighbourhood)
 * @param callback R function called with each chunk
 * @param quiet Suppress progress output
 * @return Total number of pairs, as a double: the count can exceed R's 32-bit integers
 */
// [[Rcpp::export]]
double distance_neighbors_chunked_cpp(DataFrame coords_t, double max_distance,
                                      NumericVector breaks, double chunk_rows,
                                      Function callback, bool quiet = false) {
  IntegerVector id_coord = coords_t["id_coord"];
  NumericVector lon = coords_t["lon"];
  NumericVector lat = coords_t["lat"];
  const int n_points = id_coord.size();
  const double max_dist_sq = max_distance * max_distance;
  const std::vector<double> brks(breaks.begin(), breaks.end());
  const bool with_class = brks.size() >= 2;
  const size_t chunk_size = static_cast<size_t>(std::max(1.0, chunk_rows));

  std::unordered_map<GridKey, std::vector<int>, GridKeyHash> grid_map;
  grid_map.reserve(n_points);
  for (int i = 0; i < n_points; ++i) {
    long long gx = static_cast<long long>(std::floor(lon[i] / max_distance));
    long long gy = static_cast<long long>(std::floor(lat[i] / max_distance));
    grid_map[{gx, gy}].push_back(i);
  }

  std::vector<int> res_origin, res_neighbor, res_class;
  std::vector<double> res_distance;
  res_origin.reserve(chunk_size);
  res_neighbor.reserve(chunk_size);
  res_distance.reserve(chunk_size);
  if (with_class) res_class.reserve(chunk_size);
  double total = 0;

  auto flush = [&]() {
    if (res_origin.empty()) return;
    List chunk = List::create(
      Named("id_coord_origin") = IntegerVector(res_origin.begin(), res_origin.end()),
      Named("id_coord_neighbor") = IntegerVector(res_neighbor.begin(), res_neighbor.end()),
      Named("distance") = NumericVector(res_distance.begin(), res_distance.end())
    );
    if (with_class) {
      chunk["distance_class"] = IntegerVector(res_class.begin(), res_class.end());
    }
    chunk.attr("class") = CharacterVector::create("data.table", "data.frame");
    total += static_cast<double>(res_origin.size());
    res_origin.clear();
    res_neighbor.clear();
    res_distance.clear();
    res_class.clear();
    callback(chunk);
  };

  const int progress_interval = std::max(1000, n_points / 20);
  for (int i = 0; i < n_points; ++i) {
    if (i % 1000 == 0) Rcpp::checkUserInterrupt();
    if (!quiet && i > 0 && i % progress_interval == 0) {
      Rcpp::Rcout << "\rProgress: " << int(100.0 * i / n_points) << "%" << std::flush;
    }
    const double xi = lon[i];
    const double yi = lat[i];
    const long long gx = static_cast<long long>(std::floor(xi / max_distance));
    const long long gy = static_cast<long long>(std::floor(yi / max_distance));
    for (int dx = -1; dx <= 1; ++dx) {
      for (int dy = -1; dy <= 1; ++dy) {
        auto it = grid_map.find({gx + dx, gy + dy});
        if (it == grid_map.end()) continue;
        for (int j : it->second) {
          if (j == i) continue;
          const double dist_sq = (xi - lon[j]) * (xi - lon[j]) + (yi - lat[j]) * (yi - lat[j]);
          if (dist_sq > max_dist_sq) continue;
          const double d = std::sqrt(dist_sq);
          res_origin.push_back(id_coord[i]);
          res_neighbor.push_back(id_coord[j]);
          res_distance.push_back(d);
          if (with_class) res_class.push_back(distance_class_code(d, brks));
        }
      }
    }
    // the neighbourhood of origin i is complete: a chunk may end here
    if (res_origin.size() >= chunk_size) flush();
  }
  flush();

  if (!quiet) Rcpp::Rcout << "\rProgress: 100%" << std::endl;
  return total;
}
