# alloc_dinamica_spec() on a hand-built allocation context, without a database. Where
# Dinamica is not installed, the test fallback copies the anterior map, which still exercises
# writing the inputs and reading the posterior back; tinytest resets the option after this
# file
options(evoland.dinamica_fallback = !nzchar(Sys.which("DinamicaConsole")))

grid_coords <- data.table::CJ(lat = seq(5, 95, 10), lon = seq(5, 95, 10))[,
  .(id_coord = .I, lon, lat)
]
data.table::setattr(grid_coords, "epsg", 2056L)
data.table::setattr(grid_coords, "resolution", 10)
dinamica_ctx <- list(
  id_run = 1L,
  id_period_post = 2L,
  anterior = grid_coords[, .(id_coord, id_lulc = 1L)],
  viable_trans = data.table::data.table(
    id_trans = 1L,
    id_lulc_anterior = 1L,
    id_lulc_posterior = 2L
  ),
  quota = data.table::data.table(id_trans = 1L, rate = 0.2, n_anterior = 100L, quota = 20L),
  potentials = grid_coords[, .(id_trans = 1L, id_coord, value = 0.2)],
  alloc_params = data.table::data.table(
    id_trans = 1L,
    mean_patch_size = 2,
    patch_size_variance = 1,
    patch_isometry = 1.5,
    frac_expander = 0.5
  ),
  grid = evoland:::coords_grid(grid_coords)
)

# the inputs: tables in transition order, the anterior map and one probability map per
# transition, all on the grid
work_dir <- tempfile("dinamica_inputs_")
dir.create(work_dir)
evoland:::write_dinamica_inputs(dinamica_ctx, work_dir)
expect_equal(
  data.table::fread(file.path(work_dir, "trans_rates.csv")),
  data.table::data.table(`From*` = 1L, `To*` = 2L, Rate = 0.2)
)
expect_equal(
  data.table::fread(file.path(work_dir, "patcher_table.csv"))[["Mean_Patch_Size"]],
  2
)
expect_equal(
  evoland:::rast_to_grid(dinamica_ctx$grid, terra::rast(file.path(work_dir, "anterior.tif"))),
  grid_coords[, .(id_coord, value = 1)]
)
expect_equal(
  list.files(file.path(work_dir, "probability_map_dir")),
  "001_id_trans_1.tif"
)
unlink(work_dir, recursive = TRUE)

# a whole period: one row per anterior row, only classes 1 and 2, and the temporary working
# directory is removed
dirs_before <- list.files(tempdir(), pattern = "^dinamica_")
if (getOption("evoland.dinamica_fallback")) {
  expect_warning(
    dinamica_posterior <- alloc_dinamica_spec()$one_period(dinamica_ctx),
    "as fallback so we can test"
  )
} else {
  dinamica_posterior <- alloc_dinamica_spec()$one_period(dinamica_ctx)
}
expect_equal(dinamica_posterior$id_coord, 1:100)
expect_true(all(dinamica_posterior$id_lulc %in% 1:2))
expect_equal(list.files(tempdir(), pattern = "^dinamica_"), dirs_before)
