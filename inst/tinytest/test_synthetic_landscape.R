library(tinytest)

set.seed(1)
landscape <- synthetic_landscape(n_periods = 3L, n_cells = 20L)
lulc <- terra::values(landscape$lulc)
drivers <- terra::values(landscape$drivers)

expect_equal(dim(lulc), c(400L, 3L))
expect_equal(names(landscape$lulc), paste0("id_period=", 1:3))
expect_equal(names(landscape$drivers), c("accessibility", "site_quality", "random_nuisance"))
expect_true(all(lulc %in% 1:4))
expect_true(all(drivers >= 0 & drivers <= 1))

# the lake never changes, and every change is one of the four transitions of the process
expect_equal(lulc[, 1] == 4, lulc[, 3] == 4)
changes <- unique(paste(lulc[, 1:2], lulc[, 2:3])[lulc[, 1:2] != lulc[, 2:3]])
expect_true(length(changes) > 0L)
expect_true(all(changes %in% c("2 3", "1 3", "1 2", "2 1")))

# replays from a seed
set.seed(1)
expect_equal(terra::values(synthetic_landscape(n_periods = 3L, n_cells = 20L)$lulc), lulc)
