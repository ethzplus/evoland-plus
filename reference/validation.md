# Validation of simulated land use change

Compare a simulated map against the observed one, both starting from the
same initial map:

- [`lulc_crosstab_v()`](https://ethzplus.github.io/evoland-plus/reference/lulc_crosstab_v.md)
  (`evoland_db$lulc_crosstab_v()`): cross-tabulation of land use between
  two periods, of one run or of two;

- [`figure_of_merit_v()`](https://ethzplus.github.io/evoland-plus/reference/figure_of_merit_v.md)
  (`evoland_db$figure_of_merit_v()`): Pontius' figure of merit and its
  components, cell by cell, with the value expected from random
  allocation for comparison;

- [`calc_transition_similarity()`](https://ethzplus.github.io/evoland-plus/reference/calc_transition_similarity.md):
  fuzzy similarity of differences for one transition, which tolerates
  change placed near, rather than exactly on, the observed change.
