# Verification: CLUMPY pivot mechanism in evoland-plus

Investigation date: 2026-06-26
Scope: verify the **pivot-cell selection mechanism** of the evoland-plus CLUMPY
allocator against Mazy's thesis (`mazy-thesis.pdf`) and the reference Python
`clumpy` implementation. Plus: can allocations be made deterministic by setting
transition potentials to extreme values (0/1)?

Artifacts examined:
- Thesis: Appendix 3.B (MuST), §3.4.1 (uSAM), §3.4.2 (uPAM), Appendix 3.D.1
  (multi-pixel MuST-like variant), Appendix 3.E.2 (recoded Dinamica), Fig. 3.2.
- Reference Python: `clumpy/clumpy/allocation/_gart.py`,
  `_allocator.py::_sample_pivot`, `_unbiased.py::Unbiased.allocate`.
- evoland-plus branch `copilot/discuss-transition-probability-estimation`:
  `R/alloc_clumpy.R` (`gart`, `alloc_clumpy_one_period`),
  `src/alloc_clumpy.cpp` (`grow_patch_cpp`),
  `R/evoland_db_views.R` (`adjusted_trans_pot_v`).

---

## 1. The core pivot test (GART / MuST) — CORRECT

Thesis MuST (Appendix 3.B.1): with η_w = cumulative sum of P(v|u,z) over ordered
states, draw ξ ∈ [0,1) and pick the state w with η_{w-1} ≤ ξ < η_w. Standard
inverse-CDF sampling.

- Python `generalized_allocation_rejection_test`: clamps NaN→0 and negatives→0,
  `cs = cumsum(P, axis=1)`, draws one uniform per cell, iterates classes in
  **reverse** writing `y[x < cs[:,inv]] = list_v[inv]`. Net effect = pick the
  smallest-index class whose cumsum exceeds x. ✔ inverse-CDF.
- R `gart()`: `cs = rowwise cumsum`, `u = runif(n)`, iterates `j = k..1` writing
  `y[u < cs[,j]] = states[j]`; last write wins → smallest j with `u < cs[,j]`.
  **Semantically identical to the Python**, reverse-iteration tie-handling
  preserved. ✔

The literal translation of the pivot test is faithful.

### Minor robustness gaps in the R port
- `gart()` omits the `P[P<0] = 0` / `nan_to_num` clamps that Python applies
  inside the function. NA is handled when `P_change` is built and the stay column
  is `pmax(0, 1 - rowSums)`, but **negative entries in `P_change` are never
  clamped**. A negative potential would make the cumsum non-monotonic and break
  the inverse-CDF logic. `adjusted_trans_pot_v` is documented to close rows to
  [0,1] so it is probably safe in practice; still recommend clamping inside
  `gart()` to match the reference exactly.

---

## 2. The pivot-selection STRATEGY around GART — DEVIATES from the bias-free uPAM

The thesis distinguishes two multi-pixel strategies:

- **Simplified / MuST-like (Appendix 3.D.1):** run MuST over ALL pixels at once
  → every change-pixel is a pivot → grow a patch around each; pixels are left in
  the pool during the pass; NO probability updating between pivots. Acknowledged
  as a simplification (merging only resolved at the end).
- **Canonical bias-free uPAM (§3.4.2, Fig. 3.2):** iterative. Run MuST over the
  pool, draw ONE pivot at random, **dismiss all other potential pivots**, grow
  its patch, then UPDATE: `P(v|u) -= σ/#J`, remove the patch's pixels from J,
  re-update ρ(z|u); repeat until `P(v|u) < 0 ∀v` or J empty. The per-patch
  probability updating + sampling-without-replacement is what makes it bias-free.

Reference `Unbiased.allocate` implements the uPAM loop (`while keep_allocate`:
`_sample_pivot` → grow patches → `idx = ~isin(J, J_used)` removes used pixels →
`P_v[id] -= s` → re-estimate P_Y when threshold reached). With the default
`threshold_update_P_Y = 0` it breaks after the first successful pivot each
iteration ⇒ effectively one-patch-at-a-time = canonical uPAM.

evoland `alloc_clumpy_one_period` implements the **simplified single-pass
variant**: one GART pass per anterior class over all `from_cells`; every
change-pixel becomes a pivot; patches grown for all pivots in one sweep; NO
`P_v`/potential updating between patches, NO GART re-run, NO `keep_allocate`
retry, NO merge-failure rollback. Sampling-without-replacement is enforced only
spatially (patches grow into `from_class` cells in `post_vec`, mutated in place;
the pivot loop skips cells already converted).

This matches what `PROGRESS.md` describes (the R/C++ port reproduces the
simplified `scripts/run_allocation.py`, which bypasses `Unbiased.allocate`). So
it is a faithful port of the *simplified* pipeline, but it is **not** the thesis
bias-free uPAM.

---

## 3. MOST MATERIAL ISSUE: missing 1/E(σ) pivot rarefaction

Thesis Fig. 3.2 forms the pivot probability as `P*(v|u) × ρ(z|u,v)/ρ(z|u) ×
1/E(σ)` before MuST. Reference does exactly this:
`P_v_patches = P_v.copy(); P_v_patches /= patchers.area_mean(...)`. Rationale
(Eq. 3.11): expected change = E(#pivots)·E(σ); to hit a target rate P(v|u) the
pivots must be rarer by the mean patch area.

`adjusted_trans_pot_v` scales potentials so their **per-cell mean equals the
target `rate`** (`value * rate / mean_value`) and closes rows to [0,1]. There is
**no division by mean patch area**. Therefore expected pivots = `rate · #J`, and
each pivot grows to mean size E(σ), so expected allocated pixels ≈
`rate · #J · E(σ)` — **over-allocation by a factor ≈ mean patch size.**

Action item: either divide the GART input potentials by `area_mean` per
transition (matching `P_v_patches /= area_mean` and Fig. 3.2), or confirm that
`trans_rates_t.rate` is already defined as a *pivot* rate rather than a total
quantity-of-change rate. The same `adjusted_trans_pot_v` feeds `alloc_dinamica`,
so the intended semantics of `rate` must be pinned down. **Open question for the
author.**

Also absent vs uPAM: a quantity-of-change quota (`P(v|u)` decremented as patches
land). Every GART-selected cell becomes a seed regardless of how much has already
been allocated.

---

## 4. Determinism — can potentials of 0/1 bypass the stochastic parts?

Stochastic elements in `alloc_clumpy_one_period`:
1. `gart()`'s `u <- runif(n)` — the only randomness in pivot SELECTION.
2. `pivot_cells <- sample(pivot_cells)` — random pivot processing ORDER.
3. `sample_lognorm_area()`'s `rlnorm` — random patch SIZE.
4. `grow_patch_cpp` — **deterministic** (greedy argmax, no RNG; the Python
   reference's random hollow-fill branch was NOT ported).

GART at the extremes:
- A cell whose adjusted potential for a single target = 1 (others 0): its cumsum
  row is `[0,...,1(at target),1,...]`; the smallest j with `u < cs[,j]` is the
  target for every `u ∈ [0,1)`. ⇒ that transition is selected **deterministically**,
  the rejection draw is bypassed. ✔
- All potentials 0 ⇒ stay deterministically.

So **yes**: forcing the (adjusted) potential entering GART to 1 deterministically
forces the pivot transition. Caveats:
- It must be the **adjusted/closed** potential fed to `gart()` that is 1. Raw
  potentials pass through `value * rate / mean_value` then per-cell closure; if a
  cell has other nonzero transitions, the closure step (`/ sum` when `sum > 1`)
  pulls it back below 1. To force, that cell needs only that one transition
  nonzero (or bypass the rescaling).
- Pivot selection is then deterministic, but patch SIZE (`rlnorm`) and pivot
  ORDER (`sample`) remain stochastic. For a fully deterministic single-cell
  transition also pin the area to 1 (note: `sample_lognorm_area` maps
  `area_var <= 0 || NA` to V=1, NOT zero variance; `area_mean <= 0` returns 1L —
  that is the only built-in path to a guaranteed area of 1).
- `set.seed(seed)` already makes a whole run bit-reproducible regardless.

Net: deterministic *selection* is achievable via 0/1 potentials; deterministic
*allocation* additionally requires pinning patch area (and, if patches can
contest cells, pivot order).

---

## Summary

| Aspect | Verdict |
|---|---|
| GART/MuST inverse-CDF translation (R ↔ Python ↔ thesis) | ✔ correct |
| Negative-potential clamp inside `gart()` | ⚠ missing (latent) |
| Pivot strategy vs canonical bias-free uPAM | ⚠ simplified single-pass; no per-patch P(v|u) update, no merge rollback, doesn't dismiss all-but-one pivot |
| 1/E(σ) pivot rarefaction before GART | ✖ missing ⇒ ~E(σ)× over-allocation (confirm `rate` semantics) |
| Determinism via 0/1 potentials | ✔ bypasses GART draw; area & pivot-order still stochastic |

---

## Implementation status (addendum)

The allocation backend was subsequently reworked in C++:

- **Whole routine in Rcpp.** `allocate_clumpy_cpp()` (src/alloc_clumpy.cpp) now
  runs neighbour precompute, GART, log-normal area draws and the pivot/patch
  loop in one C++ call; the former R helpers (`gart`, `sample_lognorm_area`,
  `raster_neighbors`) are replaced by `gart_cpp`, `sample_lognorm_area_cpp`,
  `raster_neighbors_cpp`. R only prepares the numeric inputs.
- **Two selectable methods.** `method = "usam"` (single-pass) and
  `method = "upam"` (iterative GART with a per-transition pixel quota and
  sampling without replacement). `batch_size` is the speed/fidelity dial
  (1 = strict one-pivot-per-draw uPAM). uPAM is affordable because evoland's
  fixed-model potentials are pool-independent, so rho(z|u) is never re-estimated.
- **1/E(sigma) rarefaction** applied to GART input (`rarefy = TRUE`); `rate`
  confirmed to be a quantity-of-change rate via `get_obs_trans_rates`.
- **Negative/NaN clamp** added inside GART.
- **Shape metric unified** into `clumpy::elongation_from_raw_moments`
  (src/clumpy_geometry.h), used by both the patch grower and
  `calculate_class_stats_cpp` (replacing the duplicate `patch_eccentricity` /
  `calculate_elongation`).
