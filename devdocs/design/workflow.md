# Modelling workflow

The fundamental function of `evoland-plus` is to use a statistically calibrated, constrained model predicting locations for future land use / land cover change (LULCC), see the graph below.
Further features, such as one- or two-way coupling to other models, build on this core.

```mermaid
---
config:
  theme: neutral
---
flowchart LR
    setup["<b>i. Setup</b><br>LULC categorization<br>Spatial domain<br>Temporal domain"]
    data["<b>ii. Data</b><br>LULC observations<br>Predictors<br>Quantization"]
    calibration["<b>iii. Calibration</b><br>Feature selection<br>Transition potential models"]
    allocation["<b>iv. Allocation</b><br>Demand<br>Allocation parameters<br>Stochastic allocation"]
    projection@{shape: doc, label: "LULC map @ t+1"}

    setup --> data --> calibration --> allocation --> projection
    projection -->|"t+1 becomes t"| allocation

    setup_variants["usually fixed"]
    data_variants["data variants"]
    model_variants["model variants"]
    allocation_variants["transition rates<br>allocation algorithm<br>patch shapes<br>seeds"]
    setup -.- setup_variants
    data -.- data_variants
    calibration -.- model_variants
    allocation -.- allocation_variants

    classDef variant fill:#f7dce8,stroke:#e377c2,stroke-dasharray:3 3,color:#555
    class setup_variants,data_variants,model_variants,allocation_variants variant
```

Calibration happens once, on the observed periods.
Extrapolation is autoregressive: each allocated map is the starting point for the next period, so neighborhood predictors are recomputed from it before the transition potentials for the next period are predicted with the already calibrated models.

The pink boxes are what may vary between runs (`runs_t`).
A child run inherits everything it does not override from its parent, so an ensemble of allocations, data or model variants shares the work done upstream of where it diverges.

Every phase reads its inputs from and writes its results to the database described in [database.md](database.md), so that each step can be rerun, inspected or replaced by another tool independently.
The [getting-started vignette](../../vignettes/evoland.qmd) walks through the phases with the functions that implement them.
