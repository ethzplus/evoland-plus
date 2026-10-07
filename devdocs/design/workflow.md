# Modelling workflow

The fundamental function of `evoland-plus` is to use a statistically calibrated, constrained model predicting locations for future land use / land cover change (LULCC), see the graph below.
Further features, such as one- or two-way coupling to other models, build on this core.

```mermaid
---
config:
  theme: neutral
---

flowchart TD
    ts@{shape : brace, label: " Timesteps: \n t = now \n t-1 = one step in the past \n t+1 = one step in the future"}
    style ts stroke:#ccc, stroke-width:2px

    subgraph preparation["1\. Data Preparation"]
        direction LR
        config@{shape : doc, label: "Parameters"}
        predictors@{shape : docs, label: "Predictors"}
        lulc_data@{shape : doc, label: "LULC data \n @ {t, t-1, t-2, ...}"}
        config --- predictors
        predictors --- lulc_data
        linkStyle 0,1 stroke-opacity:0
    end
    preparation --> calibration

    subgraph calibration["2\. Calibration Phase"]
        direction LR
        varsel["Select Features"]
        varsel --> markovmods
        markovmods["Train Markovian Models"]
        markovmods --> parametrize_alloc
        parametrize_alloc["Parameterize Allocation Strategy"]
    end

    calibration --> estimation

    subgraph estimation["3\. Prediction + Allocation"]
        direction LR
        evalmodel["`Evaluate Models @ t`"]
        Predictions["Transition Potential Maps @ t+1"]
        evalmodel --> Predictions
        Changes["Allocate projected LULCC <br> (patch / expand)"]
        Predictions --> Changes
    end

    estimation --> projection
    projection@{shape : terminal, label: "LULC projection\n @ t+1"}
    projection -->|Next iteration with t+1 as t| estimation

    classDef phase fill:#f9f9f990,stroke:#333,stroke-dasharray:2 4,color:#000000
    classDef data fill:#ddf1d5,stroke:#82b366,color:#000000
    classDef user_input fill:#fff2cc,stroke:#d6b655,color:#000000

    class calibration,preparation,estimation,allocation phase
    class predictors,lulc_data,projection data
    class config user_input
```

Every phase reads its inputs from and writes its results to the database described in [database.md](database.md), so that each step can be rerun, inspected or replaced by another tool independently.
The [getting-started vignette](../../vignettes/evoland.qmd) walks through the phases with the functions that implement them.
