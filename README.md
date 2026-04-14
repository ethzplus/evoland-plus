# evoland-plus

**evoland** **v**isualizes, **o**ptimizes, **l**ocalizes and **a**nd **d**etermines **p**redictable **l**and **u**se **s**hifts

## Overview

The `evoland-plus` R package (or `evoland` for short) provides tools for analyzing and projecting land use evolution. The package implements a statistically calibrated, constrained model for predicting locations of future land use / land cover change (LULCC).
The fundamental purpose of `evoland-plus` is to:

- **Gather and process** land use/land cover data from multiple sources into a clearly defined database structure.
- **Calibrate statistical models** to understand historical land use transitions
- **Project future land use patterns** using predictive models and allocation algorithms
- **Support scenario analysis** through interventions and parameter modifications

```mermaid
flowchart TD
    subgraph setup[1. Setup]
        direction LR
        domain_categories{{ fa:fa-table-list LULC Categorization}}
        domain_space{{ fa:fa-globe Spatial Domain }}
        domain_time{{ fa:fa-clock Temporal Domain}}
        domain_categories --- domain_space
        domain_space --- domain_time
    end
    setup --> preparation

    subgraph preparation["2. Ingesting Data"]
        direction LR
        predictors@{shape : docs, label: "Spatiotemporal Predictors"}
        lulc_data@{shape : doc, label: " LULC data \n @ {t, t-1, t-2, ...}"}
        ts@{shape : brace, label: " Timesteps: \n t = now \n t-1 = one step in the past \n t+1 = one step in the future"}
        style ts stroke:#ccc, stroke-width:2px
        predictors --- lulc_data
        lulc_data --- ts
    end
    preparation --> calibration

    subgraph calibration["3. Calibration"]
        direction LR
        varsel[Select Features]
        varsel --> markovmods
        markovmods["Train Transition<br> Potential Models"]
        markovmods --> parametrize_alloc
        parametrize_alloc["Estimate Allocation Parameters"]
    end

    calibration --> estimation

    subgraph estimation["4. Prediction + Allocation"]
        direction LR
        evalmodel["`Evaluate Transition Poten Models @ t`"]
        predictions["Transition Potential Maps @ t+1"]
        evalmodel --> predictions
        changes["Allocate projected land use demand <br> (patch / expand)"]
        predictions --> changes
    end

    estimation --> projection
    projection@{shape : terminal, label: "LULC projection\n @ t+1"}
    projection -->|Next iteration with t+1 as t| estimation

    linkStyle 0,1,3,4 stroke-opacity:0

    classDef phase fill:#f9f9f990,stroke:#333,stroke-dasharray:2 4,color:#000000
    classDef data fill:#ddf1d5,stroke:#82b366,color:#000000
    classDef user_input fill:#fff2cc,stroke:#d6b655,color:#000000

    class setup,calibration,preparation,estimation,allocation phase
    class predictors,lulc_data,projection data
    class domain_categories,domain_space,domain_time user_input
```

## Development Environment Setup

We suggest you use [rv](https://a2-ai.github.io/rv-docs/) to manage dependencies.
Simply create an `rproject.toml` at the project root and execute `rv sync`.

```toml
# sample rproject.toml
[project]
name = "evoland-plus"
r_version = "4.5"

repositories = [
    { alias = "CRAN", url = "https://stat.ethz.ch/CRAN/" },
]

dependencies = [
    { name = "evoland", path = ".", dependencies_only = true, install_suggestions = true },
    "devtools", # sundry development tasks
    "mirai", # modern multiprocessing
    "httpgd", # webview plotting
    "languageserver", # assuming your IDE loads the rv library instead of system
]
```

During development, `devtools::load_all()` acts as though you were calling `library(evoland)` on an installed package.
This makes it easy to rapidly reload code when you've changed something.
Code must be autoformatted using [air](https://posit-dev.github.io/air/) before committing.

## Documentation

This package uses pkgdown, see <http://ethzplus.github.io/evoland-plus>.

- **Tutorials & How-to Guides**: Package vignettes and examples in R man pages
- **Reference Documentation**: R man pages using the standard `?function_name`
- **Explanation & Design Rationale**: See the [project wiki](../../wiki) for
    - Detailed explanation of the modeling approach
    - Database schema and architecture decisions
    - Development guidelines and coding standards

## License

This project is licensed under the AGPL-3 License - see the [LICENSE.md](LICENSE.md) file for details.

## Contributing

Don't hesitate to get in contact with @mmyrte and/or @blenback if you'd like to contribute!

## Acknowledgments

This work represents a re-implementation and enhancement of [Benjamin Black's](/blenback) land use model published at <https://github.com/ethzplus/evoland-plus-legacy>, building on substantial prior experience while modernizing the technical implementation.
