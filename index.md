# evoland-plus

**evoland** **v**isualizes, **o**ptimizes, **l**ocalizes and **a**nd
**d**etermines **p**redictable **l**and **u**se **s**hifts

## Overview

The `evoland-plus` R package (or `evoland` for short) provides tools for
analyzing and projecting land use evolution. The package implements a
statistically calibrated, constrained model for predicting locations of
future land use / land cover change (LULCC).

### Analytic Aim

The fundamental purpose of `evoland-plus` is to:

- **Gather and process** land use/land cover data from multiple sources
- **Calibrate statistical models** to understand historical land use
  transitions
- **Project future land use patterns** using predictive models and
  allocation algorithms
- **Support scenario analysis** through interventions and parameter
  modifications

The package follows a three-phase workflow:

1.  **Data Preparation**: Marshalling heterogeneous data sources into a
    unified database structure
2.  **Calibration**: Feature selection, training Markovian transition
    models, and parameterizing allocation strategies
3.  **Prediction + Allocation**: Recursive model evaluation to generate
    future land use projections

This represents a re-implementation and enhancement of the land use
model published at <https://github.com/ethzplus/evoland-plus-legacy>,
building on substantial prior experience while modernizing the technical
implementation.

## Development Environment Setup

You can use `renv` to set up a development environment with all
dependencies. When you run `renv::init()` from inside the project
directory, you’ll be asked to either install dependencies from the
`DESCRIPTION` file, or to discover them from all project files. The
former should be faster and sufficient, given that dependencies should
always be declared in DESCRIPTION. During development,
`devtools::load_all()` acts as though you were calling
[`library(evoland)`](https://ethzplus.github.io/evoland-plus) on an
installed package. This makes it easy to rapidly reload code when you’ve
changed something.

## Documentation

This package uses pkgdown, see <http://ethzplus.github.io/evoland-plus>.

- **Tutorials & How-to Guides**: Package vignettes and examples in R man
  pages
- **Reference Documentation**: R man pages using the standard
  `?function_name`
- **Explanation & Design Rationale**: See the [project
  wiki](https://ethzplus.github.io/wiki) for
  - Detailed explanation of the modeling approach
  - Database schema and architecture decisions
  - Development guidelines and coding standards

## License

This project is licensed under the AGPL-3 License - see the
[LICENSE.md](https://ethzplus.github.io/evoland-plus/LICENSE.md) file
for details.

## Contributing

Don’t hesitate to get in contact with @mmyrte and/or @blenback if you’d
like to contribute!

## Acknowledgments

This work builds upon the foundational research and implementation
available at <https://github.com/blenback/LULCC-CH>.
