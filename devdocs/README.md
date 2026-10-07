# Developer documentation

Documentation for people (and agents) working *on* `evoland-plus`, rather than *with* it.
It lives in the repository so that it is versioned and reviewed together with the code it describes; it replaces the former GitHub wiki.

- [`design/`](design/): explanation and design rationale. How the package is put together and why.
  - [workflow.md](design/workflow.md): the modelling workflow the package implements
  - [database.md](design/database.md): the data model, table by table
  - [package-structure.md](design/package-structure.md): classes, files and dependencies
- [`style/`](style/): conventions for writing code and prose. Start at [style/README.md](style/README.md).
  - [r.md](style/r.md), [cpp.md](style/cpp.md), [sql.md](style/sql.md), [prose.md](style/prose.md)

For setup, test and build commands, see [AGENTS.md](../AGENTS.md).

## What goes where

The documentation follows the [Diátaxis](https://diataxis.fr/) framework:

| Kind            | Purpose                                         | Where                                                         |
| --------------- | ----------------------------------------------- | ------------------------------------------------------------- |
| Tutorial        | Learn by working through a complete analysis    | `vignettes/*.qmd`                                             |
| How-to guide    | A recipe for a goal the reader already has      | `vignettes/*.qmd`, `@examples` in roxygen                     |
| Reference       | Exact usage of a function, class or table       | roxygen in `R/`, rendered to `man/` and the [pkgdown site](https://ethzplus.github.io/evoland-plus/) |
| Explanation     | How and why the software is designed as it is   | `devdocs/design/`                                                 |
| Conventions     | How to write code and prose for this repository | `devdocs/style/`                                                  |

Keep each fact in one place.
A design document explains why a table has the keys it has; the column list itself belongs in the table's roxygen `@return`, and the design document links to it or repeats it only where the explanation needs it.
When a change makes a design document wrong, fix the document in the same pull request.

Design documents answer questions such as "Why do we represent climate data in a tabular format?" or "Why is model coupling approached through a database instead of an [FFI](https://en.wikipedia.org/wiki/Foreign_function_interface) or a messaging queue?".
Write them in Markdown, following [style/prose.md](style/prose.md); use Mermaid for diagrams.
