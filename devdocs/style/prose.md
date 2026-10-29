# Prose style

Applies to vignettes (`vignettes/*.qmd`), roxygen documentation, Markdown files, commit messages and pull request descriptions.

## General

- Write engaging but technical documentation, not marketing copy. Avoid rhetorical contrasts such as "it's not the x, it's the y" or "it never was the z"; state what something is and does.
- Write for a GIS modeler who knows R and geodata but not this package's internals. Define a domain term (*transition potential*, *allocation*, *anterior*/*posterior*) in italics the first time a document uses it.
- American spelling, to match the identifiers (`neighbors`, `color`).
- The project and the R package are both `evoland.plus` (`library(evoland.plus)`); write it in backticks in prose.
- Code identifiers in backticks: functions with parentheses (`as_periods_t()`), methods with `$` (`$commit()`), tables and columns bare (`periods_t`, `id_run`).
- Cite methods from the literature with author, year, a link (DOI preferred) and the equation or section, e.g. "Mazy (2022), eq. 3.I.12".

## Markdown

- One sentence per line ([semantic line breaks](https://sembr.org/)), so diffs and review comments address single sentences.
- Diagrams as Mermaid code blocks, not images, so they can be diffed and edited.

## Vignettes

Vignettes are [tutorials or how-to guides](../README.md#what-goes-where).
Say which one a vignette is, and what it assumes, in an italic note at the top:

```markdown
*Note: this tutorial assumes that you already know the basic `evoland.plus` workflow ...*
```

Front matter follows the existing vignettes, with a `VignetteIndexEntry` matching the file name:

```yaml
---
title: "Stochastic allocation sensitivity with `runs_t`"
author: Jane Doe
date: last-modified
vignette: >
  %\VignetteIndexEntry{stochastic-allocation-sensitivity}
  %\VignetteEngine{quarto::html}
  %\VignetteEncoding{UTF-8}
number-sections: true
---
```

Code chunks:

- Every chunk has a kebab-case `#| label:` saying what it does (`#| label: fit-models`), so render errors point somewhere and figures get stable file names.
- The first chunk is a hidden setup chunk (`#| include: false`) that removes artifacts of a previous render (`unlink("<name>.evolanddb", recursive = TRUE)`) and sets the seed.
- One workflow step per chunk, followed by prose saying what the output shows.
- Use only synthetic ad-hoc data or data included in the package via `data-raw/`. Keep the render under a few minutes; vignettes are built in CI.
- Code that must not run (needs Dinamica, a server, credentials) goes in an `#| eval: false` chunk, with a sentence saying why.

## Roxygen

Much of the existing documentation groups a table's functions with `@describeIn`.
roxygen still supports it, but advises structuring such pages by hand; treat `@describeIn` as legacy and prefer a manual structure with sections that fit the content.
A canonical layout for the table pages (`as_*_t()` and their helpers) is to be worked out in ethzplus/evoland.plus#81.

- The title is a short noun phrase in title case without a full stop ("Create Period Table").
- `@param` starts with the type when it isn't obvious from the name, then the meaning and the default's effect: "`Logical`. If true, the catalog is attached read-only."
- State units, valid ranges and 0- or 1-based indexing wherever they apply.
- `@return` for a table lists every column as a bullet: `` - `id_period`: Unique ID for each period ``.

## Commit messages and pull requests

- Subject line: `<scope>: <what changed>`, at most ~72 characters. The scope is the function, table or area touched:

  ```
  create_periods_t: make last period same length as others
  ducklake_db: fix concurrent write starvation
  ```

- The body explains the rationale for the change, concisely. What changed, including schema or behavior changes, should be evident from the diff.
