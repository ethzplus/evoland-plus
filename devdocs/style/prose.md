# Prose style

Applies to vignettes (`vignettes/*.qmd`), roxygen documentation, the README, code comments, commit messages and pull request descriptions.

## General

- Write for a land use modeler who knows R and geodata but not this package's internals. Define a domain term (*transition potential*, *allocation*, *anterior*/*posterior*) the first time a document uses it, in italics.
- Plain, direct sentences. Active voice; present tense ("`commit()` checks uniqueness", not "will check").
- Say what something does and why, not how great it is: no "simply", "easily", "powerful", "seamlessly".
- Prefer a short concrete example over an abstract description.
- American spelling, to match the identifiers (`neighbors`, `color`).
- Write the package name as `evoland-plus` in prose, in code formatting; `evoland` is the R package name in code (`library(evoland)`).
- Code identifiers in backticks: functions with parentheses (`as_periods_t()`), methods with `$` (`$commit()`), tables and columns bare (`periods_t`, `id_run`).
- Cite methods from the literature with author, year and a link (DOI preferred), e.g. "Mazy (2022), eq. 3.I.12".

## Markdown

- One sentence per line ([semantic line breaks](https://sembr.org/)). Diffs then show which sentence changed, and reviewers can comment on a single sentence. Don't hard-wrap within a sentence.
- ATX headings (`#`, `##`), sentence case ("Install the bundled R package", not "Install The Bundled R Package").
- `-` for bullet lists; numbered lists only where order matters.
- Fenced code blocks always name a language (```` ```r ````, ```` ```sh ````, ```` ```toml ````).
- Link text says where the link goes; never "here".
- Diagrams as Mermaid code blocks, not images, so they can be diffed and edited.

## Vignettes

Vignettes are [tutorials or how-to guides](../README.md#what-goes-where).
A tutorial walks through a complete analysis; a how-to solves one task for a reader who already knows the workflow.
Say which one a vignette is, and what it assumes, in an italic note at the top:

```markdown
*Note: this tutorial assumes that you already know the basic `evoland-plus` workflow ...*
```

Front matter follows the existing vignettes:

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

The `VignetteIndexEntry` matches the file name.

Code chunks:

- Every chunk has a `#| label:` in kebab-case, saying what the chunk does (`#| label: fit-models`). Labels make error messages during rendering point somewhere and give figures stable file names.
- Chunk options are `#|` comments, not `{r, option=}` headers.
- The first chunk is a hidden setup chunk (`#| include: false`) that removes artifacts of a previous render (`unlink("<name>.evolanddb", recursive = TRUE)`) and sets the seed. Every random step must be reproducible.
- Load packages with `library()` in a visible chunk, so readers can copy it.
- Keep chunks short: one step of the workflow each, followed by prose saying what the output shows. A chunk without explanation before or after it is a sign that it should be merged or explained.
- Use only synthetic data or data downloaded with checksums (`download_and_verify()`), and keep the render under a few minutes; vignettes are built in CI.
- Code shown that must not run (requires Dinamica, a server, credentials) is a `#| eval: false` chunk, with a sentence saying why.
- Use Quarto callouts (`::: {.callout-warning}`) for platform caveats and pitfalls, sparingly.

## Roxygen

- The title is a short noun phrase in title case without a full stop ("Create Period Table").
- The first paragraph of the description says what the object is and when to use it; details follow.
- `@param` starts with the type when it isn't obvious from the name, then the meaning and the default's effect: "`Logical`. If true, the catalog is attached read-only."
- Describe units, valid ranges and 1- or 0-based indexing wherever they apply.
- `@return` for a table lists every column as a bullet: `` - `id_period`: Unique ID for each period ``.

## Code comments

- Full sentences when the comment is a sentence; fragments are fine for short labels.
- Comment *why*: the constraint, the paper, the bug that forced the shape of the code. If a comment explains *what*, rename things until it doesn't need to.
- Keep comments true. When you change code, read the comments around it.

## Commit messages and pull requests

- Subject line: `<scope>: <what changed>`, imperative or descriptive, no full stop, at most ~72 characters. The scope is the function, table or area touched:

  ```
  create_periods_t: make last period same length as others
  ducklake_db: fix concurrent write starvation
  ```

- Body (optional for small changes): why the change was needed, and anything a reviewer can't see from the diff (a schema change, a behavior change for existing databases, a performance trade-off).
- Reference issues as `#123` in the body or subject; `close #123` to close one on merge.
- A pull request description says what changed, why, and how it was tested. Flag schema changes explicitly; see [r.md](r.md#schema-names).
