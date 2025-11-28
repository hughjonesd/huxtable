# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## 

How to behave: - Dial down the sycophancy. You don’t need to (e.g.)
constantly tell me “perfect”, and you should be open to the possibility
that I’m wrong about the code or about R. Feel free to challenge my
assumptions.

## Project Overview

Huxtable is an R package for creating styled tables in multiple output
formats (HTML, LaTeX, RTF, Word, Excel, PowerPoint). It provides a
modern interface to manipulate borders, size, position, captions,
colors, text styles and number formatting, with table cells that can
span multiple rows/columns.

## Development Commands

### Testing

- Run tests:
  [`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
- Run all tests except fuzz tests:
  `devtools::test(filter = "^(?!.*fuzz)", perl = TRUE)`
- Run single test file: `devtools::test(filter = "test-file-name")`
- Test files are in `tests/testthat/`

### Building and Checking

- Build package:
  [`devtools::build()`](https://devtools.r-lib.org/reference/build.html)
- Build without vignettes:
  `devtools::build(build_opts = "--no-build-vignettes")`
- Run R CMD check: `devtools::check(document = FALSE, remote = TRUE)`
- Generate documentation:
  [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)

### Coverage

- Run coverage analysis: see `run-covr.R` script

### Release Process

- Full CRAN release workflow: see `build-cran.R` script
- Includes vignette building, reverse dependency checks, and
  cross-platform testing

## Architecture Overview

### Core Object Structure

- Main constructor: [`huxtable()`](reference/huxtable.md) (aliased as
  [`hux()`](reference/huxtable.md)) in `R/creation.R`
- Internal constructor: `new_huxtable()` in `R/creation-internal.R`
- Huxtable objects are data frames with matrix/vector attributes for
  formatting properties
- Properties include cell, column, row, and table-level attributes
  stored in `huxtable_env$huxtable_default_attrs`

### Key Component Files

- **Property system**: `R/property-helpers.R` - defines supported
  properties and helper functions
- **Content processing**: `R/clean-contents.R` - handles number
  formatting, markdown rendering, escaping
- **Output renderers**:
  - `R/html.R` - HTML table generation
  - `R/latex.R` - LaTeX output with floating environments
  - `R/typst.R` - Typst format support
  - `R/rtf.R` - RTF format
  - `R/md.R` - Markdown output
- **Integration**: `R/knitr.R` - knitr/RMarkdown integration with format
  detection
- **Table manipulation**:
  - `R/subset-extract.R` - subsetting operations
  - `R/merge.R` - cell merging
  - `R/bind-insert.R` - row/column operations
- **Quick functions**: `R/quick-functions.R` - one-liner output
  functions
- **Regression tables**: `R/huxreg.R` - automated regression table
  creation

### Property Management

Properties are organized into four categories: - Cell properties
(alignment, borders, padding, colors, text formatting) - Column
properties (width, header designation) - Row properties (height) - Table
properties (caption, position, width)

Property getters/setters use a consistent interface with mapping
functions for bulk operations.

### Output Pipeline

1.  Content cleaning via `clean_contents()` (number formatting,
    markdown, escaping)
2.  Format-specific rendering (HTML, LaTeX, etc.)
3.  Integration with document systems (knitr, RMarkdown, Quarto)

## Development Guidelines

### Code Organization

- Functions grouped by responsibility in separate R files
- Property-related functions follow naming conventions (`get_*`,
  `set_*`)
- Test files mirror R file organization with `test-` prefix

### Testing Requirements

- Write tests for user-visible bugs with “Bugfix: …” titles
- Place tests in most relevant existing test file
- Use `skip_if_not_installed()` for optional dependencies
- Files `test-yy-end-to-end.R` and `test-zz-fuzz.R` may take longer to
  run

### Documentation Standards

- New functions require Roxygen documentation
- Internal functions use `@noRd` tag
- User-visible changes require NEWS.md entries
- Run
  [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
  after documentation changes

### Branch Naming

Use short branch names starting with: “feature”, “bugfix”, “refactor”,
or “chore”

### Important Notes

- Don’t change the API without asking first
- Update `design.md` for architectural insights
- Update `agent-notes.md` for general development notes
- Install optional packages as needed from DESCRIPTION Suggests
- Some packages (flextable, lmtest) may be missing - install if working
  on those features
- AGENTS.md was written with OpenAI Codex in mind, not everything is
  accurate.

## Key Files to Reference

- `design.md` - detailed architecture overview
- `agent-notes.md` - notes from previous development work
- `AGENTS.md` - specific instructions for LLM agents
- `README.md` - user-facing documentation
- `DESCRIPTION` - package metadata and dependencies

## Additional Tools

- Typst documentation: <https://github.com/typst/typst/tree/main/docs>

- You can set environment variable NOT_CRAN to avoid skip_on_cran(), I
  tink
