
<!-- README.md is generated from README.Rmd. Please edit that file -->

# presentifyr <a href="https://github.com/a2-ai/presentifyr/"><img src="man/figures/logo.png" align="right" height="139" alt="presentifyr website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/A2-ai/presentifyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/A2-ai/presentifyr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`presentifyr` is a Shiny application that builds PowerPoint
presentations from R-generated figures. Select images from your project,
choose slide layouts from your template, configure footnote styling, and
generate a polished .pptx in seconds. When figures or footnotes change
between review rounds, use the built-in sync workflow to quickly rebuild
the deck with up-to-date content.

## Prerequisites

- **R (\>= 4.0).**
- **A [reportifyr](https://github.com/A2-ai/reportifyr)-initialized
  project is recommended** — for additional features. See [Setting the Stage](#setting-the-stage).
- **Image files only** (`.png`, `.jpg`, etc.) — PDFs are not supported.

## Setting the Stage

`presentifyr` reads two kinds of files from a reportifyr-initialized
project, both enabling elective features:

- **Image metadata sidecars** — `_metadata.json` files saved next to
  each image by `reportifyr::ggsave_with_metadata()` or
  `reportifyr::write_object_metadata()`. Each sidecar may declare an
  abbreviation key or `meta_type` to be decoded against the Footnotes
  YAML below.
- **Footnotes YAML** — `<report_dir>/standard_footnotes.yaml`, defining
  `abbreviations`, `figure_footnotes`, and `table_footnotes`.
  `presentifyr` uses this as a lookup table when an image's metadata
  sidecar references an abbreviation key or `meta_type`, decoding them
  into footnote text on each slide. `<report_dir>` defaults to `report`;
  override with `options("presentifyr.report_dir_name")` if your project
  uses a different name.

## Installation

You can install the development version of `presentifyr` like so:

``` r
pak::pkg_install("a2-ai/presentifyr")
```

## Getting Started

### 1. Initialize

`initialize_app()` sets up the Python virtual environment (via `pyro`)
and installs `python-pptx`:

``` r
library(presentifyr)
initialize_app()
```

### 2. Launch

``` r
app()
```

## Configuration

`presentifyr` respects three options, all displayed on package attach:

| Option | Default | Description |
|----|----|----|
| `options("project.dir")` | `here::here()` | Project root directory |
| `options("presentifyr.exclude_dirs")` | Internal list | Directories hidden from the file tree |
| `options("presentifyr.report_dir_name")` | `"report"` | Report directory name (joined onto `project.dir`) |

Set these before calling `app()` or `initialize_app()`:

``` r
options(project.dir = "/path/to/project")
options(presentifyr.exclude_dirs = c("renv", "data"))
options(presentifyr.report_dir_name = "deliverables")
```
