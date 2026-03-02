
<!-- README.md is generated from README.Rmd. Please edit that file -->

# presentifyr <a href="https://github.com/a2-ai/presentifyr/"><img src="inst/www/logo.png" align="right" height="139" alt="presentifyr website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/A2-ai/presentifyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/A2-ai/presentifyr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`presentifyr` is a Shiny application that builds PowerPoint
presentations from R-generated figures. Select images from your project,
choose slide layouts from your template, configure footnote styling, and
generate a polished .pptx in seconds. When figures or footnotes change
between review rounds, use the built-in sync workflow to quickly rebuild
the deck.

## Prerequisites

- R (\>= 4.0),
- A [reportifyr](https://github.com/A2-ai/reportifyr)-compatible project
  structure,
- A remote Git repository for the project,
- Image files only (`.png`, `.jpg`, etc.) — PDFs are not supported.

## Installation

You can install the development version of `presentifyr` like so:

``` r
pak::pkg_install("a2-ai/presentifyr")
```

## Getting Started

### 1. Initialize

`initialize_app()` sets up the Python virtual environment (via
`reportifyr`) and installs `python-pptx`:

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
| `options("python-pptx.version")` | `"1.0.2"` | Pinned `python-pptx` version |

Set these before calling `app()` or `initialize_app()`:

``` r
options(project.dir = "/path/to/project")
options(presentifyr.exclude_dirs = c("renv", "data"))
```
