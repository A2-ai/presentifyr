# presentifyr

<!-- badges: start -->

<!-- badges: end -->

The goal of `presentifyr` is to automate slideshow creation for internal reviews, while also supporting recurring review cycles that involve updating figures and images within the presentation.

## Installation

To install the development version of `presentifyr`:

1. In your general project directory, `gh repo clone A2-ai/presentifyr`

2. For projects using renv snapshots dated 2024-07-20 or later, run the following commands within the project where you intend to use the app (execute each separately):

``` r
install.packages("devtools") # If not already installed.
```

``` r
devtools::load_all("path/to/presentifyr") # The path is the relative path to the `presentifyr` repository from your current working directory. 
```

## Initializing Python

A mixture of Python and R is used within `presentifyr`, thus requiring a virtual environment (.venv) to be created and managed. Fortunately, this is automated through the use of the `initialize_python()` function.

Before launching the app, please run the following to initialize a .venv and to install all required Python components:

``` r
initialize_python()
```

## Launching the Application

To launch the Shiny application, please run the following:

``` r
app()
```

## Initial Application Requirements

-   Requires the project to have a remote Git repository set up.
-   Only works for image files (.png, .jpg, etc.). PDF files are not supported and will not be displayed in the file tree.
