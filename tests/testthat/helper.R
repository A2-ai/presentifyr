# Shared test utilities for presentifyr
# Auto-sourced by testthat before tests run.

#' Create a temporary .pptx file that exists on disk
create_temp_pptx <- function() {
  path <- tempfile(fileext = ".pptx")
  file.create(path)
  path
}

#' Standard mock for run_python_script that returns success
mock_python_success <- function(script_args, label) {
  list(stdout = "", stderr = "", status = 0)
}

#' Mock for run_python_script that simulates a Python failure
mock_python_failure <- function(script_args, label) {
  e <- simpleError("Python script execution failed")
  e$status <- 1
  e$stdout <- ""
  e$stderr <- "Python script error occurred"
  stop(e)
}

#' Mock for run_python_script that simulates missing venv
mock_venv_missing <- function(script_args, label) {
  stop("Create virtual environment with initialize_python")
}
