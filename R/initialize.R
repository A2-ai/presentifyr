#' Initialize presentifyr Application Environment
#'
#' @description
#' Performs all necessary initialization steps for presentifyr,
#' including reportifyr project setup (uv, .venv, and base Python
#' dependencies) and python-pptx installation.
#'
#' @param project_dir The file path to the main project directory.
#'   Must already exist. Default is `get_project_dir()`.
#' @param report_dir_name The directory name for reports. Default is `NULL`
#'   (uses reportifyr default).
#' @param outputs_dir_name The directory name for artifacts. Default is `NULL`
#'   (uses reportifyr default).
#' @param verbose Logical. Print detailed initialization messages.
#'   Default is `TRUE`.
#'
#' @return Invisibly returns a list with initialization status:
#'   \itemize{
#'     \item \code{reportifyr}: Logical indicating reportifyr initialization success
#'     \item \code{python_pptx}: Logical indicating python-pptx installation success
#'     \item \code{errors}: Character vector of any errors encountered
#'   }
#'
#' @export
#'
#' @examples \dontrun{
#' initialize_app()
#' }
initialize_app <- function(
    project_dir = get_project_dir(),
    report_dir_name = NULL,
    outputs_dir_name = NULL,
    verbose = TRUE
) {
  status <- list(
    reportifyr = FALSE,
    python_pptx = FALSE,
    errors = character(0)
  )

  if (verbose) {
    cat("\n")
    cli::cli_rule("Initializing presentifyr")
    cat("\n")
  }

  # Step 1: Initialize reportifyr project (uv, .venv, python-docx, pyyaml, pillow)
  if (verbose) message("1/2 Initializing reportifyr project...")
  tryCatch({
    suppressMessages({
      reportifyr::initialize_report_project(
        project_dir = project_dir,
        report_dir_name = report_dir_name,
        outputs_dir_name = outputs_dir_name
      )
    })
    status$reportifyr <- TRUE
    if (verbose) message(cli::col_green(cli::symbol$tick), "  reportifyr initialization complete\n")
  }, error = function(e) {
    status$errors <<- c(status$errors, paste("reportifyr:", e$message))
    if (verbose) {
      message(cli::col_red(cli::symbol$cross), "  reportifyr initialization failed: ", e$message, "\n")
    }
  })

  # Step 2: Install python-pptx
  if (verbose) message("2/2 Installing python-pptx...")
  tryCatch({
    py_status <- install_pptx(verbose = FALSE)
    status$python_pptx <- py_status
    if (verbose) {
      if (py_status) {
        message(cli::col_green(cli::symbol$tick), "  python-pptx installed\n")
      } else {
        message(cli::col_red(cli::symbol$cross), "  python-pptx failed to install\n")
      }
    }
  }, error = function(e) {
    status$errors <<- c(status$errors, paste("python-pptx:", e$message))
    if (verbose) {
      message(cli::col_red(cli::symbol$cross), "  python-pptx failed to install: ", e$message, "\n")
    }
  })

  # Summary
  if (verbose) {
    cli::cli_rule("Initialization Summary")
  }

  if (status$reportifyr) {
    message(cli::col_green(cli::symbol$tick), " reportifyr:         SUCCESS")
  } else {
    message(cli::col_red(cli::symbol$cross), " reportifyr:         FAILED")
  }

  tryCatch({
    paths <- reportifyr::get_venv_uv_paths()
    venv_exists <- dir.exists(paths$venv)
    if (venv_exists) {
      message(cli::col_green(cli::symbol$tick), " .venv:              SUCCESS")
    } else {
      message(cli::col_red(cli::symbol$cross), " .venv:              FAILED")
    }
  }, error = function(e) {
    message(cli::col_red(cli::symbol$cross), " .venv:              FAILED")
  })

  if (status$python_pptx) {
    message(cli::col_green(cli::symbol$tick), " python-pptx:        SUCCESS")
  } else {
    message(cli::col_red(cli::symbol$cross), " python-pptx:        FAILED")
  }

  if (length(status$errors) > 0) {
    cat("\n")
    cli::cli_rule("Errors")
    for (err in status$errors) {
      message(cli::col_red(cli::symbol$cross), " ", err)
    }
  }

  message(strrep("\u2500", getOption("width", 80)), "\n")

  invisible(status)
}

#' Install python-pptx into the reportifyr virtual environment
#'
#' @param verbose Logical. Print installation messages. Default is `TRUE`.
#'
#' @return Invisibly returns `TRUE` on success, `FALSE` on failure.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' install_pptx()
#' }
install_pptx <- function(verbose = TRUE) {
  paths <- reportifyr::get_venv_uv_paths()
  uv <- paths$uv
  venv <- paths$venv
  python_exe <- file.path(venv, "bin", "python")

  pptx_version <- getOption("python-pptx.version", default = "1.0.2")

  # Check if python-pptx is already installed
  if (verbose) message("  Checking if python-pptx is already installed...")
  check_result <- processx::run(
    command = python_exe,
    args = c("-c", "import pptx; print(pptx.__version__)"),
    error_on_status = FALSE,
    stdout = "|",
    stderr = "|"
  )

  if (check_result$status == 0) {
    version <- trimws(check_result$stdout)
    if (verbose) message("  python-pptx already installed (version ", version, ")")
    return(invisible(TRUE))
  }

  # Not installed, proceed with installation
  if (verbose) message("  Installing python-pptx==", pptx_version, "...")

  result <- processx::run(
    command = uv,
    args = c(
      "pip", "install", paste0("python-pptx==", pptx_version),
      paste0("--python=", python_exe)
    ),
    echo = FALSE,
    stdout = "|",
    stderr = "|",
    error_on_status = FALSE
  )

  if (result$status == 0) {
    if (verbose) message("  python-pptx installed successfully")
    invisible(TRUE)
  } else {
    if (verbose) message("  python-pptx installation failed")
    invisible(FALSE)
  }
}
