#' Initialize presentifyr Application Environment
#'
#' @description
#' Bootstraps the Python environment presentifyr's Shiny app needs:
#' calls `fyrstartr::initialize_python(groups = "presentifyr")` to
#' install uv (if missing) and additively sync the `presentifyr`
#' dependency group (python-pptx, pillow) into `.venv/` from the
#' fyrstartr-bundled lockfile. The sync runs in `--inexact` mode, so
#' any packages already present in the venv from prior fyr-package
#' installs are left in place.
#'
#' @param project_dir The file path to the main project directory.
#'   Must already exist. Default is `get_project_dir()`.
#' @param verbose Logical. Print detailed initialization messages.
#'   Default is `TRUE`.
#'
#' @return Invisibly returns a list with initialization status:
#'   \itemize{
#'     \item \code{venv}: Logical indicating `.venv/` exists after the call
#'     \item \code{python_pptx}: Logical indicating python-pptx is importable
#'     \item \code{errors}: Character vector of any errors encountered
#'   }
#'
#' @export
#'
#' @examples \dontrun{
#' initialize_app()
#' }
initialize_app <- function(project_dir = get_project_dir(),
                           verbose = TRUE) {
  status <- list(
    venv = FALSE,
    python_pptx = FALSE,
    errors = character(0)
  )

  log4r::info(
    .le$logger,
    paste("initialize_app: starting, project_dir =", project_dir)
  )

  if (verbose) {
    cat("\n")
    cli::cli_rule("Initializing presentifyr")
    cat("\n")
  }

  # Resolve venv parent matching reportifyr's convention:
  # getOption("venv_dir") if set, otherwise here::here().
  venv_parent <- getOption("venv_dir") %||% here::here()
  venv_path <- file.path(venv_parent, ".venv")
  venv_already <- dir.exists(venv_path)

  # Ensure the venv exists 
  if (verbose) message("1/2 Checking Python venv...")

  if (venv_already) {
    status$venv <- TRUE
    if (verbose) {
      message(
        cli::col_green(cli::symbol$tick), "  .venv already present\n"
      )
    }
  } else {
    tryCatch(
      {
        fyrstartr::initialize_python(
          continue = "Y", groups = "presentifyr"
        )
        status$venv <- dir.exists(venv_path)
        if (verbose && status$venv) {
          message(
            cli::col_green(cli::symbol$tick),
            "  .venv created\n"
          )
        }
      },
      error = function(e) {
        status$errors <<- c(status$errors, paste("venv:", e$message))
        if (verbose) {
          message(
            cli::col_red(cli::symbol$cross),
            "  venv bootstrap failed: ", e$message, "\n"
          )
        }
      }
    )
  }

  # Verify python-pptx; recover if missing 
  if (verbose) message("2/2 Verifying python-pptx...")

  pptx_already <- pptx_importable(venv_path)

  if (!pptx_already && status$venv) {
    # .venv exists but pptx isn't importable -- the venv was set up by
    # a prior fyrstartr call that didn't include the presentifyr group
    log4r::info(
      .le$logger,
      "python-pptx not importable in existing .venv; re-syncing presentifyr group"
    )
    if (verbose) {
      message(
        "  .venv missing python-pptx; running fyrstartr to install presentifyr group..."
      )
    }
    tryCatch(
      fyrstartr::initialize_python(
        continue = "Y", groups = "presentifyr"
      ),
      error = function(e) {
        status$errors <<- c(
          status$errors, paste("python-pptx sync:", e$message)
        )
      }
    )
  }

  status$python_pptx <- pptx_importable(venv_path)
  if (verbose) {
    if (status$python_pptx) {
      label <- if (pptx_already) "already importable" else "importable"
      message(
        cli::col_green(cli::symbol$tick), "  python-pptx ", label, "\n"
      )
    } else {
      message(
        cli::col_red(cli::symbol$cross), "  python-pptx not importable\n"
      )
    }
  }

  # Summary 
  cli::cli_rule("Initialization Summary")

  message(
    if (status$venv) {
      paste0(
        cli::col_green(cli::symbol$tick), " .venv:              ",
        if (venv_already) "ALREADY EXISTED" else "CREATED"
      )
    } else {
      paste0(cli::col_red(cli::symbol$cross), " .venv:              FAILED")
    }
  )
  message(
    if (status$python_pptx) {
      paste0(
        cli::col_green(cli::symbol$tick), " python-pptx:        ",
        if (pptx_already) "ALREADY INSTALLED" else "INSTALLED"
      )
    } else {
      paste0(
        cli::col_red(cli::symbol$cross), " python-pptx:        FAILED"
      )
    }
  )

  if (length(status$errors) > 0) {
    cat("\n")
    cli::cli_rule("Errors")
    for (err in status$errors) {
      message(cli::col_red(cli::symbol$cross), " ", err)
    }
  }

  message(strrep("-", getOption("width", 80)), "\n")

  log4r::info(.le$logger, paste0(
    "initialize_app: complete, venv=", status$venv,
    ", python_pptx=", status$python_pptx
  ))

  invisible(status)
}

#' Test whether `import pptx` succeeds in the given venv
#'
#' @param venv_path Path to `.venv/` directory.
#'
#' @return Logical. `TRUE` if the venv exists and python can `import pptx`.
#'
#' @keywords internal
#' @noRd
pptx_importable <- function(venv_path) {
  if (!dir.exists(venv_path)) return(FALSE)

  python_exe <- if (.Platform$OS.type == "windows") {
    file.path(venv_path, "Scripts", "python.exe")
  } else {
    file.path(venv_path, "bin", "python")
  }
  if (!file.exists(python_exe)) return(FALSE)

  result <- tryCatch(
    processx::run(
      command = python_exe,
      args = c("-c", "import pptx"),
      error_on_status = FALSE,
      stdout = "|",
      stderr = "|"
    ),
    error = function(e) list(status = -1L)
  )
  isTRUE(result$status == 0)
}
