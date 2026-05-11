#' Initialize presentifyr Application Environment
#'
#' @description
#' Bootstraps the Python environment presentifyr's Shiny app needs by
#' calling `fyrstartr::initialize_python(groups = "presentifyr")`. This
#' installs uv (if missing) and additively syncs the `presentifyr`
#' dependency group (python-pptx, pillow) into `.venv/` from the
#' fyrstartr-bundled lockfile. The sync runs in `--inexact` mode, so any
#' packages already present in the venv from prior fyr-package installs
#' are left in place.
#'
#' `uv sync --frozen --inexact --group presentifyr` is idempotent — it
#' creates the venv if missing, installs the group if absent, and is a
#' fast no-op if everything is already in place. Re-running this
#' function is therefore safe and cheap.
#'
#' @param verbose Logical. Print initialization messages. Default `TRUE`.
#'
#' @return Invisibly a list:
#'   \itemize{
#'     \item \code{success}: Logical. `TRUE` iff fyrstartr returned without error.
#'     \item \code{errors}: Character vector of any error detail captured.
#'   }
#'
#' @export
#'
#' @examples \dontrun{
#' initialize_app()
#' }
initialize_app <- function(verbose = TRUE) {
  status <- list(success = FALSE, errors = character(0))

  log4r::info(.le$logger, "initialize_app: starting")

  if (verbose) {
    cat("\n")
    cli::cli_rule("Initializing presentifyr")
    cat("\n")
  }

  tryCatch(
    {
      fyrstartr::write_group_to_pyproject("presentifyr")
      fyrstartr::initialize_python(groups = "presentifyr")
      status$success <- TRUE
    },
    error = function(e) {
      detail <- if (!is.null(e$stderr) && nzchar(trimws(e$stderr))) {
        paste0(e$message, "\n", trimws(e$stderr))
      } else {
        e$message
      }
      status$errors <<- c(status$errors, detail)
    }
  )

  if (verbose) {
    if (status$success) {
      message(
        cli::col_green(cli::symbol$tick),
        " presentifyr environment ready"
      )
    } else {
      message(
        cli::col_red(cli::symbol$cross),
        " initialization failed"
      )
      for (err in status$errors) {
        message("  ", err)
      }
    }
    cat("\n")
  }

  log4r::info(
    .le$logger,
    paste0("initialize_app: complete, success=", status$success)
  )

  invisible(status)
}
