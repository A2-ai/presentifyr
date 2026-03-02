#' @importFrom rlang %||%
NULL

.onLoad <- function(...) {
  shiny::addResourcePath("presentifyr", system.file(".", package = "presentifyr"))
  options(shiny.maxRequestSize = 5 * 1024^3)
  toggle_logger()
}

.onAttach <- function(...){
  msg <- presentifyr_options_message()
  packageStartupMessage(msg)
}

.onUnload <- function(...) {
  shiny::removeResourcePath("presentifyr")
}

#' Generates a tidyverse-esque onAttach message
#'
#' @return A message to display on attach
#' @keywords internal
#' @noRd
#'
#' @examples \dontrun{
#' presentifyr_options_message()
#' }
presentifyr_options_message <- function() {
  set_options <- c()
  project_options <- c()
  version_options <- c()

  ## Project dir
  project_dir <- getOption("project.dir")
  if (is.null(project_dir)) {
    project_options <- c(
      project_options,
      "Using here::here() as project root, set options('project.dir') to change"
    )
  } else {
    set_options <- c(set_options, paste("project.dir:", project_dir))
  }

  ## Exclude dirs
  exclude_dirs <- getOption("presentifyr.exclude_dirs")
  if (is.null(exclude_dirs)) {
    project_options <- c(
      project_options,
      "Using default exclude dirs, set options('presentifyr.exclude_dirs') to change"
    )
  } else {
    set_options <- c(set_options, paste("presentifyr.exclude_dirs:", paste(exclude_dirs, collapse = ", ")))
  }

  ## Version options
  pptx_vers <- getOption("python-pptx.version")
  if (is.null(pptx_vers)) {
    version_options <- c(
      version_options,
      "Using python-pptx version 1.0.2, set options('python-pptx.version') to change"
    )
  } else {
    set_options <- c(set_options, paste("python-pptx.version:", pptx_vers))
  }

  ## Format .onAttach message
  msg <- ""
  if (length(set_options)) {
    msg <- paste0(
      msg,
      cli::rule(
        left = cli::style_bold("Set presentifyr options")
      ), "\n",
      paste0(
        cli::col_green(cli::symbol$tick), " ", set_options,
        collapse = "\n"
      ), "\n"
    )
  }

  if (length(project_options)) {
    msg <- paste0(
      msg,
      cli::rule(
        left = cli::style_bold("Project options")
      ), "\n",
      paste0(
        cli::col_yellow(cli::symbol$square), " ", project_options,
        collapse = "\n"
      ), "\n"
    )
  }

  if (length(version_options)) {
    msg <- paste0(
      msg,
      cli::rule(
        left = cli::style_bold("Version options")
      ), "\n",
      paste0(
        cli::col_yellow(cli::symbol$square), " ", version_options,
        collapse = "\n"
      )
    )
  }

  msg
}
