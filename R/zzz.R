.onLoad <- function(...) {
  shiny::addResourcePath("presentifyr", system.file(".", package = "presentifyr"))
  options(shiny.maxRequestSize = 5000*1024^2)
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
  unset_options <- c()
  optional_options <- c()

  ## Check for each used options
  root <- getOption("venv_dir")
  if (is.null(root)) {
    unset_options <- c(unset_options, "options('venv_dir') is not set. venv will be created in Project root")
  } else {
    set_options <- c(set_options, paste("venv_dir:", root))
  }
  ## Nice to haves
  uvversion <- getOption("uv.version")
  if (is.null(uvversion)) {
    optional_options <- c(optional_options, "options('uv.version') is not set. Default is 0.5.1")
  } else {
    set_options <- c(set_options, paste("uv.version:", uvversion))
  }

  pyversion <- getOption("python.version")
  if (is.null(pyversion)) {
    optional_options <- c(optional_options, "options('python.version') is not set. Default is system version")
  } else {
    set_options <- c(set_options, paste("python.version:", pyversion))
  }

  pptx_vers <- getOption("python-pptx.version")
  if (is.null(pptx_vers)) {
    optional_options <- c(optional_options, "options('python-pptx.version') is not set. Default is 1.0.2")
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

  if (length(unset_options)) {
    msg <- paste0(
      msg,
      cli::rule(
        left = cli::style_bold("Needed presentifyr options")
      ), "\n",
      paste0(
        cli::col_red(cli::symbol$cross), " ", unset_options,
        collapse = "\n"
      ), "\n",
      paste0(
        cli::col_cyan(cli::symbol$info), " ",
        cli::format_inline("Please set all options for package to work."), "\n"
      )
    )
  }

  if (length(optional_options)) {
    msg <- paste0(
      msg,
      cli::rule(
        left = cli::style_bold("Optional version options")
      ), "\n",
      paste0(
        cli::col_yellow(cli::symbol$square), " ", optional_options,
        collapse = "\n"
      )
    )
  }

  msg
}
