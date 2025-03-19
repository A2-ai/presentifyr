#' Initializes python virtual environment
#'
#' @export
#'
#' @examples \dontrun{
#' initialize_python()
#' }
initialize_python <- function() {
  cmd <- system.file("scripts/uv_setup.sh", package = "presentifyr")

  if (is.null(getOption("venv_dir"))) {
    options("venv_dir" = here::here())
  }

  args <- c(getOption("venv_dir"))

  if (!is.null(getOption("python-pptx.version"))) {
    args <- c(args, getOption("python-pptx.version"))
  } else {
    args <- c(args, "1.0.2")
  }

  if (!is.null(getOption("uv.version"))) {
    args <- c(args, getOption("uv.version"))
  } else {
    args <- c(args, "0.5.1")
  }

  if (!is.null(getOption("python.version"))) {
    args <- c(args, getOption("python.version"))
  }

  uv_path <- get_uv_path()

  if (!dir.exists(file.path(args[[1]], ".venv"))) {

    result <- processx::run(
      command = cmd,
      args = args
    )

    args_name <- c("venv_dir", "python-pptx.version", "uv.version", "python.version")
    pyvers <- get_py_version(getOption("venv_dir"))
    if (!is.null(pyvers)) {
      args <- c(args, pyvers)
    } else {
      args <- c(args, "")
    }

    message(paste(
      "Creating python virtual environment with the following settings:\n",
      paste0("\t", args_name, ": ", args, collapse = "\n")
    ))

  } else if (!file.exists(uv_path)) {
    message("installing uv")
    result <- processx::run(
      command = cmd,
      args = args
    )
  } else {
    message(paste(
      ".venv already exists at:",
      file.path(args[[1]], ".venv")
    ))
    result <- processx::run(
      command = cmd,
      args = args
    )
  }
}

#' Grabs python version for .venv
#'
#' @param venv_dir The file path to the .venv directory
#'
#' @return A string of python version or NULL.
#' @keywords internal
#' @noRd
get_py_version <- function(venv_dir) {
  file_path <- file.path(venv_dir, ".venv", "pyvenv.cfg")

  file_content <- readLines(file_path)

  version_info_line <- grep("version_info = ", file_content, value = TRUE)

  if (length(version_info_line) > 0) {
    version_info <- sub(".*version_info =\\s*", "", version_info_line)
    return(version_info)
  } else {
    return(NULL)
  }
}
