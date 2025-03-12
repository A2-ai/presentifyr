#' Adds images from file to a given PowerPoint presentation (.pptx) file.
#'
#' @param files A list of image file paths to be inserted into the .pptx file.
#' @param repo_url The url of the repository where an image is stored.
#' @param base_pptx The file path to an existing .pptx file that serves as a template. Default is NULL. If NULL, a blank presentation is used.
#' @param output_pptx The file path where the modified .pptx file will be saved.
#'
#' @keywords internal
#'
#' @examples \dontrun{
#' add_images(
#'   files = files,
#'   repo_url = repo_url,
#'   base_pptx = NULL,
#'   output_pptx = output_pptx
#' )
#' }
add_images <- function(files,
                       repo_url,
                       base_pptx = NULL,
                       slide_layout_index = NULL,
                       output_pptx) {
  log4r::debug(.le$logger, "Starting add images R function")

  script <- system.file("scripts/add_images.py", package = "presentifyr")

  if (is.null(slide_layout_index)) {
    slide_layout_index <- 8 ## Hard coded for blank presentations
    log4r::info(.le$logger, paste("Slide_layout_index is NULL, setting value to: ", slide_layout_index))
  }

  args <- c("run", script, "-f", files, "-r", repo_url, "-l", slide_layout_index, "-o", output_pptx)

  if (!is.null(base_pptx)) {
    args <- c(args, "-b", base_pptx)
  }

  if (is.null(getOption("venv_dir"))) {
    log4r::info(.le$logger, "Setting options('venv_dir') to project root.")
    message("Setting options('venv_dir') to project root.")

    options("venv_dir" = here::here())
  }

  venv_path <- file.path(getOption("venv_dir"), ".venv")

  if (!dir.exists(venv_path)) {
    log4r::error(.le$logger, "Virtual environment not found. Please initialize with initialize_python.")
    stop("Create virtual environment with initialize_python")
  }
  log4r::debug(.le$logger, paste("venv_path resolved to: ", venv_path))

  uv_path <- get_uv_path()
  log4r::debug(.le$logger, paste("uv path resolved to:", uv_path))

  result <- tryCatch({
    processx::run(
      command = uv_path,
      args = args,
      env = c("current", VIRTUAL_ENV = venv_path, PY_LOG_LEVEL = Sys.getenv("PRFY_VERBOSE", unset = "WARN")),
      error_on_status = TRUE,
      echo = TRUE,
    )
  }, error = function(e) {
    log4r::error(.le$logger, paste0("Add images Python script failed. Status: ", e$status))
    log4r::error(.le$logger, paste0("Add images Python script failed. Stderr: ", e$stderr))
    log4r::info(.le$logger, paste0("Add images Python script failed. Stdout: ", e$stdout))
    stop(paste("Add images Python script failed. Status: ", e$status, "Stderr: ", e$stderr))
  })
  log4r::debug(.le$logger, "Exiting add images R function")
}
