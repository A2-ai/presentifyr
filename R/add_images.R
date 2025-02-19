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
                       output_pptx) {

  base_pptx_path <- if (!is.null(base_pptx)) {
    base_pptx
  } else {
    "None"  ## Use a blank presentation if no template is uploaded
  }

  script <- system.file("scripts/add_images.py", package = "presentifyr")

  args <- c("run", script, "-f", files, "-r", repo_url, "-o", output_pptx, "-b", base_pptx_path)

  if (is.null(getOption("venv_dir"))) {
    message("Setting options('venv_dir') to project root.")
    options("venv_dir" = here::here())
  }

  venv_path <- file.path(getOption("venv_dir"), ".venv")

  if (!dir.exists(venv_path)) {
    stop("Create virtual environment with initialize_python")
  }

  uv_path <- get_uv_path()

  result <- tryCatch({
    processx::run(
      command = uv_path,
      args = args,
      env = c("current", VIRTUAL_ENV = venv_path),
      error_on_status = TRUE,
      echo = TRUE,
    )
  }, error = function(e) {
    stop(paste("Add images script failed. Status: ", e$status, "Stderr: ", e$stderr))
  })
}
