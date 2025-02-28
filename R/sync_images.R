#' Syncs images from file to a given PowerPoint presentation (.pptx) file.
#'
#' @param input_pptx The file path to the input .pptx file.
#' @param output_pptx The file path where the modified .pptx file will be saved.
#'
#' @keywords internal
#'
#' @examples \dontrun{
#' sync_images(
#'   input_pptx = input_pptx,
#'   output_pptx = output_pptx
#' )
#' }
sync_images <- function(input_pptx,
                        output_pptx) {
  log4r::debug(.le$logger, "Starting sync_images R function")

  image_files <- parse_directory_for_images(
    directory = here::here(),
    exclude_dirs = c("/renv/") ## Exclude unnecessary directories
  )

  if (length(image_files) == 0) {
    stop("No images found in the specified directory")
  }

  log4r::debug(.le$logger, paste0("Found ", length(image_files), " image files"))

  image_dict <- as.list(stats::setNames(image_files, basename(image_files)))
  temp_image_dict <- tempfile(fileext = ".json")
  jsonlite::write_json(image_dict, temp_image_dict, auto_unbox = TRUE, pretty = TRUE)
  log4r::debug(.le$logger, paste("Temporary image dictionary created at:", temp_image_dict))

  script <- system.file("scripts/sync_images.py", package = "presentifyr")
  args <- c("run", script, "-i", input_pptx, "-o", output_pptx, "-d", temp_image_dict)

  if (is.null(getOption("venv_dir"))) {
    message("Setting options('venv_dir') to project root")
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
      env = c("current", VIRTUAL_ENV = venv_path, PY_LOG_LEVEL = Sys.getenv("PRFY_VERBOSE", unset = "WARN")),
      error_on_status = TRUE,
      echo = TRUE,
    )
  }, error = function(e) {
    stop(paste("Sync images script failed. Status: ", e$status, "Stderr: ", e$stderr))
  })
}
