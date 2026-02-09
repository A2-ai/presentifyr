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
  log4r::debug(.le$logger, "Starting sync images R function")

  if (!file.exists(input_pptx)) {
    log4r::error(.le$logger, paste("The input .pptx file does not exist:", input_pptx))
    stop(paste("The input .pptx file does not exist:", input_pptx))
  }

  if (!grepl("\\.pptx$", input_pptx, ignore.case = TRUE)) {
    log4r::error(.le$logger, paste("Invalid file type. Expected a .pptx file:", input_pptx))
    stop("Invalid file type. Expected a .pptx file.")
  }

  exclude_dirs <- default_exclude_dirs()

  image_files <- parse_directory_for_images(
    directory = here::here(),
    exclude_dirs = exclude_dirs ## Exclude unnecessary directories
  )

  if (length(image_files) == 0 || all(image_files == "")) {
    log4r::error(.le$logger, paste("No image files found. Execution halted."))
    stop("No image files found. Execution halted.")
  }

  log4r::debug(.le$logger, paste0("Found ", length(image_files), " image files"))

  ## Use relative paths as keys to avoid basename collisions
  project_root <- here::here()
  relative_paths <- fs::path_rel(image_files, project_root)
  image_dict <- as.list(stats::setNames(image_files, relative_paths))
  temp_image_dict <- tempfile(fileext = ".json")
  jsonlite::write_json(image_dict, temp_image_dict, auto_unbox = TRUE, pretty = TRUE)
  log4r::debug(.le$logger, paste("Temporary image dictionary created at:", temp_image_dict))

  script <- system.file("scripts/sync_images.py", package = "presentifyr")
  args <- c("run", script, "-i", input_pptx, "-o", output_pptx, "-d", temp_image_dict)

  if (is.null(getOption("venv_dir"))) {
    log4r::info(.le$logger, "Setting options('venv_dir') to project root")
    message("Setting options('venv_dir') to project root")

    options("venv_dir" = here::here())
  }

  venv_path <- file.path(getOption("venv_dir"), ".venv")

  if (!dir.exists(venv_path)) {
    log4r::error(.le$logger, "Virtual environment not found. Please initialize with initialize_python")
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
    log4r::error(.le$logger, paste0("Sync images Python script failed. Status: ", e$status))
    log4r::error(.le$logger, paste0("Sync images Python script failed. Stderr: ", e$stderr))
    log4r::info(.le$logger, paste0("Sync images Python script failed. Stdout: ", e$stdout))
    stop(paste("Sync images script failed. Status: ", e$status, "Stderr: ", e$stderr))
  })
  log4r::debug(.le$logger, "Exiting sync images R function")
}
