#' Extracts layouts as images from a given PowerPoint presentation (.pptx) file.
#'
#' @param base_pptx The file path to an existing .pptx file that serves as a template.
#' @param output_dir The file path where the layout .pngs will be saved.
#'
#' @keywords internal
#'
#' @examples \dontrun{
#' extract_layouts(base_pptx = base_pptx)
#' }
extract_layouts <- function(base_pptx,
                            output_dir) {
  log4r::debug(.le$logger, "Starting extract layouts R function")

  script <- system.file("scripts/extract_layouts.py", package = "presentifyr")

  if (!file.exists(base_pptx)) {
    log4r::error(.le$logger, paste("The input base_pptx does not exist:", base_pptx))
    stop("Base PowerPoint file not found")
  }
  log4r::debug(.le$logger, paste("Using provided base_pptx: ", base_pptx))

  args <- c("run", script, "-b", base_pptx, "-o", output_dir)

  if (is.null(getOption("venv_dir"))) {
    log4r::info(.le$logger, "Setting options('venv_dir') to project root")
    message("Setting options('venv_dir') to project root.")

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
    log4r::error(.le$logger, paste0("Extract layouts Python script failed. Status: ", e$status))
    log4r::error(.le$logger, paste0("Extract layouts Python script failed. Stderr: ", e$stderr))
    log4r::info(.le$logger, paste0("Extract layouts Python script failed. Stdout: ", e$stdout))
    stop(paste("Extract layouts Python script failed. Status: ", e$status, "Stderr: ", e$stderr))
  })

  layout_images <- list.files(
    output_dir,
    pattern = "^layout_\\d+\\.png$",
    full.names = TRUE
  )

  parsed_indices <- as.integer(sub("layout_(\\d+)\\.png", "\\1", basename(layout_images)))

  out_df <- data.frame(
    index = parsed_indices,
    image_path = layout_images,
    stringsAsFactors = FALSE
  )

  log4r::debug(.le$logger, "Exiting extract layouts R function")

  return(out_df)
}
