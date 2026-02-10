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

  if (!grepl("\\.pptx$", base_pptx, ignore.case = TRUE)) {
    log4r::error(.le$logger, paste("Invalid file type. Expected a .pptx file:", base_pptx))
    stop("Invalid file type. Expected a .pptx file.")
  }

  log4r::debug(.le$logger, paste("Using provided base_pptx: ", base_pptx))

  args <- c("run", script, "-b", base_pptx, "-o", output_dir)

  paths <- reportifyr::get_venv_uv_paths()
  venv_path <- paths$venv
  uv_path <- paths$uv
  log4r::debug(.le$logger, paste("venv_path resolved to:", venv_path))
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
    pattern = "\\.png$",
    full.names = TRUE
  )

  layout_names <- sub("\\.png$", "", basename(layout_images))  # Extract names without extension

  ## Read layout metadata JSON
  metadata_path <- file.path(output_dir, "layout_metadata.json")
  placeholder_counts <- rep(NA_integer_, length(layout_names))

  if (file.exists(metadata_path)) {
    metadata <- jsonlite::fromJSON(metadata_path)
    for (i in seq_along(layout_names)) {
      layout_key <- layout_names[i]
      if (!is.null(metadata[[layout_key]])) {
        placeholder_counts[i] <- metadata[[layout_key]]$placeholder_count
      }
    }
    log4r::debug(.le$logger, "Loaded layout metadata from JSON")
  } else {
    log4r::warn(.le$logger, "Layout metadata JSON not found")
  }

  sorted_order <- order(layout_names)
  layout_names <- layout_names[sorted_order]
  layout_images <- layout_images[sorted_order]
  placeholder_counts <- placeholder_counts[sorted_order]

  out_df <- data.frame(
    layout_name = layout_names,
    image_path = layout_images,
    placeholder_count = placeholder_counts,
    stringsAsFactors = FALSE
  )

  log4r::debug(.le$logger, "Exiting extract layouts R function")

  return(out_df)
}
