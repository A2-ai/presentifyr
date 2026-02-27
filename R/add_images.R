#' Adds images from file to a given PowerPoint presentation (.pptx) file.
#'
#' @param files A list of image file paths to be inserted into the .pptx file.
#'   Used when slide_groups is NULL (one image per slide).
#' @param output_pptx The file path where the modified .pptx file will be saved.
#' @param slide_layout_name A character string indicating the name of the PowerPoint slide layout to be used. Default is NULL. If NULL, slide_layout_index is set to 1.
#' @param base_pptx The file path to an existing .pptx file that serves as a template. Default is NULL. If NULL, a blank presentation is used.
#' @param slide_groups A list of character vectors, where each vector contains file paths
#'   for images to be placed on a single slide. If provided, \code{files} is ignored.
#'   Images are placed in placeholders in order (top-left to bottom-right).
#' @param slide_positions A list of integer vectors parallel to slide_groups, indicating
#'   which placeholder (1-indexed) each image should be placed in. If NULL, images are
#'   placed in placeholders sequentially (1, 2, 3...).
#'
#' @keywords internal
#'
#' @examples \dontrun{
#' add_images(
#'   files = files,
#'   base_pptx = NULL,
#'   output_pptx = output_pptx
#' )
#' }
add_images <- function(files, output_pptx,
                       slide_layout_name = NULL, base_pptx = NULL,
                       slide_groups = NULL, slide_positions = NULL,
                       font_settings = list()) {

  log4r::debug(.le$logger, "Starting add_images function")

  ## Determine slide groups (backward compat logic stays in R)
  if (is.null(slide_groups)) {
    ## Backward compatibility: one image per slide
    slide_groups <- as.list(files)
    slide_positions <- lapply(slide_groups, function(x) seq_along(x))
    log4r::debug(.le$logger, "Using single-image mode (one image per slide)")
  } else {
    ## If positions not provided, default to sequential
    if (is.null(slide_positions)) {
      slide_positions <- lapply(slide_groups, function(x) seq_along(x))
    }
    log4r::debug(.le$logger, paste("Using grouped mode:", length(slide_groups), "slides"))
  }

  ## Pre-compute image keys via prfy_image_key()
  all_files <- unique(unlist(slide_groups))
  image_keys <- stats::setNames(
    vapply(all_files, prfy_image_key, character(1)),
    all_files
  )

  ## Build config for Python script
  config <- list(
    slide_layout_name = slide_layout_name,
    slide_groups = slide_groups,
    slide_positions = slide_positions,
    font_settings = font_settings,
    image_keys = as.list(image_keys)
  )

  temp_config <- tempfile(fileext = ".json")
  jsonlite::write_json(config, temp_config, auto_unbox = TRUE, pretty = TRUE, null = "null")
  log4r::debug(.le$logger, paste("Config JSON written to:", temp_config))

  script <- system.file("scripts/add_images.py", package = "presentifyr")
  args <- c("run", script, "-o", output_pptx, "-c", temp_config)

  if (!is.null(base_pptx) && file.exists(base_pptx)) {
    args <- c(args, "-b", base_pptx)
  }

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
      echo = TRUE
    )
  }, error = function(e) {
    log4r::error(.le$logger, paste0("Add images Python script failed. Status: ", e$status))
    log4r::error(.le$logger, paste0("Add images Python script failed. Stderr: ", e$stderr))
    log4r::info(.le$logger, paste0("Add images Python script failed. Stdout: ", e$stdout))
    stop(paste("Add images script failed. Status: ", e$status, "Stderr: ", e$stderr))
  })

  message(sprintf("PowerPoint saved as %s", output_pptx))
  log4r::info(.le$logger, paste("PowerPoint saved as", output_pptx))
}
