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
  log4r::debug(.le$logger, paste0("sync_images: input=", input_pptx, ", output=", output_pptx))

  validate_pptx_file(input_pptx)

  exclude_dirs <- default_exclude_dirs()

  root_dir <- get_project_dir()
  log4r::debug(.le$logger, paste("sync_images: scanning for images in", root_dir))

  image_files <- parse_directory_for_images(
    directory = root_dir,
    exclude_dirs = exclude_dirs ## Exclude unnecessary directories
  )

  if (length(image_files) == 0 || all(image_files == "")) {
    stop("No image files found. Execution halted.")
  }

  log4r::debug(.le$logger, paste0("sync_images: found ", length(image_files), " image files"))

  ## Build keys matching alt-text: prefer project-relative paths; include basename for backward compatibility
  keys_primary <- vapply(image_files, prfy_image_key, character(1), root = root_dir)
  keys_legacy <- basename(image_files)

  image_dict <- as.list(stats::setNames(image_files, keys_primary))
  temp_image_dict <- tempfile(fileext = ".json")
  jsonlite::write_json(
    image_dict, temp_image_dict,
    auto_unbox = TRUE, pretty = TRUE
  )
  log4r::debug(.le$logger, paste(
    "sync_images: image dictionary written to", temp_image_dict
  ))

  abbrev_defs <- load_abbreviation_definitions()
  temp_abbrev <- tempfile(fileext = ".json")
  jsonlite::write_json(
    abbrev_defs, temp_abbrev,
    auto_unbox = TRUE, pretty = TRUE
  )

  figure_footnotes <- load_figure_footnotes()
  temp_fig_fn <- tempfile(fileext = ".json")
  jsonlite::write_json(
    figure_footnotes, temp_fig_fn,
    auto_unbox = TRUE, pretty = TRUE
  )

  script <- system.file(
    "scripts/sync_images.py", package = "presentifyr"
  )

  result <- run_python_script(
    script_args = c(
      script, "-i", input_pptx, "-o", output_pptx,
      "-d", temp_image_dict, "-a", temp_abbrev,
      "-f", temp_fig_fn
    ),
    label = "Sync images"
  )
  log4r::debug(.le$logger, paste("sync_images: complete, output written to", output_pptx))
}
