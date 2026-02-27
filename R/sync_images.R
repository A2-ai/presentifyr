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

  validate_pptx_file(input_pptx)

  exclude_dirs <- default_exclude_dirs()

  root_dir <- get_project_dir()
  log4r::debug(.le$logger, paste("Scanning for images in:", root_dir))

  image_files <- parse_directory_for_images(
    directory = root_dir,
    exclude_dirs = exclude_dirs ## Exclude unnecessary directories
  )

  if (length(image_files) == 0 || all(image_files == "")) {
    stop("No image files found. Execution halted.")
  }

  log4r::debug(.le$logger, paste0("Found ", length(image_files), " image files"))

  ## Build keys matching alt-text: prefer project-relative paths; include basename for backward compatibility
  keys_primary <- vapply(image_files, prfy_image_key, character(1), root = root_dir)
  keys_legacy <- basename(image_files)

  image_dict <- as.list(stats::setNames(image_files, keys_primary))
  temp_image_dict <- tempfile(fileext = ".json")
  jsonlite::write_json(image_dict, temp_image_dict, auto_unbox = TRUE, pretty = TRUE)
  log4r::debug(.le$logger, paste("Temporary image dictionary created at:", temp_image_dict))

  script <- system.file("scripts/sync_images.py", package = "presentifyr")

  result <- run_python_script(
    script_args = c(script, "-i", input_pptx, "-o", output_pptx, "-d", temp_image_dict),
    label = "Sync images"
  )
  log4r::debug(.le$logger, "Exiting sync images R function")
}
