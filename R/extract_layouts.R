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
  log4r::debug(.le$logger, paste0("extract_layouts: input=", base_pptx, ", output_dir=", output_dir))

  script <- system.file("scripts/extract_layouts.py", package = "presentifyr")

  validate_pptx_file(base_pptx)

  result <- run_python_script(
    script_args = c(script, "-b", base_pptx, "-o", output_dir),
    label = "Extract layouts"
  )

  layout_images <- list.files(
    output_dir,
    pattern = "\\.png$",
    full.names = TRUE
  )

  layout_names <- sub("\\.png$", "", basename(layout_images))  # Extract names without extension

  ## Read layout metadata JSON
  metadata_path <- file.path(output_dir, "layout_metadata.json")
  placeholder_counts <- rep(NA_integer_, length(layout_names))
  original_names <- layout_names  ## Default to safe name if metadata missing

  if (file.exists(metadata_path)) {
    metadata <- jsonlite::fromJSON(metadata_path)
    for (i in seq_along(layout_names)) {
      layout_key <- layout_names[i]
      if (!is.null(metadata[[layout_key]])) {
        placeholder_counts[i] <- metadata[[layout_key]]$placeholder_count
        original_names[i] <- metadata[[layout_key]]$layout_name %||% layout_names[i]
      }
    }
    log4r::debug(.le$logger, paste("extract_layouts: loaded metadata from", metadata_path))
  } else {
    log4r::warn(.le$logger, paste("extract_layouts: metadata JSON not found at", metadata_path))
  }

  sorted_order <- order(layout_names)
  layout_names <- layout_names[sorted_order]
  layout_images <- layout_images[sorted_order]
  placeholder_counts <- placeholder_counts[sorted_order]
  original_names <- original_names[sorted_order]

  out_df <- data.frame(
    layout_name = layout_names,
    original_name = original_names,
    image_path = layout_images,
    placeholder_count = placeholder_counts,
    stringsAsFactors = FALSE
  )

  log4r::debug(.le$logger, paste0("extract_layouts: complete, ", nrow(out_df), " layouts found"))

  return(out_df)
}
