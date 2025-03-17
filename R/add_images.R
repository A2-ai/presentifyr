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
add_images <- function(files, repo_url, output_pptx, slide_layout_name = NULL, base_pptx = NULL) {
  # Logging setup
  log4r::debug(.le$logger, "Starting add_images function")
  log4r::debug(.le$logger, paste("Starting layout name: ", slide_layout_name))

  # if (is.null(slide_layout_index)) {
  #   slide_layout_index <- 8 ## Hard coded for blank presentations
  #   log4r::debug(.le$logger, paste("Slide_layout_index is NULL, setting value to: ", slide_layout_index))
  # }

  # Load PowerPoint template or create a new one
  if (!is.null(base_pptx) && file.exists(base_pptx)) {
    log4r::info(.le$logger, paste("Using base PowerPoint template:", base_pptx))
    ppt <- officer::read_pptx(base_pptx)
  } else {
    log4r::warn(.le$logger, "No valid PowerPoint template found. Creating a blank presentation.")
    ppt <- officer::read_pptx()
  }

  log4r::info(.le$logger, paste("Total images to insert:", length(files)))

  layouts <- officer::layout_summary(ppt)

  log4r::debug(.le$logger, paste("Available layouts: ", layouts$layout))

  if (!is.null(slide_layout_name)) {
    # Normalize layout names: replace underscores with spaces, convert to lowercase, and trim whitespace
    normalized_layout_name <- trimws(tolower(gsub("_", " ", slide_layout_name)))
    normalized_layouts <- trimws(tolower(gsub("_", " ", layouts$layout)))

    # Perform matching with normalized values
    matched_index <- match(normalized_layout_name, normalized_layouts)

    if (is.na(matched_index)) {
      log4r::error(.le$logger, paste("Invalid layout name:", slide_layout_name,
                                     "(normalized:", normalized_layout_name, ") not found in available layouts."))
      stop("Invalid layout name: not found in template")
    }

    slide_layout_index <- matched_index  # Use the matched index
    log4r::debug(.le$logger, paste("Matched layout name to index:", slide_layout_index))
  } else {
    slide_layout_index <- 1  # Default index if NULL
    log4r::info(.le$logger, paste("Slide_layout_name is NULL, setting index value to:", slide_layout_index))
  }

  selected_layout <- layouts$layout[slide_layout_index]
  selected_master <- layouts$master[slide_layout_index]

  for (i in seq_along(files)) {
    file <- files[i]
    file_url <- paste0(repo_url, "/", file)
    sentinel_val <- "{prfy}:"
    alt_text <- paste0(sentinel_val, basename(file))

    log4r::debug(.le$logger, paste("Creating slide", i, "of", length(files), "with image:", file))

    # Add a slide using the selected layout
    ppt <- officer::add_slide(ppt, layout = selected_layout, master = selected_master)

    placeholders <- officer::layout_properties(ppt, selected_layout)

    # Identify a general content placeholder (exclude specific ones like Title)
    content_placeholder <- placeholders$ph_label[grepl("Content Placeholder 2", placeholders$ph_label)]

    # Fill all placeholders with empty text (to make them visible)
    for (ph_label in placeholders$ph_label) {
      if (ph_label != content_placeholder) {
        ppt <- officer::ph_with(ppt, value = "", location = officer::ph_location_label(ph_label = ph_label))
        log4r::debug(.le$logger, paste("Inserted blank text into placeholder:", ph_label))
      }
    }

    # Insert the image in the target content placeholder
    if (length(content_placeholder) > 0) {
      ppt <- officer::ph_with(ppt, value = officer::external_img(file, alt = alt_text), location = officer::ph_location_label(ph_label = content_placeholder))
      log4r::debug(.le$logger, paste("Image inserted into placeholder for slide", i))
    } else {
      log4r::warn(.le$logger, paste("No content placeholder found on slide", i, ". Skipping image placement."))
    }

    # Add file URL to the slide notes
    ppt <- officer::set_notes(ppt, value = file_url, location = officer::notes_location_type("body"))
    log4r::debug(.le$logger, paste("Added file URL to notes for slide", i, ":", file_url))
  }

  # Save the PowerPoint
  print(ppt, target = output_pptx)
  message(sprintf("PowerPoint saved as %s", output_pptx))
  log4r::info(.le$logger, paste("PowerPoint saved as", output_pptx))
}
