#' Adds images from file to a given PowerPoint presentation (.pptx) file.
#'
#' @param files A list of image file paths to be inserted into the .pptx file.
#' @param repo_url The url of the repository where an image is stored.
#' @param output_pptx The file path where the modified .pptx file will be saved.
#' @param slide_layout_name A character string indicating the name of the PowerPoint slide layout to be used. Default is NULL. If NULL, slide_layout_index is set to 1.
#' @param base_pptx The file path to an existing .pptx file that serves as a template. Default is NULL. If NULL, a blank presentation is used.
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
add_images <- function(files, repo_url, output_pptx,
                       slide_layout_name = NULL, base_pptx = NULL) {

  log4r::debug(.le$logger, "Starting add_images function")

  if (!is.null(base_pptx) && file.exists(base_pptx)) {
    log4r::info(.le$logger, paste("Using base PowerPoint template:", base_pptx))
    ppt <- officer::read_pptx(base_pptx)
  } else {
    log4r::warn(.le$logger, "No valid PowerPoint template found. Creating a blank presentation.")
    ppt <- officer::read_pptx()
  }

  layouts <- officer::layout_summary(ppt)
  log4r::debug(.le$logger, paste("Available layouts:", paste(layouts$layout, collapse = ", ")))

  if (!is.null(slide_layout_name)) {
    log4r::debug(.le$logger, paste("Name of slide layout being used:", slide_layout_name))
    normalized_layout_name <- trimws(tolower(gsub("_", " ", slide_layout_name)))
    normalized_layouts <- trimws(tolower(gsub("_", " ", layouts$layout)))
    matched_index <- match(normalized_layout_name, normalized_layouts)

    if (is.na(matched_index)) {
      log4r::error(.le$logger, paste("Invalid layout name: ", slide_layout_name))
      stop("Invalid layout name '", slide_layout_name, "' not found in template.")
    }
    slide_layout_index <- matched_index
  } else {
    slide_layout_index <- 2 ## Default for officer::read_pptx() as of 0.6.6
  }

  selected_layout <- layouts$layout[slide_layout_index]
  selected_master <- layouts$master[slide_layout_index]
  log4r::debug(.le$logger, paste("Selected layout:", selected_layout, "on master:", selected_master))

  placeholders <- officer::layout_properties(ppt, selected_layout)

  content_placeholder <- placeholders$ph_label[grepl("Content Placeholder 2", placeholders$ph_label)]

  if (length(content_placeholder) == 0) {
    log4r::error(.le$logger, paste("No content placeholder found"))
    stop("No content placeholder found")
  }

  ## Extract bounding box (x=left, y=top, cx=width, cy=height in inches)
  ph_info <- placeholders[placeholders$ph_label == content_placeholder, ]
  ph_left   <- ph_info$offx[1]
  ph_top    <- ph_info$offy[1]
  ph_width  <- ph_info$cx[1]
  ph_height <- ph_info$cy[1]

  log4r::info(.le$logger, paste("Total images to insert:", length(files)))

  for (i in seq_along(files)) {
    file <- files[i]
    file_url <- paste0(repo_url, "/", file)
    alt_text <- paste0("{prfy}:", basename(file))

    log4r::debug(.le$logger, paste("Creating slide", i, "of", length(files), "with image:", file))
    ppt <- officer::add_slide(ppt, layout = selected_layout, master = selected_master)

    for (ph_label in placeholders$ph_label) {
      if (ph_label != content_placeholder &&
          !grepl("Slide Number Placeholder", ph_label)) {

        ppt <- officer::ph_with(
          ppt,
          value = "",
          location = officer::ph_location_label(ph_label = ph_label)
        )
      }
    }

    log4r::debug(.le$logger, paste("Placeholder bounding box:",
                                   "left=", ph_left, "top=", ph_top,
                                   "width=", ph_width, "height=", ph_height))

    img <- tryCatch({
      magick::image_read(file)
      },error = function(e) {
        log4r::error(.le$logger, paste("Error reading file:", file, "-", e$message))
        stop(e)
      })

    info <- magick::image_info(img)
    width_px  <- info$width
    height_px <- info$height

    x_res <- 300 ## DPI used by PowerPoint auto-scaling
    y_res <- 300 ## DPI used by PowerPoint auto-scaling

    raw_width_in  <- width_px  / x_res
    raw_height_in <- height_px / y_res

    scale_factor <- min(ph_width / raw_width_in, ph_height / raw_height_in)
    final_w <- raw_width_in  * scale_factor
    final_h <- raw_height_in * scale_factor

    log4r::debug(.le$logger, paste("Scaled image size:",
                                   round(final_w, 2), "x", round(final_h, 2), "inches"))

    centered_left <- ph_left + (ph_width  - final_w) / 2
    centered_top  <- ph_top  + (ph_height - final_h) / 2

    ppt <- officer::ph_with(
      ppt,
      value = officer::external_img(file, width = final_w, height = final_h, alt = alt_text),
      location = officer::ph_location(
        left   = centered_left,
        top    = centered_top,
        width  = final_w,
        height = final_h
      ),
      use_loc_size = FALSE
    )
    log4r::debug(.le$logger, paste("Image inserted, centered in placeholder for slide", i))

    # Load metadata and format slide notes
    metadata <- load_image_metadata(file)
    if (!is.null(metadata)) {
      notes_text <- format_slide_notes(metadata, file_url)
      log4r::debug(.le$logger, paste("Formatted notes with metadata for:", file))
    } else {
      notes_text <- file_url
      log4r::debug(.le$logger, paste("Using URL-only notes (no metadata) for:", file))
    }

    ppt <- officer::set_notes(ppt, value = notes_text, location = officer::notes_location_type("body"))
  }
  print(ppt, target = output_pptx)
  message(sprintf("PowerPoint saved as %s", output_pptx))
  log4r::info(.le$logger, paste("PowerPoint saved as", output_pptx))
}
