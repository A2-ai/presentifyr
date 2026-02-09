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
                       slide_groups = NULL, slide_positions = NULL) {

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

  ## Match Content Placeholder or Picture Placeholder labels
  usable_ph_mask <- grepl("Content Placeholder|Picture Placeholder", placeholders$ph_label)
  usable_placeholders_df <- placeholders[usable_ph_mask, ]

  if (nrow(usable_placeholders_df) == 0) {
    log4r::error(.le$logger, paste("No usable placeholder found (Content or Picture)"))
    stop("No usable placeholder found (Content Placeholder or Picture Placeholder)")
  }

  ## Sort placeholders by position: top-to-bottom, then left-to-right
  usable_placeholders_df <- usable_placeholders_df[
    order(usable_placeholders_df$offy, usable_placeholders_df$offx),
  ]

  log4r::debug(.le$logger, paste("Found", nrow(usable_placeholders_df), "usable placeholder(s):",
                                  paste(usable_placeholders_df$ph_label, collapse = ", ")))

  ## Determine slide groups
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

  log4r::info(.le$logger, paste("Total slides to create:", length(slide_groups)))

  for (slide_idx in seq_along(slide_groups)) {
    slide_files <- slide_groups[[slide_idx]]
    slide_pos <- slide_positions[[slide_idx]]
    if (!is.character(slide_files)) slide_files <- as.character(slide_files)

    log4r::debug(.le$logger, paste("Creating slide", slide_idx, "of", length(slide_groups),
                                   "with", length(slide_files), "image(s)"))

    ppt <- officer::add_slide(ppt, layout = selected_layout, master = selected_master)

    ## Clear non-image placeholders (title, etc.) but keep usable ones for images
    for (ph_label in placeholders$ph_label) {
      if (!ph_label %in% usable_placeholders_df$ph_label &&
          !grepl("Slide Number Placeholder", ph_label)) {
        ppt <- officer::ph_with(
          ppt,
          value = "",
          location = officer::ph_location_label(ph_label = ph_label)
        )
      }
    }

    ## Collect metadata from all images on this slide for combined notes
    slide_metadata_list <- list()

    ## Place each image in corresponding placeholder
    for (img_idx in seq_along(slide_files)) {
      file <- slide_files[img_idx]

      ## Get placeholder for this image based on specified position
      target_pos <- slide_pos[img_idx]
      ## Clamp to valid range
      ph_row_idx <- min(max(target_pos, 1), nrow(usable_placeholders_df))
      ph_info <- usable_placeholders_df[ph_row_idx, ]

      ph_left   <- ph_info$offx
      ph_top    <- ph_info$offy
      ph_width  <- ph_info$cx
      ph_height <- ph_info$cy

      log4r::debug(.le$logger, paste("Placing image", img_idx, "in placeholder:", ph_info$ph_label))
      log4r::debug(.le$logger, paste("Placeholder bounding box:",
                                     "left=", ph_left, "top=", ph_top,
                                     "width=", ph_width, "height=", ph_height))

      alt_text_key <- prfy_image_key(file)
      alt_text <- paste0("{prfy}:", alt_text_key)

      img <- tryCatch({
        magick::image_read(file)
      }, error = function(e) {
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
      log4r::debug(.le$logger, paste("Image inserted for slide", slide_idx))

      ## Collect metadata for this image
      metadata <- load_image_metadata(file)
      if (!is.null(metadata)) {
        slide_metadata_list[[basename(file)]] <- metadata
      }
    }

    ## Combine metadata from all images into slide notes
    if (length(slide_metadata_list) > 0) {
      combined_notes <- paste(
        sapply(names(slide_metadata_list), function(name) {
          paste0("## ", name, "\n", format_slide_notes(slide_metadata_list[[name]]))
        }),
        collapse = "\n\n"
      )
      ppt <- officer::set_notes(ppt, value = combined_notes, location = officer::notes_location_type("body"))
      log4r::debug(.le$logger, paste("Added combined notes for slide", slide_idx))
    }
  }
  print(ppt, target = output_pptx)
  message(sprintf("PowerPoint saved as %s", output_pptx))
  log4r::info(.le$logger, paste("PowerPoint saved as", output_pptx))
}
