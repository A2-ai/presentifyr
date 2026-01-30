#' Gets the path to uv -- pre v0.5.0 installed to /.cargo/bin post v0.5.0 to /.local/bin
#'
#' @return A character string representing the file path to uv
#' @keywords internal
#' @noRd
get_uv_path <- function() {
  uv_paths <- c(normalizePath("~/.local/bin/uv", mustWork = FALSE),
                normalizePath("~/.cargo/bin/uv", mustWork = FALSE))

  uv_path <- uv_paths[file.exists(uv_paths)][1]

  if (is.null(uv_path)) {
    stop("Please install uv with initialize_python")
  } else {
    return(uv_path)
  }
}

#' Creates a vector of available image file paths
#'
#' @param directory The path to the directory where image files will be searched.
#' @param recursive A logical value. If TRUE, searches for images recursively in subdirectories. Default is TRUE.
#' @param exclude_dirs A vector of directory names to exclude from the search. Default is NULL. If NULL, no directories are excluded.
#'
#' @return A character vector of image file paths.
#' @keywords internal
#' @noRd
parse_directory_for_images <- function(directory,
                                       recursive = TRUE,
                                       exclude_dirs = NULL) {
  log4r::debug(.le$logger, paste("Parsing directory for images:", directory))

  if (!dir.exists(directory)) {
    log4r::error(.le$logger, paste("Directory does not exist:", directory))
    stop("The specified directory does not exist.")
  }

  image_pattern <- include_imgs()

  image_files <- list.files(
    path = directory,
    pattern = image_pattern,
    recursive = recursive,
    full.names = TRUE,
    ignore.case = TRUE
  )

  log4r::debug(.le$logger, paste("Found", length(image_files), "image file(s)."))

  if (!is.null(exclude_dirs) && length(image_files) > 0) {
    exclude_pattern <- paste0(exclude_dirs, collapse = "|")
    image_files <- image_files[!grepl(exclude_pattern, image_files, ignore.case = TRUE)]
    log4r::info(.le$logger, paste("Excluded directories:", paste(exclude_dirs, collapse = ", ")))
    log4r::debug(.le$logger, paste("Remaining", length(image_files), "image file(s) after exclusion."))
  }

  if (length(image_files) == 0) {
    log4r::info(.le$logger, "No image files found in the specified directory.")
  }

  return(image_files)
}

#' Load metadata JSON for an image file
#'
#' @param image_path The path to the image file
#'
#' @return A list containing the metadata, or NULL if not found
#' @keywords internal
#' @noRd
load_image_metadata <- function(image_path) {
  # Construct metadata filename: {name}_{ext}_metadata.json
  file_name <- basename(image_path)
  file_dir <- dirname(image_path)
  name_parts <- tools::file_path_sans_ext(file_name)
  ext <- tools::file_ext(file_name)

  metadata_filename <- paste0(name_parts, "_", ext, "_metadata.json")
  metadata_path <- file.path(file_dir, metadata_filename)

  log4r::debug(.le$logger, paste("Looking for metadata at:", metadata_path))

  if (!file.exists(metadata_path)) {
    log4r::warn(.le$logger, paste("Metadata file not found:", metadata_path))
    return(NULL)
  }

  tryCatch({
    metadata <- jsonlite::read_json(metadata_path)
    log4r::debug(.le$logger, paste("Loaded metadata from:", metadata_path))
    return(metadata)
  }, error = function(e) {
    log4r::error(.le$logger, paste("Error reading metadata file:", metadata_path, "-", e$message))
    return(NULL)
  })
}

#' Format slide notes with metadata
#'
#' @param metadata A list containing the metadata (from load_image_metadata)
#'
#' @return A formatted string for slide notes
#' @keywords internal
#' @noRd
format_slide_notes <- function(metadata) {
  lines <- character()

  # Source: source_meta.path + source_meta.latest_time
  source_path <- metadata$source_meta$path %||% ""
  source_time <- metadata$source_meta$latest_time %||% ""
  if (nzchar(source_path) && nzchar(source_time)) {
    lines <- c(lines, paste0("Source: ", source_path, " ", source_time))
  } else if (nzchar(source_path)) {
    lines <- c(lines, paste0("Source: ", source_path))
  } else {
    lines <- c(lines, "Source: N/A")
  }

  # Notes: object_meta.footnotes.notes (joined with ". ")
  notes_list <- metadata$object_meta$footnotes$notes
  if (length(notes_list) > 0 && !all(notes_list == "")) {
    notes_text <- paste(sapply(notes_list, function(n) {
      if (!endsWith(n, ".")) paste0(n, ".") else n
    }), collapse = " ")
    lines <- c(lines, paste0("Notes: ", notes_text))
  } else {
    lines <- c(lines, "Notes: N/A")
  }

  # Abbreviations: object_meta.footnotes.abbreviations (comma-separated)
  abbrev_list <- metadata$object_meta$footnotes$abbreviations
  if (length(abbrev_list) > 0 && !all(abbrev_list == "")) {
    lines <- c(lines, paste0("Abbreviations: ", paste(abbrev_list, collapse = ", ")))
  } else {
    lines <- c(lines, "Abbreviations: N/A")
  }

  return(paste(lines, collapse = "\n"))
}
