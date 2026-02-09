#' Gets the path to uv -- pre v0.5.0 installed to /.cargo/bin post v0.5.0 to /.local/bin
#'
#' @return A character string representing the file path to uv
#' @keywords internal
#' @noRd
get_uv_path <- function() {
  ## Check common user install locations first
  uv_paths <- c(normalizePath("~/.local/bin/uv", mustWork = FALSE),
                normalizePath("~/.cargo/bin/uv", mustWork = FALSE))

  uv_path <- uv_paths[file.exists(uv_paths)][1]

  ## If not found in user locations, check system PATH
  if (is.na(uv_path) || is.null(uv_path)) {
    uv_path <- Sys.which("uv")
    if (uv_path == "") {
      uv_path <- NA
    }
  }

  if (is.na(uv_path)) {
    stop("Please install uv with initialize_python or ensure uv is in your PATH")
  } else {
    return(uv_path)
  }
}

#' Default directories to ignore when scanning for images
#'
#' Priority: options("presentifyr.exclude_dirs") > env PRFY_EXCLUDE_DIRS (colon/semicolon/comma
#' separated) > built-in defaults. Use character(0) to disable exclusions.
#'
#' @keywords internal
#' @noRd
default_exclude_dirs <- function() {
  builtin <- c(
    "renv", "rv", "rv/library", ".git", ".hg", ".svn",
    "node_modules", ".Rproj.user", ".venv", ".direnv",
    "__pycache__", "env", "site-library", ".cache"
  )

  env_val <- Sys.getenv("PRFY_EXCLUDE_DIRS", unset = "")
  if (nzchar(env_val)) {
    parts <- unlist(strsplit(env_val, "[;:,]", perl = TRUE))
    builtin <- parts
  }

  opt_val <- getOption("presentifyr.exclude_dirs")
  if (!is.null(opt_val)) {
    builtin <- opt_val
  }

  unique(builtin[nzchar(builtin)])
}

#' Key used in alt-text and sync mapping for images
#'
#' Prefer path relative to project root when available to avoid basename collisions.
#' Falls back to basename when the file is outside the root or relative computation fails.
#'
#' @keywords internal
#' @noRd
prfy_image_key <- function(file_path, root = getOption("project.dir", default = here::here())) {
  file_path <- fs::path_abs(file_path)
  root <- fs::path_abs(root)

  rel <- tryCatch(fs::path_rel(file_path, start = root), error = function(e) NA_character_)

  key <- if (!is.na(rel) && !startsWith(rel, "..")) rel else basename(file_path)
  # Normalize separators for portability
  gsub("\\\\", "/", key)
}

#' Creates a vector of available image file paths
#'
#' @param directory The path to the directory where image files will be searched.
#' @param recursive A logical value. If TRUE, searches for images recursively in subdirectories. Default is TRUE.
#' @param exclude_dirs A vector of directory names to exclude from the search. Default pulls from
#'   options/env via default_exclude_dirs(). Use character(0) to disable exclusions.
#'
#' @return A character vector of image file paths.
#' @keywords internal
#' @noRd
parse_directory_for_images <- function(directory,
                                       recursive = TRUE,
                                       exclude_dirs = default_exclude_dirs()) {
  log4r::debug(.le$logger, paste("Parsing directory for images:", directory))

  if (!dir.exists(directory)) {
    log4r::error(.le$logger, paste("Directory does not exist:", directory))
    stop("The specified directory does not exist.")
  }

  image_pattern <- include_imgs()

  # helper: check if any path segment matches excluded dir names
  should_skip <- function(rel_path) {
    segments <- strsplit(rel_path, .Platform$file.sep, fixed = FALSE)[[1]]
    any(segments %in% exclude_dirs)
  }

  image_files <- character(0)

  if (!recursive) {
    files_here <- fs::dir_ls(path = directory, recurse = FALSE, type = "file", glob = NULL, fail = FALSE)
    rel_files <- fs::path_rel(files_here, directory)
    keep <- vapply(rel_files, function(p) !should_skip(p), logical(1))
    image_files <- files_here[keep & grepl(image_pattern, files_here, ignore.case = TRUE)]
  } else {
    queue <- c(directory)
    visited_dirs <- 0L
    skipped_dirs <- 0L

    while (length(queue) > 0) {
      current_dir <- queue[[1]]
      queue <- queue[-1]
      visited_dirs <- visited_dirs + 1L

      entries <- fs::dir_ls(path = current_dir, recurse = FALSE, type = "any", fail = FALSE)
      if (length(entries) == 0) next

      dirs <- entries[fs::is_dir(entries)]
      files <- entries[fs::is_file(entries)]

      if (length(files) > 0) {
        rel_files <- fs::path_rel(files, directory)
        keep <- vapply(rel_files, function(p) !should_skip(p), logical(1))
        image_files <- c(image_files, files[keep & grepl(image_pattern, files, ignore.case = TRUE)])
      }

      if (length(dirs) > 0) {
        for (d in dirs) {
          rel_dir <- fs::path_rel(d, directory)
          if (should_skip(rel_dir)) {
            skipped_dirs <- skipped_dirs + 1L
            next
          }
          queue <- c(queue, d)
        }
      }
    }

    log4r::debug(.le$logger, paste("Visited", visited_dirs, "directories; skipped", skipped_dirs, "excluded directories"))
  }

  log4r::debug(.le$logger, paste("Found", length(image_files), "image file(s)."))

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
