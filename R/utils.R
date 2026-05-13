#' Get Project Root
#'
#' Returns the directory of the current project
#'
#' @return Project directory
#' @export
get_project_dir <- function() {
  getOption(
    "project.dir",
    default = here::here()
  )
}

#' Validate that a file exists and has a .pptx extension
#'
#' @param file_path Path to the file to validate
#'
#' @keywords internal
#' @noRd
validate_pptx_file <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("The input .pptx file does not exist: ", file_path)
  }
  if (!grepl("\\.pptx$", file_path, ignore.case = TRUE)) {
    stop("Invalid file type. Expected a .pptx file.")
  }
}

#' Run a Python script via uv using the fyrstartr-managed venv
#'
#' Thin wrapper over [fyrstartr::run_python_script()] that resolves the
#' venv/uv paths, exposes presentifyr's `inst/scripts/` on `PYTHONPATH`,
#' and surfaces a clean error message without leaking subprocess paths.
#'
#' @param script_args Character vector of arguments to pass after "uv run".
#'   Typically c(script_path, "-flag", value, ...).
#' @param label Short label for error messages (e.g. "Add images", "Sync images").
#'
#' @return The result from [fyrstartr::run_python_script()].
#' @keywords internal
#' @noRd
run_python_script <- function(script_args, label) {
  paths <- fyrstartr::get_venv_uv_paths()
  venv_path <- paths$venv
  uv_path <- paths$uv
  log4r::debug(.le$logger, paste("run_python_script: venv =", venv_path))
  log4r::debug(.le$logger, paste("run_python_script: uv =", uv_path))

  ## Python emits every level to stderr; this callback parses the [LEVEL]
  ## tag and filters console output by PRFY_VERBOSE. Lines without a tag
  ## (e.g. raw tracebacks) always pass through.
  py_levels <- c(
    "DEBUG" = 1, "INFO" = 2, "WARNING" = 3, "ERROR" = 4, "CRITICAL" = 5
  )
  r_levels <- c(
    "DEBUG" = 1, "INFO" = 2, "WARN" = 3, "ERROR" = 4, "FATAL" = 5
  )
  threshold <- r_levels[[Sys.getenv("PRFY_VERBOSE", unset = "WARN")]]

  py_callback <- function(chunk, proc) {
    lines <- strsplit(chunk, "\n")[[1]]
    for (line in lines) {
      line <- trimws(line)
      if (nchar(line) == 0) next

      show <- TRUE
      level_match <- regmatches(
        line, regexpr("\\[(DEBUG|INFO|WARNING|ERROR|CRITICAL)\\]", line)
      )
      if (length(level_match) == 1) {
        level <- gsub("\\[|\\]", "", level_match)
        show <- py_levels[[level]] >= threshold
      }
      if (show) cat(line, "\n")
    }
  }

  tryCatch(
    fyrstartr::run_python_script(
      uv_path = uv_path,
      args = c("run", script_args),
      venv_path = venv_path,
      script_name = label,
      pythonpath = system.file("scripts", package = "presentifyr"),
      stderr_callback = py_callback
    ),
    error = function(e) {
      log4r::error(.le$logger, paste0(label, " Python script failed: ", e$message))
      stop(paste0(label, " failed. Set PRFY_VERBOSE=DEBUG for details."))
    }
  )
}

#' Show a standardized error modal in the Shiny UI
#'
#' @param message The error message to display
#'
#' @keywords internal
#' @noRd
show_error_modal <- function(message) {
  shiny::showModal(shiny::modalDialog(
    title = "Error",
    message,
    footer = shiny::modalButton("Close")
  ))
}

#' Default directories to ignore when scanning for images
#'
#' Priority: options("presentifyr.exclude_dirs") > env PRFY_EXCLUDE_DIRS (colon/semicolon/comma
#' separated) > built-in defaults. Use character(0) to disable exclusions.
#'
#' @keywords internal
#' @noRd
default_exclude_dirs <- function() {
  exclude_dirs <- c(
    "renv", "rv", "rv/library", ".git", ".hg", ".svn",
    "node_modules", ".Rproj.user", ".venv", ".direnv",
    "__pycache__", "env", "site-library", ".cache"
  )

  env_val <- Sys.getenv("PRFY_EXCLUDE_DIRS", unset = "")
  if (nzchar(env_val)) {
    exclude_dirs <- unlist(strsplit(env_val, "[;:,]", perl = TRUE))
  }

  opt_val <- getOption("presentifyr.exclude_dirs")
  if (!is.null(opt_val)) {
    exclude_dirs <- opt_val
  }

  unique(exclude_dirs[nzchar(exclude_dirs)])
}

#' Key used in alt-text and sync mapping for images
#'
#' Prefer path relative to project root when available to avoid basename collisions.
#' Falls back to basename when the file is outside the root or relative computation fails.
#'
#' @keywords internal
#' @noRd
prfy_image_key <- function(file_path, root = get_project_dir()) {
  file_path <- fs::path_abs(file_path)
  root <- fs::path_abs(root)

  rel <- tryCatch(fs::path_rel(file_path, start = root), error = function(e) NA_character_)

  key <- if (!is.na(rel) && !startsWith(rel, "..")) rel else basename(file_path)
  # Normalize separators for portability
  as.character(gsub("\\\\", "/", key))
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
  log4r::debug(.le$logger, paste0("parse_directory_for_images: directory=", directory, ", recursive=", recursive))

  if (!dir.exists(directory)) {
    stop("The specified directory does not exist.")
  }

  image_pattern <- include_imgs()

  # helper: check if any path segment matches excluded dir names
  should_skip <- function(rel_path) {
    segments <- strsplit(rel_path, .Platform$file.sep, fixed = TRUE)[[1]]
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

    log4r::debug(.le$logger, paste0("parse_directory_for_images: visited ", visited_dirs, " dirs, skipped ", skipped_dirs, " excluded"))
  }

  log4r::debug(.le$logger, paste0("parse_directory_for_images: found ", length(image_files), " image(s) in ", directory))

  return(image_files)
}

#' Get the configured report directory for this project
#'
#' Reads the `.{report_dir}_init.json` file written by
#' `reportifyr::initialize_report_project()` to find the configured
#' report directory name. Falls back to `<project>/report` when no
#' init file is present or readable.
#'
#' @return Absolute path to the report directory.
#' @keywords internal
#' @noRd
get_report_dir <- function() {
  project_dir <- get_project_dir()
  init_files <- list.files(
    project_dir,
    pattern = "^\\..+_init\\.json$",
    full.names = TRUE,
    all.files = TRUE
  )

  if (length(init_files) == 0) {
    return(file.path(project_dir, "report"))
  }

  if (length(init_files) > 1) {
    log4r::warn(.le$logger, paste0(
      "get_report_dir: multiple init files found, using ",
      basename(init_files[1])
    ))
  }

  tryCatch({
    init <- jsonlite::read_json(init_files[1], simplifyVector = TRUE)
    report_dir_name <- init$config$report_dir_name %||% "report"
    file.path(project_dir, report_dir_name)
  }, error = function(e) {
    log4r::warn(.le$logger, paste0(
      "get_report_dir: failed to parse ",
      basename(init_files[1]), " - ", e$message
    ))
    file.path(project_dir, "report")
  })
}

#' Load abbreviation definitions from a YAML file
#'
#' @param yaml_path The file path to the abbreviations YAML. Default is
#'   NULL. If NULL, uses `<report_dir>/standard_footnotes.yaml` where
#'   `report_dir` is discovered from the reportifyr init file.
#'
#' @return A named list mapping abbreviation keys to full forms, or an
#'   empty list if the YAML is missing or malformed.
#' @keywords internal
#' @noRd
load_abbreviation_definitions <- function(yaml_path = NULL) {
  if (is.null(yaml_path)) {
    yaml_path <- file.path(get_report_dir(), "standard_footnotes.yaml")
  }

  if (!nzchar(yaml_path) || !file.exists(yaml_path)) {
    log4r::warn(.le$logger, paste0(
      "load_abbreviation_definitions: no abbreviations YAML at ",
      yaml_path,
      " - raw abbreviation keys will be used. ",
      "Create this file to enable decoding."
    ))
    return(list())
  }

  tryCatch({
    yaml_content <- yaml::read_yaml(yaml_path)
    abbrevs <- yaml_content$abbreviations
    if (is.null(abbrevs)) {
      log4r::warn(
        .le$logger,
        "load_abbreviation_definitions: no abbreviations in YAML"
      )
      return(list()) # nolint
    }
    log4r::debug(.le$logger, paste(
      "load_abbreviation_definitions: loaded", length(abbrevs), "abbreviations"
    ))
    abbrevs
  }, error = function(e) {
    log4r::warn(.le$logger, paste(
      "load_abbreviation_definitions: YAML parse error -",
      e$message
    ))
    list()
  })
}

#' Load figure_footnotes (meta_type definitions) from a YAML file
#'
#' @description Mirrors reportifyr's `figure_footnotes` lookup:
#'   `object_meta$meta_type` is a key into this dict, and the resolved
#'   text is prepended to the Notes line. Presentifyr only renders
#'   images (the figure path), so only the `figure_footnotes` section
#'   is read.
#'
#' @param yaml_path The file path to the YAML. Default is NULL. If
#'   NULL, uses `<report_dir>/standard_footnotes.yaml`.
#'
#' @return A named list mapping meta_type keys to footnote text, or
#'   an empty list if the YAML is missing or has no figure_footnotes.
#' @keywords internal
#' @noRd
load_figure_footnotes <- function(yaml_path = NULL) {
  if (is.null(yaml_path)) {
    yaml_path <- file.path(get_report_dir(), "standard_footnotes.yaml")
  }

  if (!nzchar(yaml_path) || !file.exists(yaml_path)) {
    return(list())
  }

  tryCatch({
    yaml_content <- yaml::read_yaml(yaml_path)
    yaml_content$figure_footnotes %||% list()
  }, error = function(e) {
    log4r::warn(.le$logger, paste(
      "load_figure_footnotes: YAML parse error -", e$message
    ))
    list()
  })
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

  log4r::debug(.le$logger, paste("load_image_metadata: looking for", metadata_path))

  if (!file.exists(metadata_path)) {
    log4r::debug(.le$logger, paste("load_image_metadata: not found", metadata_path))
    return(NULL)
  }

  tryCatch({
    metadata <- jsonlite::read_json(metadata_path)
    log4r::debug(.le$logger, paste("load_image_metadata: loaded", metadata_path))
    return(metadata)
  }, error = function(e) {
    log4r::warn(.le$logger, paste("load_image_metadata: failed to parse", metadata_path, "-", e$message))
    return(NULL)
  })
}

#' Format slide notes with metadata
#'
#' @param metadata A list containing the metadata
#' @param abbreviation_definitions Named list mapping abbreviation
#'   keys to their full forms. If NULL, raw keys are displayed.
#' @param figure_footnotes Named list mapping `meta_type` keys to
#'   footnote text (the `figure_footnotes` section of
#'   `standard_footnotes.yaml`). When the metadata's
#'   `object_meta$meta_type` is non-NULL and not `"NA"`, the resolved
#'   text is prepended to the Notes line. Errors if `meta_type` is set
#'   but not found in `figure_footnotes`, matching reportifyr.
#'
#' @return A formatted string for slide notes
#' @keywords internal
#' @noRd
format_slide_notes <- function(metadata,
                               abbreviation_definitions = NULL,
                               figure_footnotes = NULL) {
  lines <- character()

  source_path <- metadata$source_meta$path %||% ""
  source_time <- metadata$source_meta$latest_time %||% ""
  if (nzchar(source_path) && nzchar(source_time)) {
    lines <- c(lines, paste0(
      "Source: ", source_path, " ", source_time
    ))
  } else if (nzchar(source_path)) {
    lines <- c(lines, paste0("Source: ", source_path))
  } else {
    lines <- c(lines, "Source: N/A")
  }

  meta_type <- metadata$object_meta$meta_type
  meta_type_text <- ""
  if (is.character(meta_type) && length(meta_type) == 1L &&
        nzchar(meta_type) && meta_type != "NA") {
    figure_footnotes <- figure_footnotes %||% list()
    if (!(meta_type %in% names(figure_footnotes))) {
      stop(sprintf(
        paste0(
          "meta_type '%s' not found in figure_footnotes section ",
          "of footnotes YAML"
        ),
        meta_type
      ))
    }
    resolved <- figure_footnotes[[meta_type]]
    if (is.character(resolved) && nzchar(resolved)) {
      meta_type_text <- if (endsWith(resolved, ".")) {
        paste0(resolved, " ")
      } else {
        paste0(resolved, ". ")
      }
    }
  }

  notes_list <- metadata$object_meta$footnotes$notes
  user_notes_text <- ""
  if (length(notes_list) > 0 && !all(notes_list == "")) {
    user_notes_text <- paste(vapply(notes_list, function(n) {
      if (!endsWith(n, ".")) paste0(n, ".") else n
    }, character(1)), collapse = " ")
  }

  combined_notes <- paste0(meta_type_text, user_notes_text)
  if (nzchar(combined_notes)) {
    lines <- c(lines, paste0("Notes: ", combined_notes))
  } else {
    lines <- c(lines, "Notes: N/A")
  }

  abbrev_list <- metadata$object_meta$footnotes$abbreviations
  if (length(abbrev_list) > 0 && !all(abbrev_list == "")) {
    abbrev_text <- decode_abbreviations(
      abbrev_list, abbreviation_definitions
    )
    lines <- c(lines, paste0("Abbreviations: ", abbrev_text))
  } else {
    lines <- c(lines, "Abbreviations: N/A")
  }

  lines <- c(lines, "")
  paste(lines, collapse = "\n")
}

#' Decode abbreviation keys into "KEY: full form" strings
#'
#' @param abbrev_list Character vector of abbreviation keys
#' @param definitions Named list mapping keys to full forms
#'
#' @return A single formatted string
#' @keywords internal
#' @noRd
decode_abbreviations <- function(abbrev_list, definitions = NULL) {
  filtered <- abbrev_list[nzchar(unlist(abbrev_list))]
  if (length(filtered) == 0) {
    return("N/A")
  }
  definitions <- definitions %||% list()
  parts <- vapply(filtered, function(key) {
    if (!(key %in% names(definitions))) {
      stop(sprintf(
        paste0(
          "Abbreviation '%s' not found in abbreviations section ",
          "of footnotes YAML"
        ),
        key
      ), call. = FALSE)
    }
    full_form <- definitions[[key]]
    paste0(key, ": ", sub("\\.$", "", full_form))
  }, character(1), USE.NAMES = FALSE)
  paste0(paste(parts, collapse = ", "), ".")
}
