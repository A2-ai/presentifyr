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

  if (!file.exists(input_pptx)) {
    log4r::error(.le$logger, paste("The input .pptx file does not exist:", input_pptx))
    stop(paste("The input .pptx file does not exist:", input_pptx))
  }

  if (!grepl("\\.pptx$", input_pptx, ignore.case = TRUE)) {
    log4r::error(.le$logger, paste("Invalid file type. Expected a .pptx file:", input_pptx))
    stop("Invalid file type. Expected a .pptx file.")
  }

  exclude_dirs <- default_exclude_dirs()

  root_dir <- getOption("project.dir", default = here::here())

  image_files <- parse_directory_for_images(
    directory = root_dir,
    exclude_dirs = exclude_dirs ## Exclude unnecessary directories
  )

  if (length(image_files) == 0 || all(image_files == "")) {
    log4r::error(.le$logger, paste("No image files found. Execution halted."))
    stop("No image files found. Execution halted.")
  }

  log4r::debug(.le$logger, paste0("Found ", length(image_files), " image files"))

  ## Build keys matching alt-text: prefer project-relative paths; include basename for backward compatibility
  keys_primary <- vapply(image_files, prfy_image_key, character(1), root = root_dir)
  keys_legacy <- basename(image_files)

  file_info <- fs::file_info(image_files)
  df <- rbind(
    data.frame(key = keys_primary, path = image_files, mtime = file_info$modification_time, stringsAsFactors = FALSE),
    data.frame(key = keys_legacy, path = image_files, mtime = file_info$modification_time, stringsAsFactors = FALSE)
  )

  df <- df[order(df$key, df$mtime, decreasing = c(FALSE, TRUE)), ]
  dedup <- df[!duplicated(df$key), ]

  dup_counts <- table(df$key)
  dup_keys <- names(dup_counts[dup_counts > 1])
  if (length(dup_keys) > 0) {
    msg <- paste(
      sprintf(
        "%s (%d candidates, picked %s)",
        dup_keys,
        dup_counts[dup_keys],
        dedup$path[match(dup_keys, dedup$key)]
      ),
      collapse = "; "
    )
    log4r::warn(.le$logger, paste("Duplicate image keys; using newest by mtime ->", msg))
  }

  image_dict <- as.list(stats::setNames(dedup$path, dedup$key))
  temp_image_dict <- tempfile(fileext = ".json")
  jsonlite::write_json(image_dict, temp_image_dict, auto_unbox = TRUE, pretty = TRUE)
  log4r::debug(.le$logger, paste("Temporary image dictionary created at:", temp_image_dict))

  script <- system.file("scripts/sync_images.py", package = "presentifyr")
  args <- c("run", script, "-i", input_pptx, "-o", output_pptx, "-d", temp_image_dict)

  if (is.null(getOption("venv_dir"))) {
    log4r::info(.le$logger, "Setting options('venv_dir') to project root")
    message("Setting options('venv_dir') to project root")

    options("venv_dir" = here::here())
  }

  venv_path <- file.path(getOption("venv_dir"), ".venv")

  if (!dir.exists(venv_path)) {
    log4r::error(.le$logger, "Virtual environment not found. Please initialize with initialize_python")
    stop("Create virtual environment with initialize_python")
  }
  log4r::debug(.le$logger, paste("venv_path resolved to: ", venv_path))

  uv_path <- get_uv_path()
  log4r::debug(.le$logger, paste("uv path resolved to:", uv_path))

  result <- tryCatch({
    processx::run(
      command = uv_path,
      args = args,
      env = c("current", VIRTUAL_ENV = venv_path, PY_LOG_LEVEL = Sys.getenv("PRFY_VERBOSE", unset = "WARN")),
      error_on_status = TRUE,
      echo = TRUE,
    )
  }, error = function(e) {
    log4r::error(.le$logger, paste0("Sync images Python script failed. Status: ", e$status))
    log4r::error(.le$logger, paste0("Sync images Python script failed. Stderr: ", e$stderr))
    log4r::info(.le$logger, paste0("Sync images Python script failed. Stdout: ", e$stdout))
    stop(paste("Sync images script failed. Status: ", e$status, "Stderr: ", e$stderr))
  })
  log4r::debug(.le$logger, "Exiting sync images R function")
}
