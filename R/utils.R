#' Conditionally cleans an SSH url to HTTPS
#'
#' @param remote_url The url of the repository where an image is stored.
#'
#' @return A character string representing the https url
#' @keywords internal
#' @noRd
clean_url <- function(remote_url) {
  log4r::debug(.le$logger, paste("Received remote_url:", remote_url))

  if (grepl("^https://", remote_url)) {
    url <- paste0(dirname(remote_url), "/", basename(getwd())) ## To remove .git at end of url
    log4r::debug(.le$logger, paste("HTTPS URL cleaned to:", url))
    return(url)
  }

  if (!grepl("^git@github\\.com:", remote_url)) {
    log4r::error(.le$logger, paste("Invalid SSH URL:", remote_url))
    stop("The provided URL is not a valid SSH Key.")
  }

  parts <- strsplit(remote_url, ":|@")[[1]]
  username <- parts[2]
  repo <- sub(".git$", "", parts[3])

  https_url <- paste0(username, "/", repo)
  log4r::debug(.le$logger, paste("Converted SSH URL to:", https_url))

  return(https_url)
}

#' Gets the current git branch
#'
#' @return A character string of the current git branch
#' @keywords internal
#' @noRd
get_current_branch <- function() {
  branch_info <- tryCatch(
    {
      processx::run("git", args = c("symbolic-ref", "--short", "HEAD"))
    },
    error = function(e) {
      log4r::error(.le$logger, paste0("Failed to retrieve Git branch. Status: ", e$status))
      log4r::error(.le$logger, paste0("Failed to retrieve Git branch. Stderr: ", e$stderr))
      log4r::info(.le$logger, paste0("Failed to retrieve Git branch. Stdout: ", e$stdout))
      stop("Error retrieving current Git branch.")
    }
  )

  current_branch <- trimws(branch_info$stdout)  ## Trim any whitespace or newlines
  log4r::debug(.le$logger, paste("Current branch resolved to:", current_branch))

  return(current_branch)
}

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
#' @param full.names A logical value. If TRUE, returns full file paths; if FALSE, returns only file names. Default is TRUE.
#' @param exclude_dirs A vector of directory names to exclude from the search. Default is NULL. If NULL, no directories are excluded.
#'
#' @return A character vector of image file paths.
#' @keywords internal
#' @noRd
parse_directory_for_images <- function(directory,
                                       recursive = TRUE,
                                       full.names = TRUE,
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
    full.names = full.names,
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
