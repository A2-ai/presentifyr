#' Conditionally cleans an SSH URL to HTTPS
#'
#' @return HTTPS URL
#'
#' @keywords internal
#' @noRd
clean_url <- function(remote_url) {
  if (grepl("^https://", remote_url)) {
    url <- paste0(dirname(remote_url), "/", basename(getwd())) ## To remove .git at end of url
    return(url)
  }

  if (!grepl("^git@github\\.com:", remote_url)) {
    stop("The provided URL is not a valid SSH Key.")
  }

  parts <- strsplit(remote_url, ":|@")[[1]]
  username <- parts[2]
  repo <- sub(".git$", "", parts[3])

  https_url <- paste0(username, "/", repo)

  return(https_url)
}

#' Gets the current git branch
#'
#' @return current git branch
#'
#' @keywords internal
#' @noRd
get_current_branch <- function() {
  branch_info <- processx::run("git", args = c("symbolic-ref", "--short", "HEAD"))
  current_branch <- trimws(branch_info$stdout)  ## Trim any whitespace or newlines
  return(current_branch)
}

#' Gets the path to uv -- pre v0.5.0 installed to /.cargo/bin post v0.5.0 to /.local/bin
#'
#' @return path to uv
#'
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
#' @return vector of image file paths
#'
#' @keywords internal
#' @noRd
parse_directory_for_images <- function(directory,
                                       recursive = TRUE,
                                       full.names = TRUE,
                                       exclude_dirs = NULL) {
  if (!dir.exists(directory)) {
    stop("The specified directory does not exist.")
  }

  image_pattern <- "\\.(png|jpg|jpeg|gif|bmp|tiff|webp)$"

  image_files <- list.files(
    path = directory,
    pattern = image_pattern,
    recursive = recursive,
    full.names = full.names,
    ignore.case = TRUE
  )

  if (!is.null(exclude_dirs) && length(image_files) > 0) {
    exclude_pattern <- paste0(exclude_dirs, collapse = "|")
    image_files <- image_files[!grepl(exclude_pattern, image_files, ignore.case = TRUE)]
  }

  if (length(image_files) == 0) {
    message("No image files found in the specified directory.")
  }

  return(image_files)
}
