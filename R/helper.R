#' @import officer
#' @import purrr
#' @import gert
#' @import processx
NULL

clean_url <- function(remote_url) {
  # Check if the input is already an HTTPS URL
  if (grepl("^https://", remote_url)) {
    # The provided URL is already https, just remove the .git
    url <- paste0(dirname(remote_url), "/", basename(getwd())) # to remove .git at end of url
    return(url)
  }

  # Check if the input is a valid SSH URL
  if (!grepl("^git@github\\.com:", remote_url)) {
    stop("The provided URL is not a valid SSH Key.")
  }

  # Extract the username and repository from the SSH Key
  parts <- strsplit(remote_url, ":|@")[[1]]
  username <- parts[2]
  repo <- sub(".git$", "", parts[3])

  # Construct the HTTPS URL
  https_url <- paste0(username, "/", repo)

  return(https_url)
}

get_current_branch <- function() {
  # Get the current branch name
  branch_info <- processx::run("git", args = c("symbolic-ref", "--short", "HEAD"))
  current_branch <- trimws(branch_info$stdout)  # Trim any whitespace or newlines
  return(current_branch)
}

add_images_to_ppt <- function(files, repo_url, output_pptx, base_pptx = "default", height = 0, width = 0) {

  base_pptx_path <- switch(
    base_pptx,
    "default" = "None", # Pass "None" explicitly to the Python script for blank presentation
    "a2_ai" = system.file("templates/a2_ai_temp.pptx", package = "pptx"),
    stop("Invalid base_pptx value. Choose either 'default' or a valid template like 'a2_ai'.")
  )

  script <- system.file("scripts/add_images.py", package = "pptx")
  args <- c("run", script, "-f", files, "-r", repo_url, "-o", output_pptx, "-b", base_pptx_path)

  if (is.null(getOption("venv_dir"))) {
    message("Setting options('venv_dir') to project root.")
    options("venv_dir" = here::here())
  }

  venv_path <- file.path(getOption("venv_dir"), ".venv")

  if (!dir.exists(venv_path)) {
    stop("Create virtual environment with initialize_python")
  }

  uv_path <- get_uv_path()

  result <- tryCatch({
    processx::run(
      command = uv_path, args = args, env = c("current", VIRTUAL_ENV = venv_path), error_on_status = TRUE
    )
  }, error = function(e) {
    stop(paste("Add images script failed. Status: ", e$status, "Stderr: ", e$stderr))
  })
}

create_pptx_with_images <- function(files, remote_url = gert::git_remote_info()$url, output_pptx, base_pptx = "default", height = 0, width = 0) {
  # Convert SSH URL to HTTPS URL if needed
  repo_url <- clean_url(remote_url)

  # Get the current branch name
  current_branch <- get_current_branch()

  repo_url <- paste0(repo_url, "/blob/", current_branch)

  add_images_to_ppt(files, repo_url, output_pptx, base_pptx, height, width)
}

sync_images <- function(pptx_in,
                        pptx_out) {

  image_files <- parse_directory_for_images(
    directory = here::here(),
    exclude_dirs = c("/renv/") # Exclude unnecessary directories
  )

  if (length(image_files) == 0) {
    stop("No images found in the specified directory.")
  }

  image_dict <- as.list(setNames(image_files, basename(image_files)))
  temp_image_dict <- tempfile(fileext = ".json")
  jsonlite::write_json(image_dict, temp_image_dict, auto_unbox = TRUE, pretty = TRUE)

  script <- system.file("scripts/sync_images.py", package = "pptx")
  args <- c("run", script, "-i", pptx_in, "-o", pptx_out, "-d", temp_image_dict)

  if (is.null(getOption("venv_dir"))) {
    message("Setting options('venv_dir') to project root.")
    options("venv_dir" = here::here())
  }

    venv_path <- file.path(getOption("venv_dir"), ".venv")

    if (!dir.exists(venv_path)) {
      stop("Create virtual environment with initialize_python")
    }

    uv_path <- get_uv_path()

    result <- tryCatch({
      processx::run(
        command = uv_path, args = args, env = c("current", VIRTUAL_ENV = venv_path), error_on_status = TRUE
      )
    }, error = function(e) {
      stop(paste("Sync images script failed. Status: ", e$status, "Stderr: ", e$stderr))
    })
}

parse_directory_for_images <- function(directory, recursive = TRUE, full.names = TRUE, exclude_dirs = NULL) {
  if (!dir.exists(directory)) {
    stop("The specified directory does not exist.")
  }

  image_pattern <- "\\.(png|jpg|jpeg|gif|bmp|tiff|webp)$"

  # List files in the directory
  image_files <- list.files(
    path = directory,
    pattern = image_pattern,
    recursive = recursive,
    full.names = full.names,
    ignore.case = TRUE # Case-insensitive match
  )

  # Exclude specified subdirectories
  if (!is.null(exclude_dirs) && length(image_files) > 0) {
    exclude_pattern <- paste0(exclude_dirs, collapse = "|")
    image_files <- image_files[!grepl(exclude_pattern, image_files, ignore.case = TRUE)]
  }

  if (length(image_files) == 0) {
    message("No image files found in the specified directory.")
  }

  return(image_files)
}
