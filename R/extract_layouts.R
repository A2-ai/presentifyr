extract_layouts <- function(base_pptx, output_dir = tempdir()) {
  script <- system.file("scripts/extract_layouts.py", package = "presentifyr")

  if (!file.exists(base_pptx)) {
    stop("Base PowerPoint file not found")
  }

  args <- c("run", script, "-b", base_pptx, "-o", output_dir)

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
      command = uv_path,
      args = args,
      env = c("current", VIRTUAL_ENV = venv_path, PY_LOG_LEVEL = Sys.getenv("PRFY_VERBOSE", unset = "WARN")),
      error_on_status = TRUE,
      echo = TRUE,
    )
  }, error = function(e) {
    stop(paste("Extract layout script failed. Status: ", e$status, "Stderr: ", e$stderr))
  })

  layout_images <- list.files(
    output_dir,
    pattern = "^layout_\\d+\\.png$",
    full.names = TRUE
  )

  # Parse the real layout index from each filename, e.g. "layout_3.png" -> 3
  parsed_indices <- as.integer(sub("layout_(\\d+)\\.png", "\\1", basename(layout_images)))

  # Return them in a data frame
  out_df <- data.frame(
    index = parsed_indices,
    image_path = layout_images,
    stringsAsFactors = FALSE
  )

  # (Optional) Log how many layouts were found
  message(sprintf("Found %d layout image(s) in '%s'.", nrow(out_df), output_dir))

  return(out_df)
}
