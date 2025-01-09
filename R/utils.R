#' Gets the path to uv -- pre v0.5.0 installed to /.cargo/bin post v0.5.0 to /.local/bin
#'
#' @return path to uv
#'
#' @keywords internal
#' @noRd
get_uv_path <- function() {
  uv_paths <- c(normalizePath("~/.local/bin/uv", mustWork = FALSE),
                normalizePath("~/.cargo/bin/uv", mustWork = FALSE))

  # Find the first existing path, preferring ~/.local/bin/uv
  uv_path <- uv_paths[file.exists(uv_paths)][1]

  if (is.null(uv_path)) {
    stop("Please install uv with initialize_python")
  } else {
    return(uv_path)
  }
}
