#' @export
app_with_dir <- function(path_to_project_directory){
  withr::with_dir(path_to_project_directory, app())
}
