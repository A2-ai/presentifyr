#' Creates a new PowerPoint presentation (.pptx) file with images added from file.
#'
#' @param files A list of image file paths to be inserted into the .pptx file.
#' @param repo_url The URL of the repository where an image is stored.
#' @param base_pptx The file path to an existing .pptx file that serves as a template. Default is NULL. If NULL, a blank presentation is used.
#' @param output_pptx The file path where the modified .pptx file will be saved.
#'
#' @export
#'
#' @examples \dontrun{
#' create_pptx(
#'   files = files,
#'   repo_url = gert::git_remote_info()$url,
#'   base_pptx = NULL,
#'   output_pptx = output_pptx
#' )
#' }
create_pptx <- function(files,
                        remote_url = gert::git_remote_info()$url,
                        base_pptx,
                        output_pptx) {

  repo_url <- clean_url(remote_url) ## Convert SSH URL to HTTPS URL if needed

  current_branch <- get_current_branch()

  repo_url <- paste0(repo_url, "/blob/", current_branch)

  add_images(files, repo_url, base_pptx, output_pptx)
}
