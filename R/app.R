#' This function launches the Shiny application.
#'
#' @return This function does not return; it runs the Shiny app until the
#'   session is ended.
#' @export
app <- function() {
  app <- shiny::shinyApp(
    ui = app_ui,
    server = app_server
  )
  shiny::runApp(app)
}
