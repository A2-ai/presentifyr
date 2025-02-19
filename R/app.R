options(shiny.maxRequestSize = 1000*1024^2)

#' This function launches the Shiny application.
#'
#' @export
app <- function() {
  app <- shiny::shinyApp(
    ui = app_ui,
    server = app_server
  )
  shiny::runApp(app)
}
