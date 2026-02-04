#' @noRd
pptx_ui <- function(id) {
  theme <- bslib::bs_theme(version = 5)
  ns <- shiny::NS(id)

  bslib::page_sidebar(
    htmltools::tags$head(
      htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "presentifyr/resize-sidebar.css"),
      htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "presentifyr/modal-styles.css"),
      htmltools::tags$script(src = "presentifyr/resize-sidebar.js")
    ),
    theme = theme,
    sidebar = bslib::sidebar(

      # Logo and title section (reduced vertical padding)
      htmltools::div(
        style = "display: flex; align-items: center; margin-bottom: 5px; padding: 0;",
        htmltools::img(src = "www/logo.png", width = "75"),
        htmltools::h4("presentifyr", style = "margin-left: 5px; margin-bottom: 0; padding: 0;")
      ),

      # Divider (minimal vertical spacing)
      htmltools::tags$hr(style = "margin-top: 0; margin-bottom: 10px;"),

      # Modal action buttons/options
      htmltools::tags$span(
        title = "Configure output filename, template, and layout",
        shiny::actionButton(ns("configs"), label = "Settings", width = "100%")
      ),
      htmltools::tags$span(
        title = "Update images in an existing PowerPoint file",
        shiny::actionButton(ns("sync"), label = "Sync PowerPoint", width = "100%")
      ),
      shiny::uiOutput(ns("button")),
      treeNavigatorUI(ns("treeNavigator"))
    ),

    shiny::uiOutput(ns("show_files"))
  )
}

#' @noRd
app_ui <- function() {
  shiny::addResourcePath("www", system.file("www", package = "presentifyr"))
  pptx_ui(id = "app")
}
