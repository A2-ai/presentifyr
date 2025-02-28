#' @noRd
pptx_ui <- function(id) {
  theme <- bslib::bs_theme(version = 5)
  ns <- shiny::NS(id)

  bslib::page_sidebar(
    theme = theme,
    sidebar = bslib::sidebar(
      width = "50%",
      shiny::actionButton(ns("configs"), label = htmltools::tagList(shiny::icon("cog"), "Configurations"), width = "100%"),
      shiny::actionButton(ns("sync"), label = htmltools::tagList(shiny::icon("sync"), "Sync"), width = "100%"),
      shiny::uiOutput(ns("button")),
      treeNavigatorUI(ns("treeNavigator"))
    ),
    shiny::uiOutput(ns("show_files"))
  )
}

#' @noRd
app_ui <- function() {
  pptx_ui(id = "app")
}
