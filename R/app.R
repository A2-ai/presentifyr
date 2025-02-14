options(shiny.maxRequestSize = 1000*1024^2)
#' @import shiny
#' @import bslib
NULL

theme <- bs_theme(
  version = 5
)

pptx_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    theme = theme,
    sidebar = sidebar(
      width = "50%",
      actionButton(ns("configs"), label = tagList(icon("cog"), "Configurations"), width = "100%"),
      actionButton(ns("sync"), label = tagList(icon("sync"), "Sync"), width = "100%"),
      uiOutput(ns("button")),
      treeNavigatorUI(ns("treeNavigator"))
    ),
    uiOutput(ns("show_files"))
  )
}

pptx_server <- function(id) {
  selected_items <- treeNavigatorServer(
    id,
    rootFolder = getwd(),
    search = FALSE,
    type = "include",
    pattern = include_imgs(),
    all.files = FALSE
  )

  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      rv <- reactiveValues(
        template = "default",
        uploaded_file = NULL,
        processed_file = NULL
      )

      # Configurations Modal
      observeEvent(input$configs, {
        showModal(modalDialog(
          title = "Customize PPTX Configuration",
          uiOutput(ns("configs_options")),
          hr(),
          tags$p("Please upload a PPTX template to use.")
        ))
      })

      # Sync Modal
      observeEvent(input$sync, {
        showModal(modalDialog(
          title = "Sync Images",
          tags$p("Use this menu to sync your PPTX with a local repository."),
          actionButton(ns("open_upload"), "Upload File"),
        ))
      })

      # Upload Modal
      observeEvent(input$open_upload, {
        # Close the Sync modal when opening the Upload modal
        removeModal()
        showModal(modalDialog(
          title = "Upload a File",
          fileInput(ns("uploaded_file"), "Choose a File:", accept = ".pptx"),
          footer = tagList(
            actionButton(ns("submit_file"), "Submit"),
            modalButton("Cancel")
          )
        ))
      })

      # Handle Uploaded File and Process Immediately
      observeEvent(input$submit_file, {
        rv$uploaded_file <- input$uploaded_file

        if (is.null(rv$uploaded_file)) {
          showModal(modalDialog(
            title = "Error",
            "No file was uploaded. Please try again.",
            footer = NULL
          ))
          return()
        }

        pptx_in <- rv$uploaded_file$datapath
        pptx_out <- tempfile(fileext = ".pptx")

        # Show processing modal
        showModal(modalDialog(
          title = "Processing File",
          "Your file is being processed. This may take a few moments.",
          footer = NULL
        ))

        tryCatch({
          # Call the utility function to replace images
          sync_images(pptx_in, pptx_out)

          # Save the processed file path for download
          rv$processed_file <- pptx_out

          # Success modal
          showModal(modalDialog(
            title = "Success",
            "Images were successfully updated. Your updated presentation is ready for download.",
            footer = tagList(
              downloadButton(ns("download_synced_pptx"), "Download Updated PPTX"),
              modalButton("Close")
            )
          ))

          # Set up download handler for the updated PPTX
          output$download_synced_pptx <- downloadHandler(
            filename = function() {
              paste0("synced_", basename(rv$uploaded_file$name))
            },
            content = function(file) {
              file.copy(rv$processed_file, file)
            }
          )
        }, error = function(e) {
          # Error modal
          showModal(modalDialog(
            title = "Error",
            paste("An error occurred while processing your file:", e$message),
            footer = NULL
          ))
        })
      })

      # Configurations Options UI
      output$configs_options <- renderUI({
        tagList(
          selectInput(ns("template"), "Choose Template:", selected = rv$template,
                      choices = c("Blank Template" = "default", "A2-Ai Template" = "a2_ai")),
          footer = tagList(
            actionButton(ns("confirm"), "Apply")
          )
        )
      })

      # Confirm Configurations
      observeEvent(input$confirm, {
        rv$template <- input$template
        removeModal()
      })

      observe({
        tryCatch({
          gert::git_remote_info()$url
        }, error = function(e) {
          showModal(modalDialog("No remote repositories found. Please set before using app.", footer = NULL))
        })
      })

      output$button <- renderUI({
        req(selected_items())
        if (length(selected_items()) == 0) {
          actionButton(ns("no_files"), "No files selected", style = "pointer-events: none;")
        } else {
          downloadButton(ns("download"), "Download pptx")
        }
      })

      output$download <- downloadHandler(
        filename = function() {
          paste("report.pptx")
        },
        content = function(file) {
          temp_pptx <- tempfile(fileext = ".pptx")

          remote_url <- gert::git_remote_info()$url

          showModal(modalDialog("Creating slides for PowerPoint . . .", footer = NULL))
          start_time <- Sys.time()

          suppressWarnings(create_pptx_with_images(
            remote_url = remote_url,
            files = selected_items(),
            output_pptx = temp_pptx,
            base_pptx = rv$template
          ))

          file.copy(temp_pptx, file)
          elapsed_time <- Sys.time() - start_time
          on.exit({
            file.remove(temp_pptx)
            showModal(modalDialog(
              sprintf("PowerPoint downloaded successfully in %.2f seconds.", elapsed_time),
            ))
          })
        }
      )

      output$show_files <- renderUI({
        tagList(
          tags$h6("These files reflect the current state of your local repository.
                  Please commit and push any changes or new files to GitHub to
                  ensure the links are up to date."),
          tags$ul(
            lapply(selected_items(), function(file) {
              tags$li(file)
            })
          )
        )
      })
    }
  )
}


app_ui <- function() {
  pptx_ui(id = "app")
}

app_server <- function(input, output, session) {
  pptx_server(id="app")
}

#' @export
app <- function() {
  app <- shinyApp(
    ui = app_ui,
    server = app_server
  )
  runApp(app)
}
