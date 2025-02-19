#' @noRd
pptx_server <- function(id) {
  selected_items <- treeNavigatorServer(
    id,
    rootFolder = getwd(),
    search = FALSE,
    type = "include",
    pattern = include_imgs(),
    all.files = FALSE
  )

  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- shiny::NS(id)
      rv <- shiny::reactiveValues(
        template = "default",  # Default template for the add_images functionality
        uploaded_template = NULL,  # To store the uploaded template for add_images
        uploaded_file = NULL,  # For the sync functionality
        processed_file = NULL
      )

      # Configurations Modal for uploading a template
      shiny::observeEvent(input$configs, {
        shiny::showModal(shiny::modalDialog(
          title = "Customize PPTX Configuration",
          shiny::uiOutput(ns("configs_options")),
          htmltools::hr(),
          htmltools::tags$p("Please use the following to clear a provided PPTX template:"),
          shiny::actionButton(ns("clear_template"), "Clear Template", class = "btn-danger")  # Clear Template button
        ))
      })

      # Sync Modal for syncing images
      shiny::observeEvent(input$sync, {
        shiny::showModal(shiny::modalDialog(
          title = "Sync Images",
          htmltools::tags$p("Use this menu to sync your PPTX with a local repository."),
          shiny::actionButton(ns("open_upload_sync"), "Upload File")
        ))
      })

      # Upload Modal for Config (Template Upload)
      shiny::observeEvent(input$open_upload_sync, {
        # Close the sync modal and open the upload modal for syncing
        shiny::removeModal()
        shiny::showModal(shiny::modalDialog(
          title = "Upload PPTX for Syncing",
          shiny::fileInput(ns("uploaded_sync_file"), "Choose a PPTX File for Syncing:", accept = ".pptx"),
          footer = htmltools::tagList(
            shiny::actionButton(ns("submit_sync_file"), "Submit Sync File"),
            shiny::modalButton("Cancel")
          )
        ))
      })

      # Handle Sync Upload (for Syncing Images)
      shiny::observeEvent(input$submit_sync_file, {
        rv$uploaded_file <- input$uploaded_sync_file

        if (is.null(rv$uploaded_file)) {
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            "No PPTX file was uploaded for syncing. Please try again.",
            footer = NULL
          ))
          return()
        }

        input_pptx <- rv$uploaded_file$datapath
        output_pptx <- tempfile(fileext = ".pptx")

        # Show processing modal
        shiny::showModal(shiny::modalDialog(
          title = "Processing Sync",
          "Your PPTX file is being synced. This may take a few moments.",
          footer = NULL
        ))

        tryCatch({
          # Call the utility function to replace images in the PPTX
          sync_images(input_pptx, output_pptx)

          # Save the processed file path for download
          rv$processed_file <- output_pptx

          # Success modal
          shiny::showModal(shiny::modalDialog(
            title = "Success",
            "Images were successfully updated. Your updated presentation is ready for download.",
            footer = htmltools::tagList(
              shiny::downloadButton(ns("download_synced_pptx"), "Download Updated PPTX"),
              shiny::modalButton("Close")
            )
          ))

          # Set up download handler for the updated PPTX
          output$download_synced_pptx <- shiny::downloadHandler(
            filename = function() {
              paste0("synced_", basename(rv$uploaded_file$name))
            },
            content = function(file) {
              file.copy(rv$processed_file, file)
            }
          )
        }, error = function(e) {
          # Error modal
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            paste("An error occurred while processing your file:", e$message),
            footer = NULL
          ))
        })
      })

      # Configurations Options UI for the Template Upload
      output$configs_options <- shiny::renderUI({
        htmltools::tagList(
          shiny::fileInput(ns("uploaded_template"), "Choose a PPTX Template for Adding Images:", accept = ".pptx"),  # Upload field for template
          footer = htmltools::tagList(
            shiny::actionButton(ns("submit_template"), "Submit Template"),
            shiny::modalButton("Cancel")
          )
        )
      })

      # Handle Template Upload for Add Images
      shiny::observeEvent(input$submit_template, {
        rv$uploaded_template <- input$uploaded_template

        if (is.null(rv$uploaded_template)) {
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            "No template file was uploaded. A blank template will be used instead.",
            footer = NULL
          ))
          return()
        }

        # Update the template with the uploaded template for add_images functionality
        rv$template <- rv$uploaded_template$datapath
        shiny::removeModal()
      })

      # Clear Template Button functionality
      shiny::observeEvent(input$clear_template, {
        shiny::showModal(shiny::modalDialog(
          title = "Clear Template",
          "Are you sure you want to clear the current template and use a blank template?",
          footer = htmltools::tagList(
            shiny::actionButton(ns("confirm_clear"), "Yes, Clear Template"),
            shiny::modalButton("Cancel")
          )
        ))
      })

      # Confirm the action to clear the template
      shiny::observeEvent(input$confirm_clear, {
        # If an uploaded template exists, remove it
        if (!is.null(rv$uploaded_template) && file.exists(rv$uploaded_template$datapath)) {
          file.remove(rv$uploaded_template$datapath)
        }

        # Reset the template variables
        rv$uploaded_template <- NULL
        rv$template <- "default"  # Set to the blank template

        shiny::removeModal()

        shiny::showModal(shiny::modalDialog(
          title = "Template Cleared",
          "The uploaded template has been removed, and a blank template will be used instead.",
          footer = shiny::modalButton("Close")
        ))
      })

      shiny::observe({
        tryCatch({
          gert::git_remote_info()$url
        }, error = function(e) {
          shiny::showModal(shiny::modalDialog("No remote repositories found. Please set before using app.", footer = NULL))
        })
      })

      output$button <- shiny::renderUI({
        shiny::req(selected_items())
        if (length(selected_items()) == 0) {
          shiny::actionButton(ns("no_files"), "No files selected", style = "pointer-events: none;")
        } else {
          shiny::downloadButton(ns("download"), "Download pptx")
        }
      })

      output$download <- shiny::downloadHandler(
        filename = function() {
          paste("report.pptx")
        },
        content = function(file) {
          temp_pptx <- tempfile(fileext = ".pptx")

          remote_url <- gert::git_remote_info()$url

          shiny::showModal(shiny::modalDialog("Creating slides for PowerPoint . . .", footer = NULL))
          start_time <- Sys.time()

          # Use the uploaded template, or the default if not uploaded
          base_pptx <- if (!is.null(rv$uploaded_template)) rv$uploaded_template$datapath else rv$template

          suppressWarnings(create_pptx(
            remote_url = remote_url,
            files = selected_items(),
            output_pptx = temp_pptx,
            base_pptx = base_pptx
          ))

          file.copy(temp_pptx, file)
          elapsed_time <- Sys.time() - start_time
          on.exit({
            file.remove(temp_pptx)
            shiny::showModal(shiny::modalDialog(
              sprintf("PowerPoint downloaded successfully in %.2f seconds.", elapsed_time),
            ))
          })
        }
      )

      output$show_files <- shiny::renderUI({
        htmltools::tagList(
          htmltools::tags$h6("These files reflect the current state of your local repository.
                  Please commit and push any changes or new files to GitHub to
                  ensure the links are up to date."),
          htmltools::tags$ul(
            lapply(selected_items(), function(file) {
              htmltools::tags$li(file)
            })
          )
        )
      })
    }
  )
}

app_server <- function(input, output, session) {
  pptx_server(id="app")
}
