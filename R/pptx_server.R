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
      log4r::debug(.le$logger, "pptx_server module started")
      ns <- shiny::NS(id)
      rv <- shiny::reactiveValues(
        uploaded_template = NULL,  # To store the uploaded template for add_images
        uploaded_file = NULL,  # For the sync functionality
        processed_file = NULL
      )

      # Configurations Modal for uploading a template
      shiny::observeEvent(input$configs, {
        log4r::info(.le$logger, "Opening Configuration Modal")
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
        log4r::info(.le$logger, "Opening Sync Modal")
        shiny::showModal(shiny::modalDialog(
          title = "Sync Images",
          htmltools::tags$p("Use this menu to sync your PPTX with a local repository."),
          shiny::actionButton(ns("open_upload_sync"), "Upload File")
        ))
      })

      # Upload Modal for Config (Template Upload)
      shiny::observeEvent(input$open_upload_sync, {
        log4r::info(.le$logger, "Opening Upload Modal for Sync Modal")
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
        log4r::info(.le$logger, "Processing submitted PPTX for syncing")
        rv$uploaded_file <- input$uploaded_sync_file

        if (is.null(rv$uploaded_file)) {
          log4r::error(.le$logger, "No PPTX file uploaded for syncing")
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            "No PPTX file was uploaded for syncing. Please try again.",
            footer = shiny::modalButton("Close")
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

        log4r::debug(.le$logger, paste0("Input PPTX file: ", input_pptx))

        tryCatch({
          sync_images(input_pptx, output_pptx)

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
              log4r::info(.le$logger, "Downloading synced PPTX file")
            }
          )
        }, error = function(e) {
          log4r::error(.le$logger, paste0("Error processing PPTX file: ", e$message))
          # Error modal
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            paste("An error occurred while processing your file:", e$message),
            footer = shiny::modalButton("Close")
          ))
        })
      })

      # Configurations Options UI for the Template Upload
      output$configs_options <- shiny::renderUI({
        log4r::info(.le$logger, "Rendering UI for PPTX template upload options")
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
          log4r::error(.le$logger, "No template PPTX file was uploaded")
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            "No template file was uploaded. A blank template will be used instead.",
            footer = shiny::modalButton("Close")
          ))
          return()
        }

        # Update the template with the uploaded template for add_images functionality
        log4r::debug(.le$logger, paste0("Template uploaded: ", rv$uploaded_template$datapath))
        log4r::info(.le$logger, "Template uploaded successfully")
        shiny::removeModal()
      })

      # Clear Template Button functionality
      shiny::observeEvent(input$clear_template, {
        log4r::info(.le$logger, "Clearing template")
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
          log4r::info(.le$logger, "Template file removed")
        }

        # Reset the template variables
        rv$uploaded_template <- NULL

        shiny::removeModal()

        log4r::info(.le$logger, "Template reset to default")
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

          remote_url <- gert::git_remote_info()$url # tryCatch in observe as primary

          if (is.null(remote_url)) {
            shiny::showModal(shiny::modalDialog(
              title = "Error",
              "No remote repositories found. Please set a remote repository before using this feature.",
              footer = NULL
            ))
            return()
          } # Backup for secondary

          shiny::showModal(shiny::modalDialog("Creating slides for PowerPoint . . .", footer = NULL))
          log4r::info(.le$logger, "Starting PowerPoint creation process")
          start_time <- Sys.time()

          # Use the uploaded template, or the default if not uploaded
          base_pptx <- if (!is.null(rv$uploaded_template)) rv$uploaded_template$datapath

          tryCatch({
            create_pptx(
              remote_url = remote_url,
              files = selected_items(),
              output_pptx = temp_pptx,
              base_pptx = base_pptx
            )

            file.copy(temp_pptx, file)
            elapsed_time <- Sys.time() - start_time

            log4r::info(.le$logger, sprintf("PowerPoint successfully created and downloaded in %.2f seconds.", elapsed_time))

            shiny::showModal(shiny::modalDialog(
              title = "Success",
              sprintf("PowerPoint downloaded successfully in %.2f seconds.", elapsed_time),
              footer = shiny::modalButton("Close")
            ))

          }, error = function(e) {
            log4r::error(.le$logger, paste0("Error creating PPTX: ", e$message))
            shiny::showModal(shiny::modalDialog(
              title = "Error",
              paste("An error occurred while creating the PowerPoint:", e$message),
              footer = shiny::modalButton("Close")
            ))
          }, finally = {
            on.exit({
              if (file.exists(temp_pptx)) file.remove(temp_pptx)
            })
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
      log4r::debug(.le$logger, "pptx_server module loaded successfully")
    }
  )
}

#' @noRd
app_server <- function(input, output, session) {
  log4r::info(.le$logger, "Initializing app server.")
  pptx_server(id="app")
}
