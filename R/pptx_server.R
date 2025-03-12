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
        uploaded_template = NULL,
        extracted_layouts = NULL,
        selected_layout_idx = NULL,
        uploaded_file = NULL,
        processed_file = NULL
      )

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 1. Create configs modal for uploading & configuring PPTX
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      shiny::observeEvent(input$configs, {
        log4r::info(.le$logger, "Opening Configuration Modal")
        showConfigModal()
      })

      showConfigModal <- function() {
        shiny::showModal(
          shiny::modalDialog(
            title = "Customize PPTX Configuration",
            shiny::uiOutput(ns("configs_options")),
            htmltools::hr(),
            htmltools::tags$p("Please use the following to clear a provided PPTX template:"),
            shiny::actionButton(ns("clear_template"), "Clear Template"),
            footer = shiny::modalButton("Close")
          )
        )
      }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 2. UI for file input, template confirmation, layout selection
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      output$configs_options <- shiny::renderUI({
        log4r::info(.le$logger, "Rendering UI for PPTX configuration & layout selection")

        file_upload_area <- shiny::tagList(
          shiny::fileInput(
            ns("uploaded_template"),
            "Choose a PPTX Template for Adding Images:",
            accept = ".pptx"
          ),
          shiny::actionButton(ns("submit_template"), "Submit Template")
        )

        template_label <- NULL
        if (!is.null(rv$uploaded_template)) {
          template_name <- basename(rv$uploaded_template$name)
          template_label <- htmltools::tags$p(
            style = "font-weight:bold; color:blue; margin-top:5px;", ## Blue confirmation text
            paste("Currently using template:", template_name)
          )
        }

        layout_area <- NULL
        if (!is.null(rv$extracted_layouts) && nrow(rv$extracted_layouts) > 0) {
          layout_area <- htmltools::tagList(
            htmltools::hr(),
            buildLayoutSelectionUI()
          )
        }

        layout_label <- NULL
        if (!is.null(rv$selected_layout_idx)) {
          layout_label <- htmltools::tags$p(
            style = "font-weight:bold; color:green; margin-top:5px;", ## Green confirmation text
            paste("Selected Layout:", rv$selected_layout_idx)
          )
        }

        shiny::tagList(
          file_upload_area,
          template_label,
          layout_area,
          layout_label
        )
      })

      buildLayoutSelectionUI <- function() {
        layout_divs <- lapply(seq_len(nrow(rv$extracted_layouts)), function(i) {
          layout_idx <- rv$extracted_layouts$index[i]
          htmltools::tags$div(
            style = "display:inline-block; margin: 10px; text-align:center;",
            htmltools::tags$img(src = rv$extracted_layouts$image_path[i], width = "150px"),
            htmltools::tags$p(paste("Layout:", layout_idx)),
            shiny::actionButton(ns(paste0("btn_layout_", layout_idx)), paste("Select Layout", layout_idx))
          )
        })

        htmltools::tags$div(
          style = "margin-top:10px;",
          htmltools::tags$h4("Select a Layout"),
          do.call(htmltools::tagList, layout_divs)
        )
      }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 3. Submit template - extract layouts
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

        base_pptx <- rv$uploaded_template$datapath
        log4r::debug(.le$logger, paste0("Template uploaded: ", base_pptx))

        output_dir <- tempdir()
        log4r::debug(.le$logger, paste0("Temporary output directory: ", output_dir))

        tryCatch({
          layouts_df <- extract_layouts(base_pptx, output_dir)

          shiny::addResourcePath("pptx_layouts", output_dir)
          layouts_df$image_path <- file.path("pptx_layouts", basename(layouts_df$image_path))

          rv$extracted_layouts <- layouts_df
          log4r::info(.le$logger, paste0("Extracted ", nrow(layouts_df), " layouts from template"))

        }, error = function(e) {
          log4r::error(.le$logger, paste0("Error extracting layouts: ", e$message))
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            paste("An error occurred while extracting layouts:", e$message),
            footer = shiny::modalButton("Close")
          ))
        })
      })

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 4. Observers for each select layout button
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      shiny::observe({
        if (is.null(rv$extracted_layouts) || nrow(rv$extracted_layouts) == 0) return()

        for (layout_idx in rv$extracted_layouts$index) {
          local({
            li <- layout_idx
            observeEvent(input[[paste0("btn_layout_", li)]], {
              rv$selected_layout_idx <- li
              log4r::info(.le$logger, paste0("User selected layout index: ", li))
            })
          })
        }
      })

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 5. Clear template logic + return to configs modal
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      shiny::observeEvent(input$clear_template, {
        log4r::info(.le$logger, "Clearing template")
        shiny::showModal(shiny::modalDialog(
          title = "Clear Template",
          "Are you sure you want to clear the current template and use a blank template?",
          footer = htmltools::tagList(
            shiny::actionButton(ns("confirm_clear"), "Yes, Clear Template"),
            shiny::modalButton("Close")
          )
        ))
      })

      shiny::observeEvent(input$confirm_clear, {
        if (!is.null(rv$uploaded_template) && file.exists(rv$uploaded_template$datapath)) {
          file.remove(rv$uploaded_template$datapath)
          log4r::info(.le$logger, "Template file removed")
        }

        old_layout_files <- list.files(
          tempdir(),
          pattern = "^layout_\\d+\\.png$",
          full.names = TRUE
        )
        if (length(old_layout_files) > 0) {
          unlink(old_layout_files, force = TRUE)
          log4r::debug(.le$logger, paste("Removed old layout PNGs:", paste(old_layout_files, collapse = ", ")))
        }

        removeResourcePath("pptx_layouts")

        rv$uploaded_template <- NULL
        rv$extracted_layouts <- NULL
        rv$selected_layout_idx <- NULL

        shiny::removeModal()

        shiny::showModal(shiny::modalDialog(
          title = "Template Cleared",
          "The uploaded template has been removed, and a blank template will be used instead.",
          footer = htmltools::tagList(
            shiny::actionButton(ns("close_cleared_modal"), "Close")
          )
        ))
      })

      shiny::observeEvent(input$close_cleared_modal, {
        shiny::removeModal()
        showConfigModal() ## Show config modal after clearing
      })

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 6. Create sync modal for uploading & syncing PPTX
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      shiny::observeEvent(input$sync, {
        log4r::info(.le$logger, "Opening Sync Modal")
        shiny::showModal(shiny::modalDialog(
          title = "Sync Images",
          htmltools::tags$p("Use this menu to sync your PPTX with a local repository."),
          shiny::actionButton(ns("open_upload_sync"), "Upload File")
        ))
      })

      shiny::observeEvent(input$open_upload_sync, {
        log4r::info(.le$logger, "Opening Upload Modal for Sync Modal")
        shiny::removeModal()
        shiny::showModal(shiny::modalDialog(
          title = "Upload PPTX for Syncing",
          shiny::fileInput(ns("uploaded_sync_file"), "Choose a PPTX File for Syncing:", accept = ".pptx"),
          footer = htmltools::tagList(
            shiny::actionButton(ns("submit_sync_file"), "Submit Sync File"),
            shiny::modalButton("Close")
          )
        ))
      })

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
        log4r::debug(.le$logger, paste0("Input PPTX file: ", input_pptx))

        output_pptx <- tempfile(fileext = ".pptx")

        shiny::showModal(shiny::modalDialog(
          title = "Processing Sync",
          "Your PPTX file is being synced. This may take a few moments.",
          footer = NULL
        ))

        tryCatch({
          sync_images(input_pptx, output_pptx)

          rv$processed_file <- output_pptx

          shiny::showModal(shiny::modalDialog(
            title = "Success",
            "Images were successfully updated. Your updated presentation is ready for download.",
            footer = htmltools::tagList(
              shiny::downloadButton(ns("download_synced_pptx"), "Download Updated PPTX"),
              shiny::modalButton("Close")
            )
          ))

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
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            paste("An error occurred while processing your file:", e$message),
            footer = shiny::modalButton("Close")
          ))
        })
      })

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 7. Create initial PPTX with add_images
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
          repo_url   <- clean_url(remote_url)
          log4r::debug(.le$logger, paste("Repo URL: ", repo_url))

          current_branch <- get_current_branch()
          full_url   <- paste0(repo_url, "/blob/", current_branch)
          log4r::debug(.le$logger, paste("Full URL: ", full_url))

          shiny::showModal(shiny::modalDialog("Creating slides for PowerPoint . . .", footer = NULL))
          log4r::info(.le$logger, "Starting PowerPoint creation process")

          base_pptx <- if (!is.null(rv$uploaded_template)) rv$uploaded_template$datapath

          chosen_layout_idx <- rv$selected_layout_idx

          tryCatch({
            start_time <- Sys.time()

            add_images(
              files = selected_items(),
              repo_url = full_url,
              output_pptx = temp_pptx,
              slide_layout_index = chosen_layout_idx,
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
          htmltools::tags$h6(
            "These files reflect the current state of your local repository.
             Please commit and push any changes or new files to GitHub to
             ensure the links are up to date."
          ),
          htmltools::tags$ul(
            lapply(selected_items(), function(file) {
              htmltools::tags$li(file)
            })
          )
        )
      })
      log4r::debug(.le$logger, "pptx_server module loaded successfully")

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 8. Session close + clear layouts
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      session$onSessionEnded(function() {
        old_layout_files <- list.files(
          tempdir(),
          pattern = "^layout_\\d+\\.png$",
          full.names = TRUE
        )
        if (length(old_layout_files) > 0) {
          unlink(old_layout_files, force = TRUE)
          log4r::info(.le$logger, paste("Session ended, removed layout PNGs:", paste(old_layout_files, collapse = ", ")))
        }

        removeResourcePath("pptx_layouts")
        log4r::info(.le$logger, "Session ended -> resource path 'pptx_layouts' removed.")
      })
    }
  )
}

#' @noRd
app_server <- function(input, output, session) {
  log4r::info(.le$logger, "Initializing app server.")
  pptx_server(id="app")
}
