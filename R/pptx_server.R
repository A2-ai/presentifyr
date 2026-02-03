#' @noRd
pptx_server <- function(id) {
  selected_items <- treeNavigatorServer(
    id,
    rootFolder = getwd(),
    search = FALSE,
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
        uploaded_file = NULL,
        processed_file = NULL,
        report_filename = NULL,
        slide_groups = NULL,        ## List of vectors for multi-image slides
        pending_files = NULL        ## Files pending for preview
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
            textInput(
              ns("pptx_filename"),
              label       = "PowerPoint File Name (Extension Not Required):",
              placeholder = "presentation"
            ),
            shiny::uiOutput(ns("configs_options")),
            htmltools::hr(),
            htmltools::tags$p("Please use the following to clear a provided PPTX template:"),
            shiny::actionButton(ns("clear_template"), "Clear Template"),
            footer = shiny::modalButton("Close")
          )
        )
      }

      shiny::observeEvent(input$pptx_filename, {
        rv$report_filename <- trimws(input$pptx_filename)
      })

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
            style = "font-weight:bold; color:#e45600; margin-top:5px;",
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
        if (!is.null(rv$selected_layout_name)) {
          layout_label <- htmltools::tags$p(
            style = "font-weight:bold; color:#e45600; margin-top:5px;",
            paste("Selected Layout:", rv$selected_layout_name)
          )
        }

        filename_label <- NULL
        if (!is.null(rv$report_filename) && nzchar(rv$report_filename)) {

          display_name <- rv$report_filename
          if (!grepl("\\.pptx$", display_name, ignore.case = TRUE))
            display_name <- paste0(display_name, ".pptx")

          filename_label <- htmltools::tags$p(
            style = "font-weight:bold; color:#e45600; margin-top:5px;",
            paste("Output file name:", display_name)
          )
        }

        shiny::tagList(
          filename_label,
          file_upload_area,
          template_label,
          layout_area,
          layout_label
        )
      })

      buildLayoutSelectionUI <- function() {
        layout_divs <- lapply(seq_len(nrow(rv$extracted_layouts)), function(i) {
          layout_name <- rv$extracted_layouts$layout_name[i]
          image_path <- rv$extracted_layouts$image_path[i]
          placeholder_count <- rv$extracted_layouts$placeholder_count[i]

          ## Build placeholder count badge
          ph_badge <- if (!is.na(placeholder_count)) {
            htmltools::tags$span(
              style = "background: #e45600; color: white; padding: 2px 8px; border-radius: 10px; font-size: 0.85em;",
              paste(placeholder_count, "slot(s)")
            )
          } else {
            NULL
          }

          htmltools::tags$div(
            style = "display:inline-block; margin: 10px; text-align:center;",
            htmltools::tags$img(src = image_path, width = "150px"),
            htmltools::tags$p(paste("Layout:", layout_name), ph_badge),
            shiny::actionButton(ns(paste0("btn_layout_", layout_name)), paste("Select", layout_name))
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

        for (layout_name in rv$extracted_layouts$layout_name) {
          local({
            ln <- layout_name
            shiny::observeEvent(input[[paste0("btn_layout_", ln)]], {
              rv$selected_layout_name <- ln
              log4r::info(.le$logger, paste0("User selected layout: ", ln))
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
          pattern = "\\.png$",
          full.names = TRUE
        )
        if (length(old_layout_files) > 0) {
          unlink(old_layout_files, force = TRUE)
          log4r::debug(.le$logger, paste("Removed old layout PNGs:", paste(old_layout_files, collapse = ", ")))
        }

        shiny::removeResourcePath("pptx_layouts")

        rv$uploaded_template <- NULL
        rv$extracted_layouts <- NULL
        rv$selected_layout_name <- NULL

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
          shiny::actionButton(ns("preview_slides"), "Preview & Download")
        }
      })

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 7a. Preview modal for slide arrangement
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      shiny::observeEvent(input$preview_slides, {
        log4r::info(.le$logger, "Opening slide preview modal")

        files <- selected_items()
        rv$pending_files <- files

        ## Determine placeholder count from selected layout
        placeholder_count <- 1L
        if (!is.null(rv$extracted_layouts) && !is.null(rv$selected_layout_name)) {
          ## Convert selected layout name to safe name for lookup
          safe_name <- gsub("[^\\w\\-_]", "_", rv$selected_layout_name, perl = TRUE)
          layout_row <- rv$extracted_layouts[rv$extracted_layouts$layout_name == safe_name, ]
          if (nrow(layout_row) > 0 && !is.na(layout_row$placeholder_count[1])) {
            placeholder_count <- layout_row$placeholder_count[1]
          }
        }

        ## Create initial slide groups (auto-distribute)
        n_files <- length(files)
        n_slides <- ceiling(n_files / placeholder_count)

        slide_groups <- vector("list", n_slides)
        for (i in seq_along(files)) {
          slide_idx <- ceiling(i / placeholder_count)
          if (is.null(slide_groups[[slide_idx]])) {
            slide_groups[[slide_idx]] <- character(0)
          }
          slide_groups[[slide_idx]] <- c(slide_groups[[slide_idx]], files[i])
        }
        rv$slide_groups <- slide_groups

        log4r::debug(.le$logger, paste("Preview:", n_files, "files,", placeholder_count, "placeholders,", n_slides, "slides"))

        showPreviewModal(placeholder_count)
      })

      showPreviewModal <- function(placeholder_count) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Preview Slide Arrangement",
            size = "l",
            htmltools::tags$p(
              paste0("Layout has ", placeholder_count, " placeholder(s) per slide. ",
                     "Images will be distributed across ", length(rv$slide_groups), " slide(s).")
            ),
            htmltools::tags$p(
              style = "color: #666; font-size: 0.9em;",
              "Use the arrows to reorder images. Images are grouped into slides based on their order."
            ),
            htmltools::hr(),
            shiny::uiOutput(ns("preview_slides_ui")),
            footer = htmltools::tagList(
              shiny::downloadButton(ns("download"), "Generate PPTX"),
              shiny::modalButton("Cancel")
            )
          )
        )
      }

      output$preview_slides_ui <- shiny::renderUI({
        shiny::req(rv$slide_groups)

        slide_divs <- lapply(seq_along(rv$slide_groups), function(slide_idx) {
          slide_files <- rv$slide_groups[[slide_idx]]

          file_items <- lapply(seq_along(slide_files), function(file_idx) {
            file <- slide_files[file_idx]
            global_idx <- sum(sapply(rv$slide_groups[seq_len(slide_idx - 1)], length)) + file_idx

            htmltools::tags$div(
              style = "display: flex; align-items: center; padding: 5px; margin: 2px 0; background: #f5f5f5; border-radius: 4px;",
              htmltools::tags$span(
                style = "flex-grow: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;",
                basename(file)
              ),
              shiny::actionButton(
                ns(paste0("move_up_", global_idx)),
                shiny::icon("arrow-up"),
                class = "btn-sm btn-outline-secondary",
                style = "margin-left: 5px;",
                disabled = global_idx == 1
              ),
              shiny::actionButton(
                ns(paste0("move_down_", global_idx)),
                shiny::icon("arrow-down"),
                class = "btn-sm btn-outline-secondary",
                style = "margin-left: 5px;",
                disabled = global_idx == length(rv$pending_files)
              )
            )
          })

          htmltools::tags$div(
            style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 4px;",
            htmltools::tags$h5(paste("Slide", slide_idx), style = "margin-top: 0;"),
            do.call(htmltools::tagList, file_items)
          )
        })

        do.call(htmltools::tagList, slide_divs)
      })

      ## Observers for move up/down buttons
      shiny::observe({
        shiny::req(rv$pending_files)

        for (i in seq_along(rv$pending_files)) {
          local({
            idx <- i

            shiny::observeEvent(input[[paste0("move_up_", idx)]], {
              if (idx > 1) {
                files <- unlist(rv$slide_groups)
                files[c(idx - 1, idx)] <- files[c(idx, idx - 1)]
                rv$slide_groups <- regroup_files(files, rv$slide_groups)
              }
            }, ignoreInit = TRUE)

            shiny::observeEvent(input[[paste0("move_down_", idx)]], {
              files <- unlist(rv$slide_groups)
              if (idx < length(files)) {
                files[c(idx, idx + 1)] <- files[c(idx + 1, idx)]
                rv$slide_groups <- regroup_files(files, rv$slide_groups)
              }
            }, ignoreInit = TRUE)
          })
        }
      })

      ## Helper to regroup files maintaining slide sizes
      regroup_files <- function(files, current_groups) {
        sizes <- sapply(current_groups, length)
        new_groups <- vector("list", length(sizes))
        file_idx <- 1
        for (i in seq_along(sizes)) {
          new_groups[[i]] <- files[file_idx:(file_idx + sizes[i] - 1)]
          file_idx <- file_idx + sizes[i]
        }
        new_groups
      }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 7b. Download handler (now triggered from preview modal)
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      output$download <- shiny::downloadHandler(
        filename = function() {
          base <- rv$report_filename
          if (is.null(base) || base == "") {
            "report.pptx"
          } else {
            base <- gsub("[^[:alnum:]_ -]", "_", base)
            if (!grepl("\\.pptx$", base, ignore.case = TRUE))
              base <- paste0(base, ".pptx")
            base
          }
        },
        content = function(file) {
          temp_pptx <- tempfile(fileext = ".pptx")

          shiny::removeModal()
          shiny::showModal(shiny::modalDialog("Creating slides for PowerPoint . . .", footer = NULL))
          log4r::info(.le$logger, "Starting PowerPoint creation process")

          base_pptx <- if (!is.null(rv$uploaded_template)) rv$uploaded_template$datapath

          chosen_layout <- rv$selected_layout_name

          tryCatch({
            start_time <- Sys.time()

            add_images(
              files = unlist(rv$slide_groups),  ## Flatten for backward compat if needed
              output_pptx = temp_pptx,
              slide_layout_name = chosen_layout,
              base_pptx = base_pptx,
              slide_groups = rv$slide_groups    ## Pass groupings for multi-image
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
            "These files reflect the current state of your local repository.",
            htmltools::tags$br(),
            htmltools::tags$br(),
             "Please commit and push any changes or new files to GitHub to
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
          pattern = "\\.png$",
          full.names = TRUE
        )
        if (length(old_layout_files) > 0) {
          unlink(old_layout_files, force = TRUE)
          log4r::info(.le$logger, paste("Session ended, removed layout PNGs:", paste(old_layout_files, collapse = ", ")))
        }

        if ("pptx_layouts" %in% names(shiny::resourcePaths())) {
          shiny::removeResourcePath("pptx_layouts")
        }
        log4r::info(.le$logger, "Session ended, removed resource path 'pptx_layouts'")
      })
    }
  )
}

#' @noRd
app_server <- function(input, output, session) {
  log4r::info(.le$logger, "Initializing app server")
  pptx_server(id="app")
}
