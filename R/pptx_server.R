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
      default_footnote_font <- list(
        font_name      = "Calibri",
        font_size      = 8,
        bold           = FALSE,
        italic         = FALSE,
        underline      = FALSE,
        font_color     = "#000000"
      )

      rv <- shiny::reactiveValues(
        uploaded_template = NULL,
        extracted_layouts = NULL,
        uploaded_file = NULL,
        processed_file = NULL,
        report_filename = NULL,
        slide_groups = NULL,        ## List of vectors for multi-image slides
        slide_positions = NULL,     ## List of integer vectors - which placeholder each image goes to
        pending_files = NULL,       ## Files pending for preview
        placeholder_count = 1L,     ## Number of placeholders in selected layout
        img_dirs = NULL,            ## Directories for image resource paths
        file_input_key = 0L,        ## Counter to force fileInput re-render on clear
        config_modal_key = 0L,      ## Counter to refresh modal UI only on open
        footnote_font = default_footnote_font  ## Footnote font formatting settings
      )

      update_footnote_font <- function(...) {
        updates <- list(...)
        rv$footnote_font <- modifyList(rv$footnote_font, updates)
      }

      persist_footnote_font_inputs <- function() {
        if (!is.null(input$fn_font_name) && nzchar(input$fn_font_name)) {
          update_footnote_font(font_name = input$fn_font_name)
        }
        if (!is.null(input$fn_font_size) && is.numeric(input$fn_font_size)) {
          update_footnote_font(font_size = input$fn_font_size)
        }
        if (!is.null(input$fn_bold)) {
          update_footnote_font(bold = isTRUE(input$fn_bold))
        }
        if (!is.null(input$fn_italic)) {
          update_footnote_font(italic = isTRUE(input$fn_italic))
        }
        if (!is.null(input$fn_underline)) {
          update_footnote_font(underline = isTRUE(input$fn_underline))
        }
        if (!is.null(input$fn_font_color)) {
          color <- trimws(input$fn_font_color)
          if (grepl("^#[0-9A-Fa-f]{6}$", color)) {
            update_footnote_font(font_color = color)
          }
        }
      }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 1. Create configs modal for uploading & configuring PPTX
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      shiny::observeEvent(input$configs, {
        log4r::info(.le$logger, "Opening Configuration Modal")
        showConfigModal()
      })

      showConfigModal <- function() {
        rv$config_modal_key <- rv$config_modal_key + 1L
        fs <- shiny::isolate(rv$footnote_font)
        shiny::showModal(
          shiny::modalDialog(
            title = "Customize PowerPoint Configuration",
            ## Reduce spacing below modal title
            htmltools::tags$style("
              .modal-header { padding-bottom: 10px; margin-bottom: 0; }
              .modal-body { padding-top: 10px; }
            "),
            shiny::uiOutput(ns("filename_input_ui")),
            shiny::uiOutput(ns("filename_label_ui")),
            shiny::uiOutput(ns("configs_options")),
            shiny::uiOutput(ns("footnote_font_ui")),
            easyClose = FALSE,
            footer = shiny::actionButton(ns("close_configs"), "Close")
          )
        )

        ## Push saved state into controls after modal UI is mounted.
        shiny::onFlushed(function() {
          shiny::updateSelectInput(session, "fn_font_name", selected = fs$font_name)
          shiny::updateNumericInput(session, "fn_font_size", value = fs$font_size)
          shiny::updateCheckboxInput(session, "fn_bold", value = isTRUE(fs$bold))
          shiny::updateCheckboxInput(session, "fn_italic", value = isTRUE(fs$italic))
          shiny::updateCheckboxInput(session, "fn_underline", value = isTRUE(fs$underline))
          shiny::updateTextInput(session, "fn_font_color", value = fs$font_color)
        }, once = TRUE)
      }

      shiny::observeEvent(input$close_configs, {
        persist_footnote_font_inputs()
        shiny::removeModal()
      })

      ## Render filename input with conditional clear button
      output$filename_input_ui <- shiny::renderUI({
        current_value <- if (!is.null(rv$report_filename)) rv$report_filename else ""
        has_value <- nzchar(current_value)

        htmltools::tags$div(
          class = "form-group shiny-input-container",
          htmltools::tags$label(
            class = "control-label",
            `for` = ns("pptx_filename"),
            "PowerPoint File Name (Extension Not Required):"
          ),
          htmltools::tags$div(
            class = "input-with-clear",
            shiny::textInput(
              ns("pptx_filename"),
              label = NULL,
              value = current_value,
              placeholder = "presentation"
            ),
            if (has_value) {
              shiny::actionButton(
                ns("clear_filename"),
                htmltools::tags$i(class = "fa fa-times"),
                class = "btn-clear-input"
              )
            }
          )
        )
      })

      ## Clear filename button
      shiny::observeEvent(input$clear_filename, {
        rv$report_filename <- ""
      })

      shiny::observeEvent(input$pptx_filename, {
        rv$report_filename <- trimws(input$pptx_filename)
      })

      ## Render filename label separately so it doesn't affect template upload
      output$filename_label_ui <- shiny::renderUI({
        if (!is.null(rv$report_filename) && nzchar(rv$report_filename)) {
          display_name <- rv$report_filename
          if (!grepl("\\.pptx$", display_name, ignore.case = TRUE)) {
            display_name <- paste0(display_name, ".pptx")
          }
          htmltools::tags$p(
            style = "font-weight:bold; color:#e45600; margin-top:5px;",
            paste("Output file name:", display_name)
          )
        }
      })

      ## Separate renderUI for submit/clear buttons so fileInput doesn't re-render
      output$submit_template_btn <- shiny::renderUI({
        file_input_id <- paste0("uploaded_template_", rv$file_input_key)
        file_ready <- !is.null(input[[file_input_id]])
        template_loaded <- !is.null(rv$uploaded_template)
        htmltools::tags$div(
          class = "form-group shiny-input-container",
          style = "display: flex; gap: 10px;",
          shiny::actionButton(
            ns("submit_template"),
            "Submit Template",
            disabled = !file_ready,
            style = "flex: 1;"
          ),
          shiny::actionButton(
            ns("clear_template"),
            "Clear Template",
            class = "btn-outline-secondary",
            disabled = !template_loaded,
            style = "flex: 1;"
          )
        )
      })

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 2. UI for file input, template confirmation, layout selection
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      output$configs_options <- shiny::renderUI({
        log4r::info(.le$logger, "Rendering UI for PPTX configuration & layout selection")

        ## Force fileInput re-render when key changes (e.g., after clearing template)
        file_key <- rv$file_input_key

        file_upload_area <- shiny::tagList(
          shiny::fileInput(
            ns(paste0("uploaded_template_", file_key)),
            "Choose a PowerPoint Template for Adding Images:",
            accept = ".pptx"
          ),
          shiny::uiOutput(ns("submit_template_btn"))
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
            paste("Selected layout:", rv$selected_layout_name)
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
        htmltools::tags$div(
          class = "form-group shiny-input-container",
          style = "margin-top: 10px;",
          htmltools::tags$label(
            class = "control-label",
            "Select a Layout:"
          ),
          shiny::uiOutput(ns("layout_carousel"))
        )
      }

      ## Initialize carousel index
      rv$layout_index <- 1L

      ## Render carousel layout selector
      output$layout_carousel <- shiny::renderUI({
        shiny::req(rv$extracted_layouts)
        shiny::req(nrow(rv$extracted_layouts) > 0)

        idx <- rv$layout_index
        total <- nrow(rv$extracted_layouts)

        layout_name <- rv$extracted_layouts$layout_name[idx]
        image_path <- rv$extracted_layouts$image_path[idx]
        placeholder_count <- rv$extracted_layouts$placeholder_count[idx]

        htmltools::tags$div(
          class = "layout-carousel",
          ## Navigation row
          htmltools::tags$div(
            class = "layout-carousel-nav",
            shiny::actionButton(
              ns("layout_prev"),
              label = htmltools::tags$i(class = "fa fa-chevron-left"),
              class = "btn-carousel",
              disabled = idx <= 1
            ),
            htmltools::tags$span(
              class = "layout-carousel-counter",
              paste(idx, "/", total)
            ),
            shiny::actionButton(
              ns("layout_next"),
              label = htmltools::tags$i(class = "fa fa-chevron-right"),
              class = "btn-carousel",
              disabled = idx >= total
            )
          ),
          ## Layout preview
          htmltools::tags$div(
            class = "layout-carousel-preview",
            htmltools::tags$img(src = image_path, class = "layout-carousel-img"),
            htmltools::tags$div(
              class = "layout-carousel-info",
              htmltools::tags$div(class = "layout-carousel-name", layout_name),
              htmltools::tags$span(
                class = "layout-carousel-slots",
                paste(placeholder_count, ifelse(placeholder_count == 1, "slot", "slots"))
              )
            )
          ),
          ## Select button
          shiny::actionButton(
            ns("layout_select_btn"),
            label = if (identical(rv$selected_layout_name, layout_name)) {
              htmltools::tagList(htmltools::tags$i(class = "fa fa-check"), " Selected")
            } else {
              "Select This Layout"
            },
            class = if (identical(rv$selected_layout_name, layout_name)) "btn-selected" else "btn-select",
            width = "100%"
          )
        )
      })

      ## Navigate previous
      shiny::observeEvent(input$layout_prev, {
        if (rv$layout_index > 1) {
          rv$layout_index <- rv$layout_index - 1L
        }
      })

      ## Navigate next
      shiny::observeEvent(input$layout_next, {
        if (rv$layout_index < nrow(rv$extracted_layouts)) {
          rv$layout_index <- rv$layout_index + 1L
        }
      })

      ## Select current layout
      shiny::observeEvent(input$layout_select_btn, {
        shiny::req(rv$extracted_layouts)
        rv$selected_layout_name <- rv$extracted_layouts$layout_name[rv$layout_index]
        log4r::info(.le$logger, paste0("User selected layout: ", rv$selected_layout_name))
      })

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 2b. Footnote Font Settings (collapsible section in config modal)
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      output$footnote_font_ui <- shiny::renderUI({
        rv$config_modal_key
        fs <- shiny::isolate(rv$footnote_font)

        htmltools::tags$details(
          class = "footnote-font-settings",
          htmltools::tags$summary("Footnote Font Settings"),
          htmltools::tags$div(
            class = "footnote-font-grid",
            ## Font family
            shiny::selectInput(
              ns("fn_font_name"),
              "Font Family",
              choices = c("Calibri", "Arial", "Times New Roman", "Helvetica",
                          "Cambria", "Georgia", "Verdana", "Tahoma",
                          "Consolas", "Courier New"),
              selected = fs$font_name
            ),
            ## Font size
            shiny::numericInput(
              ns("fn_font_size"),
              "Font Size (pt)",
              value = fs$font_size,
              min = 4, max = 72, step = 1
            ),
            ## Bold / Italic / Underline toggles in a row
            htmltools::tags$div(
              class = "footnote-font-toggles",
              htmltools::tags$label(class = "control-label", "Style"),
              htmltools::tags$div(
                class = "footnote-toggle-row",
                shiny::checkboxInput(ns("fn_bold"), "Bold", value = fs$bold),
                shiny::checkboxInput(ns("fn_italic"), "Italic", value = fs$italic),
                shiny::checkboxInput(ns("fn_underline"), "Underline", value = fs$underline)
              )
            ),
            ## Font color
            shiny::textInput(
              ns("fn_font_color"),
              "Font Color",
              value = fs$font_color
            ),
            htmltools::tags$script(htmltools::HTML(sprintf(
              "var el = document.getElementById('%s');
               el.type = 'color';
               el.addEventListener('input', function() {
                 Shiny.setInputValue('%s', el.value);
               });",
              ns("fn_font_color"), ns("fn_font_color")
            )))
          )
        )
      })

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 3. Submit template - extract layouts
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      shiny::observeEvent(input$submit_template, {
        file_input_id <- paste0("uploaded_template_", rv$file_input_key)
        rv$uploaded_template <- input[[file_input_id]]

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

        output_dir <- file.path(tempdir(), "prfy_layouts")
        dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
        ## Clean old layout files before extracting new ones
        old_files <- list.files(output_dir, full.names = TRUE)
        if (length(old_files) > 0) unlink(old_files, force = TRUE)
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

        layout_dir <- file.path(tempdir(), "prfy_layouts")
        if (dir.exists(layout_dir)) {
          unlink(layout_dir, recursive = TRUE, force = TRUE)
          log4r::debug(.le$logger, paste("Removed layout directory:", layout_dir))
        }

        shiny::removeResourcePath("pptx_layouts")

        rv$uploaded_template <- NULL
        rv$extracted_layouts <- NULL
        rv$selected_layout_name <- NULL
        rv$file_input_key <- rv$file_input_key + 1L  ## Force fileInput to re-render with new ID

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
          htmltools::tags$p("Use this menu to sync your PowerPoint with a local repository."),
          shiny::actionButton(ns("open_upload_sync"), "Upload File"),
          footer = shiny::modalButton("Close")
        ))
      })

      shiny::observeEvent(input$open_upload_sync, {
        log4r::info(.le$logger, "Opening Upload Modal for Sync Modal")
        shiny::removeModal()
        shiny::showModal(shiny::modalDialog(
          title = "Upload PowerPoint for Syncing",
          shiny::fileInput(ns("uploaded_sync_file"), "Choose a PowerPoint File for Syncing:", accept = ".pptx"),
          footer = htmltools::tagList(
            htmltools::tags$span(
              title = "Sync and download the updated PowerPoint",
              shiny::actionButton(ns("submit_sync_file"), htmltools::tagList(shiny::icon("sync"), "Sync PowerPoint"), class = "btn-sync")
            ),
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
            "No PowerPoint file was uploaded for syncing. Please try again.",
            footer = shiny::modalButton("Close")
          ))
          return()
        }

        input_pptx <- rv$uploaded_file$datapath
        log4r::debug(.le$logger, paste0("Input PPTX file: ", input_pptx))

        output_pptx <- tempfile(fileext = ".pptx")

        shiny::showModal(shiny::modalDialog(
          title = "Processing Sync",
          "Your PowerPoint file is being synced. This may take a few moments.",
          footer = NULL
        ))

        tryCatch({
          sync_images(input_pptx, output_pptx)

          rv$processed_file <- output_pptx

          shiny::showModal(shiny::modalDialog(
            title = "Success",
            "Images were successfully updated. Your updated presentation is ready for download.",
            footer = htmltools::tagList(
              shiny::downloadButton(ns("download_synced_pptx"), "Download Updated PowerPoint"),
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
        files_selected <- length(selected_items()) > 0
        htmltools::tags$span(
          title = "Arrange slides and generate PowerPoint",
          shiny::actionButton(
            ns("preview_slides"),
            if (files_selected) "Preview & Download" else "No files selected",
            disabled = !files_selected,
            width = "100%"
          )
        )
      })

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 7a. Preview modal for slide arrangement
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      shiny::observeEvent(input$preview_slides, {
        log4r::info(.le$logger, "Opening slide preview modal")

        selected <- selected_items()
        ## selected_items() returns list of lists with 'path' element - extract and make absolute
        files <- vapply(selected, function(item) {
          f <- if (is.list(item)) item$path else item
          if (startsWith(f, "/") || grepl("^[A-Za-z]:", f)) {
            f
          } else {
            file.path(getwd(), f)
          }
        }, character(1), USE.NAMES = FALSE)
        rv$pending_files <- files

        ## Set up resource path for image thumbnails
        img_dirs <- unique(dirname(files))
        for (i in seq_along(img_dirs)) {
          path_name <- paste0("preview_imgs_", i)
          if (!(path_name %in% names(shiny::resourcePaths()))) {
            shiny::addResourcePath(path_name, img_dirs[i])
          }
        }
        rv$img_dirs <- img_dirs  ## Store for use in renderUI

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
        rv$placeholder_count <- placeholder_count

        ## Create initial slide groups (auto-distribute)
        n_files <- length(files)
        n_slides <- ceiling(n_files / placeholder_count)

        slide_groups <- vector("list", n_slides)
        slide_positions <- vector("list", n_slides)
        for (i in seq_along(files)) {
          slide_idx <- ceiling(i / placeholder_count)
          if (is.null(slide_groups[[slide_idx]])) {
            slide_groups[[slide_idx]] <- character(0)
            slide_positions[[slide_idx]] <- integer(0)
          }
          slide_groups[[slide_idx]] <- c(slide_groups[[slide_idx]], files[i])
          ## Default position: 1, 2, 3... in order
          pos_in_slide <- length(slide_groups[[slide_idx]])
          slide_positions[[slide_idx]] <- c(slide_positions[[slide_idx]], pos_in_slide)
        }
        rv$slide_groups <- slide_groups
        rv$slide_positions <- slide_positions

        log4r::debug(.le$logger, paste("Preview:", n_files, "files,", placeholder_count, "placeholders,", n_slides, "slides"))

        showPreviewModal(placeholder_count)
      })

      showPreviewModal <- function(placeholder_count) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Preview Slide Arrangement",
            size = "l",
            ## Styles for image thumbnails and lightbox
            htmltools::tags$style(htmltools::HTML("
              .preview-thumbnail {
                width: 60px;
                height: 45px;
                object-fit: cover;
                border-radius: 4px;
                margin-right: 10px;
                border: 1px solid #ddd;
                cursor: pointer;
                transition: transform 0.15s, box-shadow 0.15s;
              }
              .preview-thumbnail:hover {
                transform: scale(1.1);
                box-shadow: 0 2px 8px rgba(0,0,0,0.2);
              }
              /* Lightbox overlay */
              .img-lightbox {
                display: none;
                position: fixed;
                top: 0;
                left: 0;
                width: 100%;
                height: 100%;
                background: rgba(0,0,0,0.85);
                z-index: 10000;
                justify-content: center;
                align-items: center;
                cursor: pointer;
              }
              .img-lightbox.active {
                display: flex;
              }
              .img-lightbox img {
                max-width: 90%;
                max-height: 90%;
                border-radius: 8px;
                box-shadow: 0 4px 20px rgba(0,0,0,0.5);
              }
              .img-lightbox .close-hint {
                position: absolute;
                top: 20px;
                right: 30px;
                color: white;
                font-size: 1.2em;
              }
            ")),
            ## Lightbox container (will be populated by JS)
            htmltools::tags$div(
              id = "img-lightbox",
              class = "img-lightbox",
              onclick = "this.classList.remove('active');",
              htmltools::tags$span(class = "close-hint", "Click anywhere to close"),
              htmltools::tags$img(id = "lightbox-img", src = "", alt = "Preview")
            ),
            ## JavaScript to handle thumbnail clicks
            htmltools::tags$script(htmltools::HTML("
              $(document).on('click', '.preview-thumbnail', function() {
                var src = $(this).attr('src');
                $('#lightbox-img').attr('src', src);
                $('#img-lightbox').addClass('active');
              });
            ")),
            htmltools::tags$p(
              paste0("Layout has ", placeholder_count, " placeholder(s) per slide. ",
                     "Initially, images will be distributed across ", length(rv$slide_groups), " slide(s).")
            ),
            htmltools::tags$p(
              style = "color: #666; font-size: 0.9em;",
              "Use the arrows to reorder images. Images are grouped into slides based on their order."
            ),
            htmltools::hr(),
            shiny::uiOutput(ns("preview_slides_ui")),
            footer = htmltools::tagList(
              htmltools::tags$span(
                title = "Generate and download the PowerPoint file",
                shiny::downloadButton(ns("download"), "Generate PowerPoint")
              ),
              shiny::modalButton("Cancel")
            )
          )
        )
      }

      ## Helper to get web-accessible path for an image file
      get_image_web_path <- function(file_path) {
        dir_path <- dirname(file_path)
        dir_idx <- which(rv$img_dirs == dir_path)[1]
        if (!is.na(dir_idx)) {
          paste0("preview_imgs_", dir_idx, "/", basename(file_path))
        } else {
          ## Fallback - shouldn't happen if paths are set up correctly
          log4r::warn(.le$logger, paste("Could not find resource path for:", file_path))
          ""
        }
      }

      output$preview_slides_ui <- shiny::renderUI({
        shiny::req(rv$slide_groups, rv$slide_positions)

        total_files <- length(rv$pending_files)
        ph_count <- rv$placeholder_count

        slide_divs <- lapply(seq_along(rv$slide_groups), function(slide_idx) {
          slide_files <- rv$slide_groups[[slide_idx]]
          slide_pos <- rv$slide_positions[[slide_idx]]
          n_images <- length(slide_files)

          ## Show position selector if fewer images than placeholders
          slide_has_room <- n_images < ph_count
          show_position_selector <- slide_has_room && ph_count > 1

          ## Hide up/down arrows when slide has room (use slot selector instead)
          show_reorder_arrows <- !slide_has_room

          file_items <- lapply(seq_along(slide_files), function(file_idx) {
            file <- slide_files[file_idx]
            current_pos <- slide_pos[file_idx]

            ## Calculate global index across all slides
            prior_count <- if (slide_idx == 1) 0L else sum(lengths(rv$slide_groups[seq_len(slide_idx - 1)]))
            global_idx <- prior_count + file_idx

            ## Show split button on all images except the first in each slide
            ## "Split before" semantics: this image and everything after moves to new slide
            show_split <- file_idx > 1

            ## Position selector buttons (only if room to choose)
            position_btns <- NULL
            if (show_position_selector) {
              position_btns <- htmltools::tags$span(
                style = "margin-left: 10px; margin-right: 5px;",
                htmltools::tags$span("Slot:", style = "font-size: 0.85em; color: #666; margin-right: 3px;"),
                lapply(seq_len(ph_count), function(p) {
                  btn_class <- if (p == current_pos) "btn-sm btn-primary" else "btn-sm btn-slot-unselected"
                  shiny::actionButton(
                    ns(paste0("pos_", slide_idx, "_", file_idx, "_", p)),
                    as.character(p),
                    class = btn_class,
                    style = "padding: 2px 8px; margin: 0 1px;",
                    title = paste("Place image in slot", p)
                  )
                })
              )
            }

            ## Reorder arrows (only for full slides, hide when not usable)
            reorder_btns <- NULL
            if (show_reorder_arrows) {
              up_btn <- if (global_idx > 1) {
                shiny::actionButton(
                  ns(paste0("move_up_", global_idx)),
                  shiny::icon("arrow-up"),
                  class = "btn-sm btn-outline-secondary",
                  style = "margin-left: 5px;",
                  title = "Move image up in order"
                )
              }
              down_btn <- if (global_idx < total_files) {
                shiny::actionButton(
                  ns(paste0("move_down_", global_idx)),
                  shiny::icon("arrow-down"),
                  class = "btn-sm btn-outline-secondary",
                  style = "margin-left: 5px;",
                  title = "Move image down in order"
                )
              }
              reorder_btns <- htmltools::tagList(up_btn, down_btn)
            }

            ## Get web path for thumbnail
            web_path <- get_image_web_path(file)

            htmltools::tags$div(
              style = "display: flex; align-items: center; justify-content: flex-start; padding: 5px; margin: 2px 0; background: #f5f5f5; border-radius: 4px;",
              ## Image thumbnail
              htmltools::tags$img(
                src = web_path,
                alt = basename(file),
                class = "preview-thumbnail"
              ),
              htmltools::tags$span(
                style = "flex-grow: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;",
                title = basename(file),
                basename(file)
              ),
              position_btns,
              reorder_btns,
              if (show_split) {
                shiny::actionButton(
                  ns(paste0("split_before_", global_idx)),
                  shiny::icon("level-down-alt"),
                  class = "btn-sm btn-new-slide",
                  style = "margin-left: 5px;",
                  title = "Move this image to a new slide"
                )
              }
            )
          })

          ## Add merge button only if this slide has room AND isn't the last slide
          merge_btn <- NULL
          if (slide_has_room && slide_idx < length(rv$slide_groups)) {
            merge_btn <- htmltools::tags$div(
              style = "text-align: right; margin-top: 5px;",
              shiny::actionButton(
                ns(paste0("merge_slide_", slide_idx)),
                htmltools::tagList(shiny::icon("compress-alt"), " Merge with next"),
                class = "btn-sm btn-outline-secondary",
                title = "Merge this slide with the next slide"
              )
            )
          }

          htmltools::tags$div(
            style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 4px; text-align: left;",
            htmltools::tags$h5(paste("Slide", slide_idx), style = "margin-top: 0;"),
            do.call(htmltools::tagList, file_items),
            merge_btn
          )
        })

        htmltools::tags$div(
          style = "text-align: left;",
          do.call(htmltools::tagList, slide_divs)
        )
      })

      ## Track created observers to prevent duplicates
      .created_obs <- new.env(parent = emptyenv())
      .created_obs$move <- integer(0)
      .created_obs$merge <- integer(0)
      .created_obs$pos <- character(0)

      ## Observers for move up/down buttons
      shiny::observe({
        shiny::req(rv$pending_files)

        for (i in seq_along(rv$pending_files)) {
          if (!(i %in% .created_obs$move)) {
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

              ## Split before: this image and everything after moves to new slide
              shiny::observeEvent(input[[paste0("split_before_", idx)]], {
                ## Find which slide this index is in and split it
                rv$slide_groups <- split_before_index(rv$slide_groups, idx)
              }, ignoreInit = TRUE)
            })
            .created_obs$move <- c(.created_obs$move, i)
          }
        }
      })

      ## Observers for merge buttons
      shiny::observe({
        shiny::req(rv$slide_groups)

        for (s in seq_along(rv$slide_groups)) {
          if (!(s %in% .created_obs$merge)) {
            local({
              slide_idx <- s

              shiny::observeEvent(input[[paste0("merge_slide_", slide_idx)]], {
                if (slide_idx < length(rv$slide_groups)) {
                  result <- merge_slides(rv$slide_groups, rv$slide_positions, slide_idx)
                  rv$slide_groups <- result$groups
                  rv$slide_positions <- result$positions
                }
              }, ignoreInit = TRUE)
            })
            .created_obs$merge <- c(.created_obs$merge, s)
          }
        }
      })

      ## Observers for position selector buttons
      shiny::observe({
        shiny::req(rv$slide_groups, rv$slide_positions)

        for (s in seq_along(rv$slide_groups)) {
          for (f in seq_along(rv$slide_groups[[s]])) {
            for (p in seq_len(rv$placeholder_count)) {
              obs_key <- paste0(s, "_", f, "_", p)
              if (!(obs_key %in% .created_obs$pos)) {
                local({
                  slide_idx <- s
                  file_idx <- f
                  pos <- p

                  shiny::observeEvent(input[[paste0("pos_", slide_idx, "_", file_idx, "_", pos)]], {
                    rv$slide_positions[[slide_idx]][file_idx] <- pos
                  }, ignoreInit = TRUE)
                })
                .created_obs$pos <- c(.created_obs$pos, obs_key)
              }
            }
          }
        }
      })

      ## Helper to regroup files maintaining slide sizes (also resets positions)
      regroup_files <- function(files, current_groups) {
        sizes <- lengths(current_groups)
        new_groups <- vector("list", length(sizes))
        new_positions <- vector("list", length(sizes))
        file_idx <- 1
        for (i in seq_along(sizes)) {
          new_groups[[i]] <- files[file_idx:(file_idx + sizes[i] - 1)]
          ## Reset positions to 1, 2, 3... when reordering
          new_positions[[i]] <- seq_len(sizes[i])
          file_idx <- file_idx + sizes[i]
        }
        rv$slide_positions <- new_positions
        new_groups
      }

      ## Helper to split slides BEFORE a global file index
      ## "Split before" semantics: this image and everything after moves to new slide
      split_before_index <- function(groups, global_idx) {
        cumulative <- cumsum(lengths(groups))

        ## Find which slide contains this index
        slide_idx <- which(cumulative >= global_idx)[1]
        prior <- if (slide_idx == 1) 0 else cumulative[slide_idx - 1]
        local_idx <- global_idx - prior

        ## Split the slide: before gets images 1 to local_idx-1, after gets local_idx to end
        slide_files <- groups[[slide_idx]]
        before_files <- slide_files[seq_len(local_idx - 1)]
        after_files <- slide_files[local_idx:length(slide_files)]

        ## Rebuild groups
        new_groups <- list()
        new_positions <- list()

        if (slide_idx > 1) {
          new_groups <- groups[seq_len(slide_idx - 1)]
          new_positions <- rv$slide_positions[seq_len(slide_idx - 1)]
        }

        ## Add before_files (will have at least 1 image since button only shows for file_idx > 1)
        new_groups <- c(new_groups, list(before_files))
        new_positions <- c(new_positions, list(seq_len(length(before_files))))

        ## Add after_files (this image and any following in the same slide)
        new_groups <- c(new_groups, list(after_files))
        new_positions <- c(new_positions, list(seq_len(length(after_files))))

        ## Add remaining slides
        if (slide_idx < length(groups)) {
          new_groups <- c(new_groups, groups[(slide_idx + 1):length(groups)])
          new_positions <- c(new_positions, rv$slide_positions[(slide_idx + 1):length(groups)])
        }

        rv$slide_positions <- new_positions
        new_groups
      }

      ## Helper to merge slide with next
      merge_slides <- function(groups, positions, slide_idx) {
        if (slide_idx >= length(groups)) {
          return(list(groups = groups, positions = positions))
        }

        merged_files <- c(groups[[slide_idx]], groups[[slide_idx + 1]])
        ## Reassign positions sequentially when merging
        merged_positions <- seq_len(length(merged_files))

        new_groups <- list()
        new_positions <- list()

        if (slide_idx > 1) {
          new_groups <- groups[seq_len(slide_idx - 1)]
          new_positions <- positions[seq_len(slide_idx - 1)]
        }

        new_groups <- c(new_groups, list(merged_files))
        new_positions <- c(new_positions, list(merged_positions))

        if (slide_idx + 1 < length(groups)) {
          new_groups <- c(new_groups, groups[(slide_idx + 2):length(groups)])
          new_positions <- c(new_positions, positions[(slide_idx + 2):length(groups)])
        }

        list(groups = new_groups, positions = new_positions)
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
              slide_groups = rv$slide_groups,   ## Pass groupings for multi-image
              slide_positions = rv$slide_positions,  ## Pass position preferences
              font_settings = rv$footnote_font
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
        ## Check if we have any PowerPoint details to show
        has_filename <- !is.null(rv$report_filename) && nzchar(rv$report_filename)
        has_template <- !is.null(rv$uploaded_template)
        has_footnote_settings <- !identical(rv$footnote_font, default_footnote_font)
        files <- selected_items()
        has_files <- length(files) > 0

        ## Empty state — nothing configured and no files selected
        if (!has_filename && !has_template && !has_footnote_settings && !has_files) {
          return(htmltools::tags$div(
            class = "empty-state",
            htmltools::tags$div(class = "empty-state-icon", shiny::icon("sliders")),
            htmltools::tags$div(
              class = "empty-state-text",
              "Configure your presentation in Settings, then select images from the file tree."
            )
          ))
        }

        ## PowerPoint details card
        pptx_details <- NULL
        if (has_filename || has_template || has_footnote_settings) {
          detail_rows <- list()
          fs <- rv$footnote_font
          style_flags <- c(
            if (isTRUE(fs$bold)) "Bold",
            if (isTRUE(fs$italic)) "Italic",
            if (isTRUE(fs$underline)) "Underline"
          )
          style_text <- if (length(style_flags) > 0) paste(style_flags, collapse = ", ") else "Regular"
          font_summary <- paste0(fs$font_name, " ", fs$font_size, "pt (", style_text, ", ", fs$font_color, ")")

          if (has_filename) {
            display_name <- rv$report_filename
            if (!grepl("\\.pptx$", display_name, ignore.case = TRUE)) {
              display_name <- paste0(display_name, ".pptx")
            }
            detail_rows <- c(detail_rows, list(
              htmltools::tags$div(
                class = "pptx-detail-row",
                htmltools::tags$span(class = "detail-icon", shiny::icon("file")),
                htmltools::tags$span(class = "detail-label", "Output"),
                htmltools::tags$span(class = "detail-value", display_name)
              )
            ))
          }

          if (has_template) {
            template_name <- basename(rv$uploaded_template$name)
            detail_rows <- c(detail_rows, list(
              htmltools::tags$div(
                class = "pptx-detail-row",
                htmltools::tags$span(class = "detail-icon", shiny::icon("file-powerpoint")),
                htmltools::tags$span(class = "detail-label", "Template"),
                htmltools::tags$span(class = "detail-value", template_name)
              )
            ))
          }

          detail_rows <- c(detail_rows, list(
            htmltools::tags$div(
              class = "pptx-detail-row",
              htmltools::tags$span(class = "detail-icon", shiny::icon("font")),
              htmltools::tags$span(class = "detail-label", "Footnotes"),
              htmltools::tags$span(class = "detail-value", font_summary)
            )
          ))

          ## Layout preview
          layout_preview <- NULL
          if (!is.null(rv$selected_layout_name) && !is.null(rv$extracted_layouts)) {
            layout_row <- rv$extracted_layouts[rv$extracted_layouts$layout_name == rv$selected_layout_name, ]
            if (nrow(layout_row) > 0) {
              image_path <- layout_row$image_path[1]
              slot_count <- layout_row$placeholder_count[1]
              slot_badge <- NULL
              if (!is.na(slot_count)) {
                slot_badge <- htmltools::tags$span(
                  class = "layout-carousel-slots",
                  paste0(slot_count, if (slot_count == 1L) " slot" else " slots")
                )
              }
              layout_preview <- htmltools::tags$div(
                class = "pptx-layout-preview",
                htmltools::tags$img(src = image_path),
                htmltools::tags$div(
                  class = "layout-caption",
                  htmltools::tags$span(class = "layout-name", rv$selected_layout_name),
                  slot_badge
                )
              )
            }
          }

          pptx_details <- htmltools::tags$div(
            class = "pptx-details-card",
            detail_rows,
            layout_preview
          )
        }

        ## File list section
        file_list <- NULL
        if (has_files) {
          ## Resolve to absolute paths for resource registration
          abs_files <- vapply(files, function(f) {
            if (startsWith(f, "/") || grepl("^[A-Za-z]:", f)) f
            else file.path(getwd(), f)
          }, character(1), USE.NAMES = FALSE)

          ## Register resource paths for image thumbnails
          img_dirs <- unique(dirname(abs_files))
          for (i in seq_along(img_dirs)) {
            path_name <- paste0("filelist_imgs_", i)
            if (!(path_name %in% names(shiny::resourcePaths()))) {
              shiny::addResourcePath(path_name, img_dirs[i])
            }
          }

          file_items <- lapply(seq_along(files), function(idx) {
            file <- files[[idx]]
            abs_file <- abs_files[[idx]]
            fname <- basename(file)
            dname <- dirname(file)
            dir_label <- if (dname == "." || dname == "") NULL else {
              htmltools::tags$span(class = "file-dir", dname)
            }

            ## Build thumbnail URL
            dir_idx <- which(img_dirs == dirname(abs_file))[1]
            thumb_src <- paste0("filelist_imgs_", dir_idx, "/", fname)

            htmltools::tags$div(
              class = "file-list-item",
              htmltools::tags$img(class = "file-thumbnail", src = thumb_src),
              htmltools::tags$span(class = "file-name", fname),
              dir_label
            )
          })
          file_list <- htmltools::tags$div(
            class = "file-list-container",
            htmltools::tags$div(class = "file-list-heading", "Selected Images"),
            file_items,
            ## Lightbox overlay for expanded image view
            htmltools::tags$div(
              id = "filelist-lightbox",
              class = "filelist-lightbox",
              onclick = "this.classList.remove('active');",
              htmltools::tags$span(class = "close-hint", "Click anywhere to close"),
              htmltools::tags$img(id = "filelist-lightbox-img", src = "", alt = "Preview")
            ),
            htmltools::tags$script(htmltools::HTML("
              $(document).off('click.filelist').on('click.filelist', '.file-thumbnail', function() {
                $('#filelist-lightbox-img').attr('src', $(this).attr('src'));
                $('#filelist-lightbox').addClass('active');
              });
            "))
          )
        }

        htmltools::tagList(pptx_details, file_list)
      })
      log4r::debug(.le$logger, "pptx_server module loaded successfully")

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 8. Session close + clear layouts
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      session$onSessionEnded(function() {
        layout_dir <- file.path(tempdir(), "prfy_layouts")
        if (dir.exists(layout_dir)) {
          unlink(layout_dir, recursive = TRUE, force = TRUE)
          log4r::info(.le$logger, paste("Session ended, removed layout directory:", layout_dir))
        }

        if ("pptx_layouts" %in% names(shiny::resourcePaths())) {
          shiny::removeResourcePath("pptx_layouts")
        }

        ## Clean up preview image resource paths
        resource_paths <- names(shiny::resourcePaths())
        preview_paths <- resource_paths[grepl("^preview_imgs_", resource_paths)]
        for (path_name in preview_paths) {
          shiny::removeResourcePath(path_name)
        }

        log4r::info(.le$logger, "Session ended, removed resource paths")
      })
    }
  )
}

#' @noRd
app_server <- function(input, output, session) {
  log4r::info(.le$logger, "Initializing app server")
  pptx_server(id="app")
}
