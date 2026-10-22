library(shiny)
library(shinydashboard)
library(DT)
library(tools)
library(readxl)
library(openxlsx)
library(data.table)
library(shinyjs)

server <- function(input, output, session) { # nolint

  rv <- reactiveValues(data = NULL, level_names = NULL, export_filename = NULL, uploaded_files = list(),
    db_level = NULL, db_message = NULL, db_units = NULL, db_units_error = NULL)

  # Main content output - shows empty state or table
  output$main_content <- renderUI({
    if (is.null(rv$data)) {
      tags$div(class = "empty-state",
        icon("upload", class = "fa-2x"),
        h4("No data uploaded yet"),
        p("Use the file picker on the left to upload one or more Fieldbook '_database' files (.csv, .txt, .xls, .xlsx)."),
        tags$small("The table and export options will appear here after processing.")
      )
    } else {
      tagList(
        div(id = "export-indicator", class = "dt-export-prefix", "Ready for Export:"),
        shinycssloaders::withSpinner(DT::DTOutput("table"), type = 6)
      )
    }
  })

  # Removed server-side download; using DataTables client-side exports instead

  # Display list of uploaded files (styled list)
  output$fileList <- renderUI({
    if (length(rv$uploaded_files) == 0) {
      tags$div(em("No files uploaded yet"))
    } else {
      file_names <- sapply(rv$uploaded_files, function(x) x$name)
      tags$ul(class = "list-group",
        lapply(file_names, function(fn) tags$li(class = "list-group-item", fn))
      )
    }
  })

  # Unit levels found in the upload, with the typed most-defined level
  output$levels_ui <- renderUI({
    req(!is.null(rv$level_names))
    tags$div(class = "alert alert-secondary",
      paste("Unit levels, top to bottom:", describe_levels(rv$level_names, trimws(input$deep_level)))
    )
  })

  # Styled feedback as Bootstrap alerts
  output$feedback_ui <- renderUI({
    tags$div(class = "alert alert-info", "Please upload one or more Fieldbook output '_database' files in .csv or .txt format.")
  })

  # Unit level names, top to bottom, with the typed most-defined level
  resolved_levels <- reactive({
    # Not req(rv$level_names): a single level typed in the sidebar is NA, which req() treats as missing
    req(!is.null(rv$level_names))
    level_names <- rv$level_names
    level_names[is.na(level_names)] <- trimws(input$deep_level)
    level_names
  })

  # Template columns filled from DeltaBreed's unit details, when they were fetched
  backfilled <- reactive({
    req(rv$data, rv$db_units)
    backfill_template(rv$data, resolved_levels(), rv$db_units)
  })

  # Outcome of the optional DeltaBreed lookups: level name, then template columns
  output$db_status <- renderUI({
    fill_status <- if (!is.null(rv$db_units_error)) {
      tags$div(class = "alert alert-warning", paste("Experiment details could not be fetched from DeltaBreed:",
        rv$db_units_error, "Template columns are left blank."))
    } else if (!is.null(rv$db_units) && !is.null(rv$data)) {
      result <- backfilled()
      example <- function(ids, what) if (length(ids) > 0) paste0(length(ids), " ", what, " (e.g. ", ids[1], ").")
      notes <- c(
        example(result$missing, "unit(s) not found in DeltaBreed; their template columns are blank"),
        example(result$mismatched, "unit(s) under a different parent in DeltaBreed; their template columns are blank"),
        example(result$too_deep, "unit(s) below the first sub-level; Sub-Obs Unit and Sub Unit ID are blank for them"))
      tags$div(class = if (length(notes) > 0) "alert alert-warning" else "alert alert-success",
        paste(c(paste0("Filled template columns for ", result$filled, " of ", nrow(rv$data), " rows from DeltaBreed."), notes),
          collapse = " "))
    }
    tagList(
      if (!is.null(rv$db_message)) tags$div(class = rv$db_message$class, rv$db_message$text),
      fill_status
    )
  })

  # A text input counts as filled only when it has non-blank text
  filled <- function(x) !is.null(x) && nzchar(trimws(x))

  # Ask DeltaBreed for the most-defined level's name, using one unit at that level from the upload
  lookup_deep_level <- function() {
    if (!filled(input$db_url) || !filled(input$db_token)) {
      rv$db_message <- list(class = "alert alert-warning", text = "Enter the BrAPI Base URL and an access token from DeltaBreed's BrAPI tab first.")
      return(invisible(FALSE))
    }
    n <- length(rv$level_names)
    id_col <- paste0(".id", n)
    row <- rv$data[!is.na(rv$data[[id_col]])][1]
    result <- withProgress(message = "Asking DeltaBreed for the level name...", {
      tryCatch(deltabreed_unit_level(trimws(input$db_url), trimws(input$db_token), row[[id_col]]), error = function(e) e)
    })
    if (inherits(result, "error")) {
      rv$db_level <- NULL
      rv$db_message <- list(class = "alert alert-warning",
        text = paste("DeltaBreed lookup failed:", conditionMessage(result), "The typed level name is used instead."))
      return(invisible(FALSE))
    }

    level_name <- capitalize_level(result$level_name)
    notes <- character(0)
    if (is.na(rv$level_names[n])) {
      updateTextInput(session, "deep_level", value = level_name)
    } else if (!identical(tolower(rv$level_names[n]), tolower(level_name))) {
      # A single-level file already named its level with an empty column (e.g. Tank); keep it, but say so
      notes <- c(notes, paste0("Field Book calls this level '", rv$level_names[n], "', DeltaBreed calls it '", level_name, "'; Field Book's name was kept."))
    }
    # Field Book's parent for this unit should be one DeltaBreed lists, under the same level name
    if (n > 1) {
      fb_parent <- row[[paste0(".id", n - 1)]]
      db_parent_level <- names(result$parents)[result$parents == fb_parent]
      if (length(db_parent_level) == 0) {
        notes <- c(notes, paste0("DeltaBreed does not list ", fb_parent, " as a parent of ", row[[id_col]], "; check the experiment."))
      } else if (!tolower(rv$level_names[n - 1]) %in% tolower(db_parent_level)) {
        notes <- c(notes, paste0("Field Book calls the parent level '", rv$level_names[n - 1], "', DeltaBreed calls it '", capitalize_level(db_parent_level[1]), "'."))
      }
    }
    rv$db_level <- level_name
    rv$db_message <- list(class = if (length(notes) > 0) "alert alert-warning" else "alert alert-success",
      text = paste(c(paste0("Level name from DeltaBreed: ", level_name, "."), notes), collapse = " "))
    invisible(TRUE)
  }

  # Fetch DeltaBreed's details for every unit in the program, to fill the template columns
  fetch_units <- function() {
    units <- withProgress(message = "Fetching experiment details from DeltaBreed...", {
      tryCatch(deltabreed_units(trimws(input$db_url), trimws(input$db_token)), error = function(e) e)
    })
    if (inherits(units, "error")) {
      rv$db_units <- NULL
      rv$db_units_error <- conditionMessage(units)
    } else {
      rv$db_units <- units
      rv$db_units_error <- NULL
    }
  }

  observeEvent(input$db_lookup, {
    if (is.null(rv$data)) {
      rv$db_message <- list(class = "alert alert-info", text = "Upload a Field Book file first.")
    } else if (lookup_deep_level()) {
      fetch_units()
    }
  })

  observe({
    if (is.null(input$files) || length(input$files) == 0) {
      output$feedback_ui <- renderUI({
        tags$div(class = "alert alert-info", "Please upload one or more Fieldbook output '_database' files in .csv or .txt format.")
      })
    } else {
      req(input$files)
      files <- input$files

      current_count <- length(rv$uploaded_files)
      new_count <- nrow(files)
      total_count <- current_count + new_count

      if (total_count > 10) {
        output$feedback_ui <- renderUI({
          tags$div(class = "alert alert-danger",
            paste0("Cannot upload ", new_count, " files. You already have ", current_count, " files uploaded. Maximum allowed is 10 files. Please remove some files or upload fewer files.")
          )
        })
        return()
      }

      for (i in seq_len(nrow(files))) {
        rv$uploaded_files[[length(rv$uploaded_files) + 1]] <- files[i, ]
      }

      all_files <- do.call(rbind, rv$uploaded_files)
      rv$db_level <- NULL
      rv$db_message <- NULL
      rv$db_units <- NULL
      rv$db_units_error <- NULL
      processed_files <- list()
      success_count <- 0
      error_messages <- c()

      for (i in seq_len(nrow(all_files))) {
        file <- all_files[i, ]
        result <- process_single_file(file)
        if (result$success) {
          result$name <- file$name
          processed_files[[length(processed_files) + 1]] <- result
          success_count <- success_count + 1
        } else {
          error_messages <- c(error_messages, paste(file$name, ":", result$message))
        }
      }

      # Files can only share one table when they have the same unit levels
      level_sets <- lapply(processed_files, function(x) x$level_names)
      if (success_count > 0 && !all(vapply(level_sets, identical, logical(1), level_sets[[1]]))) {
        level_summary <- vapply(processed_files, function(x) paste0(x$name, ": ", describe_levels(x$level_names)), character(1))
        error_messages <- c(error_messages, paste("These files have different unit levels and cannot be combined -", paste(level_summary, collapse = "; ")))
        success_count <- 0
      }

      if (success_count > 0) {
        tied <- unique(unlist(lapply(processed_files, function(x) x$tied)))
        tied_note <- if (length(tied) > 0) paste0(" Note: the order of ", paste(tied, collapse = ", "), " could not be told apart from the data, so Field Book's column order was kept. Check the unit levels below.") else ""
        if (length(error_messages) > 0) {
          output$feedback_ui <- renderUI({
            tags$div(class = "alert alert-warning",
              paste0("Successfully processed ", success_count, " file(s). Errors: ", paste(error_messages, collapse = "; "), tied_note)
            )
          })
        } else {
          output$feedback_ui <- renderUI({
            tags$div(class = if (nzchar(tied_note)) "alert alert-warning" else "alert alert-success",
              paste0("Successfully processed all ", success_count, " file(s).", tied_note)
            )
          })
        }

        combined_data <- rbindlist(lapply(processed_files, function(x) x$data), fill = TRUE)

        # Generate export filename based on uploaded files
        generate_export_filename <- function(files) {
          # Generate timestamp in format YYYYMMDD_HHMMSS
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

          if (nrow(files) == 1) {
            # Single file: replace "_database" with "_deltabreed"
            base_name <- tools::file_path_sans_ext(files$name[1])
            base_name <- gsub("_database$", "_deltabreed", base_name)
            return(paste0(timestamp, "_", base_name))
          } else {
            # Multiple files: use timestamp + "_deltabreed_upload"
            return(paste0(timestamp, "_deltabreed_upload"))
          }
        }

        rv$export_filename <- generate_export_filename(all_files)
        rv$level_names <- level_sets[[1]]

        # Suggest a name for the most-defined level unless the user already typed one
        if (anyNA(rv$level_names) && trimws(input$deep_level) %in% c("", "Plant", "Plot")) {
          deep_default <- if (length(rv$level_names) > 1) "Plant" else "Plot"
          if (deep_default %in% rv$level_names) deep_default <- ""
          updateTextInput(session, "deep_level", value = deep_default)
        }

        # Show the export indicator
        shinyjs::runjs("var el = document.getElementById('export-indicator'); if (el) { el.style.display = 'block'; }")

        rv$data <- combined_data

        # Name the most-defined level and fill the template columns from DeltaBreed when the user gave a URL and token
        if (filled(input$db_url) && filled(input$db_token) && lookup_deep_level()) fetch_units()
      } else {
        output$feedback_ui <- renderUI({
          tags$div(class = "alert alert-danger",
            paste("No table could be produced. Errors:", paste(error_messages, collapse = "; "))
          )
        })
        rv$data <- NULL
        rv$level_names <- NULL

        # Hide the export indicator
        shinyjs::runjs("var el = document.getElementById('export-indicator'); if (el) { el.style.display = 'none'; }")
      }
    }
  }) %>%
    bindEvent(input$files)

  # Name the ObsUnitID columns from the unit levels; renaming never re-runs the conversion
  output$table <- DT::renderDT({
    req(rv$data)
    level_names <- resolved_levels()
    validate(
      need(all(level_names != ""), "Type the name of the most-defined unit level in the sidebar."),
      need(!anyDuplicated(level_names), paste("Each unit level needs a different name:", paste(level_names, collapse = " > ")))
    )
    export_data <- name_id_cols(if (is.null(rv$db_units)) rv$data else backfilled()$data, level_names)

    # Hide blank template columns in the UI, so ones filled from DeltaBreed can be reviewed; export includes all columns
    template_n <- match(".id1", names(rv$data)) - 1
    blank <- vapply(export_data[, seq_len(template_n), with = FALSE], function(x) all(is.na(x) | x == ""), logical(1))
    hide_targets <- unname(which(blank)) - 1
    export_cols <- 0:(ncol(export_data) - 1)

    DT::datatable(export_data, options = list(
      scrollX = TRUE,
      pageLength = 25,
      dom = "Bfrtip",
      buttons = list(
        list(extend = "copy", text = "Copy", title = NULL, exportOptions = list(columns = export_cols, modifier = list(page = "all"))),
        list(extend = "csv", text = "CSV", filename = rv$export_filename, exportOptions = list(columns = export_cols, modifier = list(page = "all"))),
        list(extend = "excel", text = "Excel", filename = rv$export_filename, title = NULL, exportOptions = list(columns = export_cols, modifier = list(page = "all")))
      ),
      ordering = TRUE,
      columnDefs = list(
        list(visible = FALSE, targets = hide_targets)
      )
    ), extensions = c("Buttons"), rownames = FALSE)
  }, server = FALSE)

  # Clear all uploaded files
  observeEvent(input$clearFiles, {
    rv$uploaded_files <- list()
    rv$data <- NULL
    rv$level_names <- NULL
    rv$db_level <- NULL
    rv$db_message <- NULL
    rv$db_units <- NULL
    rv$db_units_error <- NULL
    output$feedback_ui <- renderUI({
      tags$div(class = "alert alert-info", "All files cleared. Please upload new files.")
    })
    output$fileList <- renderUI({
      tags$div(em("No files uploaded yet"))
    })

    # Hide the export indicator
    shinyjs::runjs("var el = document.getElementById('export-indicator'); if (el) { el.style.display = 'none'; }")
  })
}
