library(shiny)
library(shinydashboard)
library(DT)
library(tools)
library(readxl)
library(openxlsx)
library(data.table)
library(shinyjs)

server <- function(input, output, session) { # nolint
  
  rv <- reactiveValues(data = NULL, uploaded_files = list())
  
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
  
  # Function to process a single file
  process_single_file <- function(file) {
    fileType <- file_ext(file$datapath)
    
    # Upload data AS DATA.TABLES (DTs) using native data.table function fread
    tryCatch({
      if (fileType == "xls" || fileType == "xlsx") {
        # If pathname for fb is xls
        fb <- as.data.table(read_excel(file$datapath))
      } else if (fileType == "csv" || fileType == "txt") {
        # If pathname for fb is csv
        fb <- fread(file$datapath, colClasses = c(timestamp = "character"))
      } else {
        return(list(success = FALSE, data = NULL, message = "Unknown file type"))
      }
    }, error = function(e) {
      return(list(success = FALSE, data = NULL, message = paste("Error reading file:", e$message)))
    })
    
    if (!"value" %in% colnames(fb)) {
      return(list(success = FALSE, data = NULL, message = "Incorrect table format. Check if Fieldbook output was uploaded in table format (original file name ends in '_table'), and try uploading a Fieldbook output in database.csv format (file ending in '_database.csv')."))
    }
    
    # Rename observation unit ID column in Field Book to match Deltabreed
    setnames(fb, "ObservationUnitDbId", "ObsUnitID")
    
    # Set key for efficient joins
    setkeyv(fb, "ObsUnitID")
    
    # Pivot the data.table wider based on whether timestamps exist
    if (!all(is.na(fb$timestamp))) {
      setnames(fb, "timestamp", "TS")
      # Remove the period and the three digits that follow
      fb$TS <- sub("\\.[0-9]{3}", "", fb$TS)
      # Replace the space with a T
      fb$TS <- gsub(" ", "T", fb$TS)
      # Pivot the data.table wider with both value and TS
      fbWide <- dcast(fb, ObsUnitID ~ trait, value.var = c("value", "TS"))
      setnames(fbWide, gsub("value_", "", names(fbWide)))
      setnames(fbWide, gsub("TS_", "TS:", names(fbWide)))
    } else {
      # Pivot the data.table wider with just value
      fbWide <- dcast(fb, ObsUnitID ~ trait, value.var = "value")
      setnames(fbWide, gsub("value_", "", names(fbWide)))
    }
    
    # Set key on the pivoted data
    setkeyv(fbWide, "ObsUnitID")
    
    # Create a unique data.table with one row per ObsUnitID
    fb_unique <- unique(fb, by = "ObsUnitID")
    
    # Create a template data.table with all required columns and set key
    dt <- data.table(
      "Germplasm Name" = fb_unique$Germplasm,
      "Germplasm GID" = NA_character_,
      "Test (T) or Check (C)" = fb_unique$EntryType,
      "Exp Title" = NA_character_,
      "Exp Description" = NA_character_,
      "Exp Unit" = NA_character_,
      "Exp Type" = NA_character_,
      "Env" = NA_character_,
      "Env Location" = NA_character_,
      "Env Year" = NA_character_,
      "Exp Unit ID" = NA_character_,
      "Exp Replicate #" = fb_unique$Rep,
      "Exp Block #" = fb_unique$Block,
      "Row" = fb_unique$Row,
      "Column" = fb_unique$Col,
      "Lat" = NA_real_,
      "Long" = NA_real_,
      "Elevation" = NA_real_,
      "RTK" = fb_unique$geo_coordinates,
      "Treatment Factors" = NA_character_,
      "ObsUnitID" = fb_unique$ObsUnitID
    )
    setkeyv(dt, "ObsUnitID")
    
    # Combine template with the trait data (key-based join)
    tryCatch({
      dbReady <- dt[fbWide]
      
      # Validate the combined data
      if (nrow(dbReady) == 0) {
        return(list(success = FALSE, data = NULL, message = "No data after combining template with trait data"))
      }
      
      # Check for any issues with the data structure
      if (any(is.na(dbReady$ObsUnitID))) {
        return(list(success = FALSE, data = NULL, message = "Missing ObsUnitID values in combined data"))
      }
      
      return(list(success = TRUE, data = dbReady, message = paste("Successfully processed", file$name)))
    }, error = function(e) {
      return(list(success = FALSE, data = NULL, message = paste("Error combining data:", e$message)))
    })
  }
  
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
  
  # Styled feedback as Bootstrap alerts
  output$feedback_ui <- renderUI({
    tags$div(class = "alert alert-info", "Please upload one or more Fieldbook output '_database' files in .csv or .txt format.")
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
      processed_files <- list()
      success_count <- 0
      error_messages <- c()
      
      for (i in seq_len(nrow(all_files))) {
        file <- all_files[i, ]
        result <- process_single_file(file)
        if (result$success) {
          processed_files[[length(processed_files) + 1]] <- result$data
          success_count <- success_count + 1
        } else {
          error_messages <- c(error_messages, paste(file$name, ":", result$message))
        }
      }
      
      if (success_count > 0) {
        if (length(error_messages) > 0) {
          output$feedback_ui <- renderUI({
            tags$div(class = "alert alert-warning",
              paste0("Successfully processed ", success_count, " file(s). Errors: ", paste(error_messages, collapse = "; "))
            )
          })
        } else {
          output$feedback_ui <- renderUI({
            tags$div(class = "alert alert-success",
              paste0("Successfully processed all ", success_count, " file(s).")
            )
          })
        }
        
        combined_data <- if (length(processed_files) > 1) {
          rbindlist(processed_files, fill = TRUE)
        } else {
          processed_files[[1]]
        }
        
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
        
        export_filename <- generate_export_filename(all_files)
        
        # Determine index of ObsUnitID and compute hidden/display/export column sets
        obs_unit_col <- which(names(combined_data) == "ObsUnitID")
        hide_targets <- if (length(obs_unit_col) == 1 && !is.na(obs_unit_col) && obs_unit_col > 1) 0:(obs_unit_col - 2) else integer(0)
        export_cols <- 0:(ncol(combined_data) - 1)

        # Render full data but hide columns before ObsUnitID in the UI; export includes all columns
        output$table <- DT::renderDT({
          combined_data
        }, server = FALSE, options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "copy", text = "Copy", exportOptions = list(columns = export_cols, modifier = list(page = "all"))),
            list(extend = "csv", text = "CSV", filename = export_filename, exportOptions = list(columns = export_cols, modifier = list(page = "all"))),
            list(extend = "excel", text = "Excel", filename = export_filename, exportOptions = list(columns = export_cols, modifier = list(page = "all")))
          ),
          ordering = TRUE,
          columnDefs = list(
            list(visible = FALSE, targets = hide_targets)
          )
        ), extensions = c("Buttons"), rownames = FALSE)
        
        # Show the export indicator
        shinyjs::runjs("var el = document.getElementById('export-indicator'); if (el) { el.style.display = 'block'; }")
        
        rv$data <- combined_data
      } else {
        output$feedback_ui <- renderUI({
          tags$div(class = "alert alert-danger",
            paste("Failed to process any files. Errors:", paste(error_messages, collapse = "; "))
          )
        })
        output$table <- DT::renderDT({
          data.table()
        }, server = FALSE, options = list(dom = "t"), rownames = FALSE)
        
        # Hide the export indicator
        shinyjs::runjs("var el = document.getElementById('export-indicator'); if (el) { el.style.display = 'none'; }")
      }
    }
  }) %>%
    bindEvent(input$files)
  
  # Clear all uploaded files
  observeEvent(input$clearFiles, {
    rv$uploaded_files <- list()
    rv$data <- NULL
    output$feedback_ui <- renderUI({
      tags$div(class = "alert alert-info", "All files cleared. Please upload new files.")
    })
    output$fileList <- renderUI({
      tags$div(em("No files uploaded yet"))
    })
    output$table <- DT::renderDT({
      data.table()
    }, options = list(dom = "t"), rownames = FALSE)
    
    # Hide the export indicator
    shinyjs::runjs("var el = document.getElementById('export-indicator'); if (el) { el.style.display = 'none'; }")
  })
}
