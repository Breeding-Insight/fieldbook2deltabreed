library(shiny)
library(shinydashboard)
library(DT)
library(tools)
library(readxl)
library(openxlsx)
library(data.table)
library(shinyjs)

# DeltaBreed v1.3 template columns, from the alfalfa Plot (top level) and Plant (sub-entity) exports
top_template_cols <- c("Germplasm Name", "Germplasm GID", "Test (T) or Check (C)", "Exp Title",
  "Exp Description", "Exp Unit", "Exp Type", "Env", "Env Location", "Env Year", "Exp Unit ID",
  "Exp Replicate #", "Exp Block #", "Row", "Column", "Lat", "Long", "Elevation", "RTK",
  "Treatment Factors")
sub_template_cols <- append(append(top_template_cols, "Sub-Obs Unit", after = 6),
  "Sub Unit ID", after = 12)

# Field Book and BrAPI columns that never name a unit level
fb_standard_cols <- c("gid", "ObservationUnitDbId", "germplasmId", "expUnitID", "Germplasm",
  "EntryType", "ObservationUnitName", "Column", "Col", "Block", "Row", "Rep", "Location", "rtk",
  "geo_coordinates", "trait", "value", "timestamp", "person", "location", "number",
  "device_name", "attached_photo", "attached_video", "attached_audio")

# Field Book writes each ancestor unit as "<ObsUnitID> [<key>]"
parent_pattern <- "^[0-9a-fA-F-]{36} \\[.*\\]$"

# Keep the ObsUnitID part of an ancestor value; empty means no ancestor at that level
parent_id <- function(x) {
  x <- sub(" .*$", "", as.character(x))
  x[x == ""] <- NA_character_
  x
}

# Find the ancestor level columns (e.g. Plot, Plant, Cluster) and order them top to bottom
find_level_cols <- function(fb) {
  cols <- setdiff(names(fb), "ObservationUnitDbId")
  cols <- cols[vapply(cols, function(col) {
    values <- as.character(fb[[col]])
    values <- values[!is.na(values) & values != ""]
    length(values) > 0 && all(grepl(parent_pattern, values))
  }, logical(1))]
  if (length(cols) < 2) return(cols)

  ids <- fb[, lapply(.SD, parent_id), .SDcols = cols]
  # A higher level is filled on every row a lower one is, and has fewer distinct units
  filled <- vapply(ids, function(x) sum(!is.na(x)), integer(1))
  units <- vapply(ids, function(x) uniqueN(x, na.rm = TRUE), integer(1))
  ord <- order(-filled, units)
  cols <- cols[ord]
  sort_key <- paste(filled, units)[ord]

  for (j in seq_along(cols)[-1]) {
    upper <- ids[[cols[j - 1]]]
    lower <- ids[[cols[j]]]
    parents_per_unit <- data.table(upper, lower)[!is.na(lower), uniqueN(upper), by = lower]$V1
    if (any(!is.na(lower) & is.na(upper)) || any(parents_per_unit != 1)) {
      stop("Unit levels '", cols[j - 1], "' and '", cols[j], "' do not nest: every ", cols[j],
        " must sit inside exactly one ", cols[j - 1], ".")
    }
  }
  # Levels with identical counts cannot be ordered from the data; they keep Field Book's order
  structure(cols, tied = cols[sort_key %in% sort_key[duplicated(sort_key)]])
}

# Function to process a single file
process_single_file <- function(file) {
  fileType <- file_ext(file$datapath)

  # Upload data AS DATA.TABLES (DTs) using native data.table function fread
  fb <- tryCatch({
    if (fileType == "xls" || fileType == "xlsx") {
      # If pathname for fb is xls
      as.data.table(read_excel(file$datapath))
    } else if (fileType == "csv" || fileType == "txt") {
      # If pathname for fb is csv
      fread(file$datapath, colClasses = c(timestamp = "character"))
    } else {
      NULL
    }
  }, error = function(e) e)

  if (is.null(fb)) {
    return(list(success = FALSE, data = NULL, message = "Unknown file type"))
  }
  if (inherits(fb, "error")) {
    return(list(success = FALSE, data = NULL, message = paste("Error reading file:", conditionMessage(fb))))
  }

  if (!"value" %in% colnames(fb)) {
    return(list(success = FALSE, data = NULL, message = "Incorrect table format. Check if Fieldbook output was uploaded in table format (original file name ends in '_table'), and try uploading a Fieldbook output in database.csv format (file ending in '_database.csv')."))
  }

  missing_cols <- setdiff(c("ObservationUnitDbId", "trait"), colnames(fb))
  if (length(missing_cols) > 0) {
    return(list(success = FALSE, data = NULL, message = paste("Missing required column(s):", paste(missing_cols, collapse = ", "))))
  }
  if (any(is.na(fb$ObservationUnitDbId) | fb$ObservationUnitDbId == "")) {
    return(list(success = FALSE, data = NULL, message = "Some rows have no ObservationUnitDbId"))
  }

  # dcast would collapse repeated observations into counts, so stop instead
  repeats <- fb[duplicated(fb, by = c("ObservationUnitDbId", "trait"))]
  if (nrow(repeats) > 0) {
    return(list(success = FALSE, data = NULL, message = paste0(nrow(repeats), " repeated observation(s) of the same trait on the same unit (e.g. '", repeats$trait[1], "' on ", repeats$ObservationUnitDbId[1], "). Repeated measures are not supported yet.")))
  }

  # Ancestor levels, top to bottom; each row's own unit sits one level below its last ancestor
  level_cols <- tryCatch(find_level_cols(fb), error = function(e) e)
  if (inherits(level_cols, "error")) {
    return(list(success = FALSE, data = NULL, message = conditionMessage(level_cols)))
  }
  n_levels <- length(level_cols) + 1
  id_cols <- paste0(".id", seq_len(n_levels))

  # One row per unit: ancestor IDs fill the levels above it, its own ID goes at its own depth
  units <- unique(fb[, c("ObservationUnitDbId", level_cols), with = FALSE])
  if (anyDuplicated(units$ObservationUnitDbId)) {
    return(list(success = FALSE, data = NULL, message = "Some units are listed under more than one parent unit"))
  }
  depth <- integer(nrow(units))
  for (col in level_cols) depth <- depth + !is.na(parent_id(units[[col]]))
  ids <- data.table(ObservationUnitDbId = units$ObservationUnitDbId)
  for (j in seq_len(n_levels)) {
    level_id <- if (j <= length(level_cols)) parent_id(units[[level_cols[j]]]) else rep(NA_character_, nrow(units))
    level_id[depth + 1 == j] <- units$ObservationUnitDbId[depth + 1 == j]
    set(ids, j = id_cols[j], value = level_id)
  }

  # Level names come from the ancestor column headers; NA is the most-defined level, typed in the app
  level_names <- c(level_cols, NA_character_)
  if (length(level_cols) == 0) {
    # A top-level-only export can still name its level with an empty column (e.g. Tank)
    empty_cols <- setdiff(names(fb), fb_standard_cols)
    empty_cols <- empty_cols[vapply(empty_cols, function(col) all(is.na(fb[[col]]) | as.character(fb[[col]]) == ""), logical(1))]
    if (length(empty_cols) == 1) level_names <- empty_cols
  }

  # Pivot the data.table wider based on whether timestamps exist
  if (!all(is.na(fb$timestamp))) {
    setnames(fb, "timestamp", "TS")
    # Remove the period and the three digits that follow
    fb$TS <- sub("\\.[0-9]{3}", "", fb$TS)
    # Replace the space with a T
    fb$TS <- gsub(" ", "T", fb$TS)
    # Pivot the data.table wider with both value and TS
    fbWide <- dcast(fb, ObservationUnitDbId ~ trait, value.var = c("value", "TS"))
    # dcast prefixes each column with its value.var; rename TS_ first so a trait named "TS_..." keeps its name
    setnames(fbWide, sub("^TS_", "TS:", names(fbWide)))
    setnames(fbWide, sub("^value_", "", names(fbWide)))
  } else {
    # Pivot the data.table wider with just value; dcast names these columns by trait alone
    fbWide <- dcast(fb, ObservationUnitDbId ~ trait, value.var = "value")
  }

  template_cols <- if (n_levels > 1) sub_template_cols else top_template_cols
  clashes <- intersect(setdiff(names(fbWide), "ObservationUnitDbId"), c(template_cols, id_cols))
  if (length(clashes) > 0) {
    return(list(success = FALSE, data = NULL, message = paste("Trait names clash with DeltaBreed template columns:", paste(clashes, collapse = ", "))))
  }

  # Join unit IDs to the traits, parents before their sub-units, and add the blank template
  dbReady <- ids[fbWide, on = "ObservationUnitDbId"]
  dbReady[, ObservationUnitDbId := NULL]
  setorderv(dbReady, id_cols, na.last = FALSE)
  dbReady[, (template_cols) := NA]
  setcolorder(dbReady, c(template_cols, id_cols))

  list(success = TRUE, data = dbReady, level_names = level_names, tied = attr(level_cols, "tied"),
    message = paste("Successfully processed", file$name))
}

# Describe unit levels top to bottom; NA is the unnamed most-defined level
describe_levels <- function(level_names, deepest = "(deepest level)") {
  level_names[is.na(level_names)] <- deepest
  paste(level_names, collapse = " > ")
}

# A cell holds an observation unless it is NA or empty text
present <- function(v) if (is.character(v)) !is.na(v) & v != "" else !is.na(v)

# Combine processed files that share unit levels. A unit found in several files becomes one row; any parent
# or trait that those files fill with different values is returned as a conflict instead
combine_files <- function(processed_files, level_names) {
  combined <- rbindlist(lapply(processed_files, function(x) data.table(.file = x$name, x$data)), fill = TRUE)
  id_cols <- grep("^\\.id[0-9]+$", names(combined), value = TRUE)
  # A row's own unit is its deepest filled ID
  combined[, .unit := do.call(fcoalesce, rev(.SD)), .SDcols = id_cols]
  shared <- combined[, uniqueN(.file), by = .unit][V1 > 1, .unit]
  if (length(shared) == 0) {
    return(list(data = combined[, !c(".file", ".unit")], conflicts = character(0), merged = 0L))
  }

  rows <- combined[.unit %in% shared]
  # Timestamps may differ for the same value; the earliest is kept below
  compare_cols <- setdiff(names(rows), c(".file", ".unit", grep("^TS:", names(rows), value = TRUE)))
  conflicts <- unlist(lapply(compare_cols, function(col) {
    found <- rows[present(get(col)), .(n = uniqueN(get(col)), detail = paste0(get(col), " in ", .file, collapse = ", ")),
      by = .unit][n > 1]
    if (nrow(found) == 0) return(NULL)
    label <- if (col %in% id_cols) {
      level <- level_names[match(col, id_cols)]
      paste(if (is.na(level)) "deepest level" else level, "ObsUnitID")
    } else col
    paste0("Unit ", found$.unit, " has different values for '", label, "': ", found$detail)
  }))
  if (length(conflicts) > 0) return(list(data = NULL, conflicts = conflicts, merged = 0L))

  # Each column takes the unit's one observed value; a timestamp takes the earliest
  pick <- function(v, col) if (startsWith(col, "TS:")) sort(v[present(v)])[1] else v[present(v)][1]
  merged <- rows[, Map(pick, .SD, names(.SD)), by = .unit, .SDcols = setdiff(names(rows), c(".file", ".unit"))]
  data <- rbind(combined[!.unit %in% shared], merged, use.names = TRUE, fill = TRUE)
  data[, c(".file", ".unit") := NULL]
  setorderv(data, id_cols, na.last = FALSE)
  list(data = data, conflicts = character(0), merged = length(shared))
}

server <- function(input, output, session) { # nolint

  rv <- reactiveValues(data = NULL, level_names = NULL, export_filename = NULL, uploaded_files = list())

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

      # A unit in several files is merged into one row, unless the files disagree about it
      if (success_count > 0) {
        combined <- combine_files(processed_files, level_sets[[1]])
        if (length(combined$conflicts) > 0) {
          shown <- head(combined$conflicts, 10)
          more <- if (length(combined$conflicts) > 10) paste0("; and ", length(combined$conflicts) - 10, " more") else ""
          error_messages <- c(error_messages, paste0("These files give the same unit different values and cannot be combined - ",
            paste(shown, collapse = "; "), more, ". Fix the files and upload again."))
          success_count <- 0
        }
      }

      if (success_count > 0) {
        merged_note <- if (combined$merged > 0) paste0(" ", combined$merged, " unit(s) appeared in more than one file and were merged.") else ""
        tied <- unique(unlist(lapply(processed_files, function(x) x$tied)))
        tied_note <- if (length(tied) > 0) paste0(" Note: the order of ", paste(tied, collapse = ", "), " could not be told apart from the data, so Field Book's column order was kept: ", describe_levels(level_sets[[1]]), ".") else ""
        if (length(error_messages) > 0) {
          output$feedback_ui <- renderUI({
            tags$div(class = "alert alert-warning",
              paste0("Successfully processed ", success_count, " file(s). Errors: ", paste(error_messages, collapse = "; "), merged_note, tied_note)
            )
          })
        } else {
          output$feedback_ui <- renderUI({
            tags$div(class = if (nzchar(tied_note)) "alert alert-warning" else "alert alert-success",
              paste0("Successfully processed all ", success_count, " file(s).", merged_note, tied_note)
            )
          })
        }

        combined_data <- combined$data

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

        # Show the export indicator
        shinyjs::runjs("var el = document.getElementById('export-indicator'); if (el) { el.style.display = 'block'; }")

        rv$data <- combined_data
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

  # Name the ObsUnitID columns by depth (Top level, then Sub-level); renaming never re-runs the conversion
  output$table <- DT::renderDT({
    req(rv$data)
    n_levels <- length(rv$level_names)
    level_labels <- if (n_levels <= 2) c("Top level", "Sub-level")[seq_len(n_levels)] else
      c("Top level", paste("Sub-level", seq_len(n_levels - 1)))
    export_data <- copy(rv$data)
    setnames(export_data, paste0(".id", seq_len(n_levels)), paste(level_labels, "ObsUnitID"))

    # Hide the blank template columns in the UI; export includes all columns
    hide_targets <- 0:(match(".id1", names(rv$data)) - 2)
    export_cols <- 0:(ncol(export_data) - 1)

    # Exports keep only the rows matching the search box, so ask before exporting a filtered table
    confirm_export <- function(button) DT::JS(paste0(
      "function(e, dt, node, config) {",
      "  var search = dt.search();",
      "  if (search && !confirm('A search is applied: only the ' + dt.rows({search: \"applied\"}).count() + ' of ' + dt.rows().count() +",
      "    ' rows matching \"' + search + '\" will be exported. Proceed?')) return;",
      "  $.fn.dataTable.ext.buttons.", button, ".action.call(this, e, dt, node, config);",
      "}"))

    DT::datatable(export_data, options = list(
      scrollX = TRUE,
      pageLength = 25,
      dom = "Bfrtip",
      buttons = list(
        list(extend = "copy", text = "Copy", title = NULL, action = confirm_export("copyHtml5"), exportOptions = list(columns = export_cols, modifier = list(page = "all"))),
        list(extend = "csv", text = "CSV", filename = rv$export_filename, action = confirm_export("csvHtml5"), exportOptions = list(columns = export_cols, modifier = list(page = "all"))),
        list(extend = "excel", text = "Excel", filename = rv$export_filename, title = NULL, action = confirm_export("excelHtml5"), exportOptions = list(columns = export_cols, modifier = list(page = "all")))
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
    output$feedback_ui <- renderUI({
      tags$div(class = "alert alert-info", "All files cleared. Please upload new files.")
    })

    # Hide the export indicator
    shinyjs::runjs("var el = document.getElementById('export-indicator'); if (el) { el.style.display = 'none'; }")

    # Empty the file picker's name box and hide its "Upload complete" bar
    shinyjs::reset("files")
    shinyjs::runjs("var bar = document.getElementById('files_progress'); if (bar) { bar.style.visibility = 'hidden'; }")
  })
}
