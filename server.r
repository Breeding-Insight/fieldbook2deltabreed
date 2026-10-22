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
    setnames(fbWide, gsub("value_", "", names(fbWide)))
    setnames(fbWide, gsub("TS_", "TS:", names(fbWide)))
  } else {
    # Pivot the data.table wider with just value
    fbWide <- dcast(fb, ObservationUnitDbId ~ trait, value.var = "value")
    setnames(fbWide, gsub("value_", "", names(fbWide)))
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

# Describe unit levels top to bottom; NA is the typed most-defined level
describe_levels <- function(level_names, typed = "(typed level)") {
  level_names[is.na(level_names)] <- typed
  paste(level_names, collapse = " > ")
}

# DeltaBreed's BrAPI returns level names in lower case (e.g. "plant"); headers use "Plant"
capitalize_level <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))

# BrAPI v2 root from the BrAPI Base URL on DeltaBreed's BrAPI tab
brapi_base <- function(base_url) paste0(sub("/brapi/v2$", "", sub("/+$", "", trimws(base_url))), "/brapi/v2")

# Send a BrAPI request; the token only goes in the Authorization header and is redacted from errors
brapi_send <- function(req, token, timeout = 15) {
  resp <- tryCatch(
    req |>
      httr2::req_auth_bearer_token(token) |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) stop("Could not reach DeltaBreed: ", gsub(token, "<token>", conditionMessage(e), fixed = TRUE), call. = FALSE)
  )
  if (httr2::resp_status(resp) == 401) {
    stop("DeltaBreed rejected the access token (it may have expired). Generate a new one on DeltaBreed's BrAPI tab.", call. = FALSE)
  }
  resp
}

# The units a DeltaBreed unit sits inside, named by level. Relationships also hold rep and block,
# and possibly the unit itself
unit_parents <- function(unit, obs_unit_id = unit$observationUnitDbId) {
  relationships <- Filter(function(r) !is.null(r$observationUnitDbId) && !tolower(r$levelName) %in% c("rep", "block"),
    unit$observationUnitPosition$observationLevelRelationships)
  parents <- setNames(parent_id(vapply(relationships, function(r) r$observationUnitDbId, "")),
    vapply(relationships, function(r) r$levelName, ""))
  parents[parents != obs_unit_id]
}

# Ask DeltaBreed's BrAPI which level a unit belongs to, and which units it sits inside.
# base_url is the BrAPI Base URL from DeltaBreed's BrAPI tab; the token only goes in the Authorization header
deltabreed_unit_level <- function(base_url, token, obs_unit_id) {
  brapi <- brapi_base(base_url)
  send <- function(req) brapi_send(req, token)
  body <- function(resp) tryCatch(httr2::resp_body_json(resp)$result, error = function(e) NULL)
  not_found <- paste0("Unit ", obs_unit_id, " was not found in this DeltaBreed program. Check that the BrAPI Base URL is for the program this Field Book file came from.")

  # BrAPI v2 offers GET /observationunits/{id}; fall back to the ?observationUnitDbId= filter
  resp <- send(httr2::request(brapi) |> httr2::req_url_path_append("observationunits", obs_unit_id))
  unit <- if (httr2::resp_status(resp) == 200) body(resp) else NULL
  if (is.null(unit$observationUnitDbId)) {
    resp <- send(httr2::request(brapi) |> httr2::req_url_path_append("observationunits") |>
      httr2::req_url_query(observationUnitDbId = obs_unit_id))
    # DeltaBreed v1.3 answers 500 instead of an empty list when nothing matches (noted in deltabreedquery)
    if (httr2::resp_status(resp) %in% c(404, 500)) stop(not_found, call. = FALSE)
    if (httr2::resp_status(resp) != 200) {
      stop("DeltaBreed returned status ", httr2::resp_status(resp), " for observation units.", call. = FALSE)
    }
    result <- body(resp)
    if (is.null(result)) stop("DeltaBreed did not return BrAPI data. Check the BrAPI Base URL.", call. = FALSE)
    matches <- Filter(function(u) identical(u$observationUnitDbId, obs_unit_id), result$data)
    if (length(matches) == 0) stop(not_found, call. = FALSE)
    unit <- matches[[1]]
  }

  level_name <- unit$observationUnitPosition$observationLevel$levelName
  if (is.null(level_name) || level_name == "") {
    stop("DeltaBreed did not report a level for unit ", obs_unit_id, ".", call. = FALSE)
  }
  list(level_name = level_name, level_order = unit$observationUnitPosition$observationLevel$levelOrder,
    parents = unit_parents(unit, obs_unit_id))
}

# Every record from a paginated BrAPI list endpoint, requested as deltabreedquery 1.0.4 does
brapi_get_all <- function(base_url, token, endpoint) {
  records <- list()
  page <- 0
  repeat {
    resp <- brapi_send(httr2::request(brapi_base(base_url)) |> httr2::req_url_path_append(endpoint) |>
      httr2::req_url_query(pageSize = 10000, page = page), token, timeout = 60)
    # DeltaBreed v1.3 answers 500 instead of an empty list when there is nothing to return
    if (httr2::resp_status(resp) == 500) break
    if (httr2::resp_status(resp) != 200) {
      stop("DeltaBreed returned status ", httr2::resp_status(resp), " for ", endpoint, ".", call. = FALSE)
    }
    json <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
    if (is.null(json$result)) stop("DeltaBreed did not return BrAPI data for ", endpoint, ". Check the BrAPI Base URL.", call. = FALSE)
    records <- c(records, json$result$data)
    page <- page + 1
    total_pages <- json$metadata$pagination$totalPages
    if (is.null(total_pages) || page >= total_pages) break
  }
  records
}

# Text of one BrAPI field; a missing or null field is NA
brapi_text <- function(x) if (length(x) == 0) NA_character_ else as.character(x[[1]])

# DeltaBreed's details for every unit in the program, one row per unit, named as template columns.
# Field mapping follows deltabreedquery 1.0.4 (define_mapping_obsunits, define_mapping_expts, handle_subunits_obsdf)
deltabreed_units <- function(base_url, token) {
  units <- brapi_get_all(base_url, token, "observationunits")
  if (length(units) == 0) stop("DeltaBreed returned no observation units for this program.", call. = FALSE)
  trials <- brapi_get_all(base_url, token, "trials")
  studies <- brapi_get_all(base_url, token, "studies")
  seasons <- brapi_get_all(base_url, token, "seasons")

  lookup <- function(records, key, value) {
    setNames(vapply(records, function(r) brapi_text(value(r)), ""), vapply(records, function(r) brapi_text(r[[key]]), ""))
  }
  exp_description <- lookup(trials, "trialDbId", function(t) t$trialDescription)
  exp_type <- lookup(trials, "trialDbId", function(t) t$additionalInfo$experimentType)
  season_year <- lookup(seasons, "seasonDbId", function(s) s$year)
  env_year <- lookup(studies, "studyDbId", function(s) season_year[brapi_text(s$seasons)])

  rows <- lapply(units, function(u) {
    position <- u$observationUnitPosition
    level_code <- function(level) {
      brapi_text(unlist(lapply(Filter(function(r) identical(tolower(brapi_text(r$levelName)), level),
        position$observationLevelRelationships), function(r) r$levelCode)))
    }
    entry_type <- brapi_text(position$entryType)
    treatments <- vapply(u$treatments, function(t) brapi_text(t$modality), "")
    list(
      observationUnitDbId = brapi_text(u$observationUnitDbId),
      unit_name = brapi_text(u$observationUnitName),
      parents = list(unname(unit_parents(u))),
      `Germplasm Name` = brapi_text(u$germplasmName),
      `Germplasm GID` = brapi_text(u$additionalInfo$gid),
      # BrAPI entry types are CHECK and TEST; the template uses C and T
      `Test (T) or Check (C)` = switch(toupper(entry_type), CHECK = "C", TEST = "T", entry_type),
      `Exp Title` = brapi_text(u$trialName),
      `Exp Description` = brapi_text(exp_description[brapi_text(u$trialDbId)]),
      `Exp Type` = brapi_text(exp_type[brapi_text(u$trialDbId)]),
      Env = brapi_text(u$studyName),
      `Env Location` = brapi_text(u$locationName),
      `Env Year` = brapi_text(env_year[brapi_text(u$studyDbId)]),
      `Exp Replicate #` = level_code("rep"),
      `Exp Block #` = level_code("block"),
      Row = brapi_text(position$positionCoordinateX),
      Column = brapi_text(position$positionCoordinateY),
      `Treatment Factors` = if (length(treatments) > 0) paste(treatments, collapse = "; ") else NA_character_
    )
  })
  unique(rbindlist(rows), by = "observationUnitDbId")
}

# Template columns DeltaBreed gives per unit; sub-units without them repeat their top-level unit's
unit_template_cols <- c("Germplasm Name", "Germplasm GID", "Test (T) or Check (C)", "Exp Replicate #", "Exp Block #",
  "Row", "Column")
# Experiment and environment template columns, taken from the row's own unit
env_template_cols <- c("Exp Title", "Exp Description", "Exp Type", "Env", "Env Location", "Env Year", "Treatment Factors")

# Fill a converted table's template columns from deltabreed_units(). A unit missing from DeltaBreed, or under a
# different parent there, keeps blank template columns. IDs, traits, timestamps and rows are never changed
backfill_template <- function(data, level_names, units) {
  data <- copy(data)
  ids <- as.matrix(data[, paste0(".id", seq_along(level_names)), with = FALSE])
  rows <- seq_len(nrow(ids))
  depth <- rowSums(!is.na(ids))
  own <- ids[cbind(rows, depth)]
  parent <- ifelse(depth > 1, ids[cbind(rows, pmax(depth - 1, 1))], NA_character_)

  self <- units[match(own, units$observationUnitDbId)]
  top <- units[match(ids[, 1], units$observationUnitDbId)]
  missing <- is.na(self$observationUnitDbId)
  mismatched <- !missing & !is.na(parent) & !mapply(`%in%`, parent, self$parents)
  ok <- !missing & !mismatched

  for (col in c(unit_template_cols, env_template_cols)) {
    value <- self[[col]]
    if (col %in% unit_template_cols) value <- fifelse(is.na(value) | value == "", top[[col]], value)
    value[!ok] <- NA
    set(data, j = col, value = value)
  }
  set(data, j = "Exp Unit", value = fifelse(ok, level_names[1], NA_character_))
  set(data, j = "Exp Unit ID", value = fifelse(ok, top$unit_name, NA_character_))
  # DeltaBreed v1.3 templates have one sub-level; deeper rows leave it blank
  if ("Sub-Obs Unit" %in% names(data)) {
    sub_row <- ok & depth == 2
    set(data, j = "Sub-Obs Unit", value = fifelse(sub_row, level_names[2], NA_character_))
    set(data, j = "Sub Unit ID", value = fifelse(sub_row, units$unit_name[match(ids[, 2], units$observationUnitDbId)], NA_character_))
  }
  list(data = data, filled = sum(ok), missing = own[missing], mismatched = own[mismatched],
    too_deep = own[ok & depth > 2])
}

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
    req(rv$level_names)
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
    export_data <- copy(if (is.null(rv$db_units)) rv$data else backfilled()$data)
    setnames(export_data, paste0(".id", seq_along(level_names)), paste(level_names, "ObsUnitID"))

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
