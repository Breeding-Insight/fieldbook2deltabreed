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

#' Find the ancestor level columns (e.g. Plot, Plant, Cluster) and order them top to bottom
#'
#' @param fb A Field Book database export as a data.table.
#' @return The ancestor column names, top to bottom. Attribute `tied` names levels whose order could not
#'   be told apart from the data. Stops if the levels do not nest.
#' @export
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

#' Convert one Field Book database export to the DeltaBreed v1.3 layout
#'
#' @param file Path to a Field Book `_database` export (.csv, .txt, .xls or .xlsx), or a list with
#'   `datapath` and `name`, like a row of Shiny's `fileInput`.
#' @return A list with `success` and `message`. On success also `data` (template columns, `.id1` to `.idk`
#'   from the top level down, traits, then `TS:` columns), `level_names` (`NA` for the most-defined level
#'   when the file does not name it) and `tied`. Name the ID columns with [name_id_cols()].
#' @export
process_single_file <- function(file) {
  if (is.character(file)) file <- list(datapath = file, name = basename(file))
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

#' Name the ID columns of a converted table after their unit levels
#'
#' @param data The `data` of a [process_single_file()] or [backfill_template()] result.
#' @param level_names Unit level names, top to bottom, with the most-defined level filled in.
#' @return A copy of `data` with `.id1` to `.idk` renamed `<Level> ObsUnitID`.
#' @export
name_id_cols <- function(data, level_names) {
  data <- copy(data)
  setnames(data, paste0(".id", seq_along(level_names)), paste(level_names, "ObsUnitID"))
  data
}

# Describe unit levels top to bottom; NA is the typed most-defined level
describe_levels <- function(level_names, typed = "(typed level)") {
  level_names[is.na(level_names)] <- typed
  paste(level_names, collapse = " > ")
}
