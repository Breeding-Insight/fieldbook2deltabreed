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

#' Ask DeltaBreed's BrAPI which level a unit belongs to, and which units it sits inside
#'
#' @param base_url The BrAPI Base URL from DeltaBreed's BrAPI tab.
#' @param token A DeltaBreed access token. It only goes in the Authorization header and is redacted from errors.
#' @param obs_unit_id An ObsUnitID from the converted file.
#' @return A list: `level_name` as DeltaBreed reports it (e.g. `"plant"`), `level_order`, and `parents`,
#'   the parent ObsUnitIDs named by level.
#' @export
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

#' DeltaBreed's details for every unit in the program, one row per unit, named as template columns
#'
#' Field mapping follows deltabreedquery 1.0.4 (define_mapping_obsunits, define_mapping_expts, handle_subunits_obsdf).
#'
#' @inheritParams deltabreed_unit_level
#' @return A data.table with `observationUnitDbId`, `unit_name`, `parents` (a list column) and the template
#'   columns DeltaBreed fills, for [backfill_template()].
#' @export
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

#' Fill a converted table's template columns from [deltabreed_units()]
#'
#' A unit missing from DeltaBreed, or under a different parent there, keeps blank template columns. IDs,
#' traits, timestamps and rows are never changed.
#'
#' @param data The `data` of a [process_single_file()] result, before [name_id_cols()].
#' @param level_names Unit level names, top to bottom, with the most-defined level filled in.
#' @param units A [deltabreed_units()] result.
#' @return A list: `data` (a filled copy), `filled` (the number of rows filled), and the ObsUnitIDs that were
#'   `missing` from DeltaBreed, `mismatched` (under a different parent) or `too_deep` (below the first sub-level).
#' @export
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
