# DeltaBreed BrAPI calls against a mocked server; no real DeltaBreed is contacted

lookup <- function(mock, id, url = brapi_url) httr2::with_mocked_responses(mock, deltabreed_unit_level(url, brapi_token, id))
fetch <- function(mock, url = brapi_url) httr2::with_mocked_responses(mock, deltabreed_units(url, brapi_token))
error_text <- function(expr) {
  e <- tryCatch({
    expr
    NULL
  }, error = conditionMessage)
  expect_type(e, "character")
  expect_no_match(e, brapi_token, fixed = TRUE)
  e
}

test_that("deltabreed_unit_level() reads a unit's level and ancestors, sending the token only as a header", {
  s <- synthetic_fieldbook()
  log <- new.env()
  specimen <- s$truth[level == 4][1]
  r <- lookup(brapi_mock(brapi_units(s$truth), log = log), specimen$id)
  expect_identical(r$level_name, "specimen")
  expect_identical(r$level_order, 3L)
  expect_identical(r$parents, c(plot = specimen$plot, plant = specimen$plant, cluster = specimen$cluster))
  expect_length(log$urls, 1)
  expect_identical(log$auth, paste("Bearer", brapi_token))
  expect_no_match(log$urls, brapi_token, fixed = TRUE)
  expect_identical(capitalize_level(c("plant", "plot", "Tank")), c("Plant", "Plot", "Tank"))
})

test_that("deltabreed_unit_level() falls back to ?observationUnitDbId= when GET by ID fails", {
  units <- brapi_units(synthetic_fieldbook()$truth)
  id <- units[[length(units)]]$observationUnitDbId
  log <- new.env()
  expect_identical(lookup(brapi_mock(units, path_form = FALSE, log = log), id)$level_name, "specimen")
  expect_length(log$urls, 2)
  expect_match(log$urls[2], "observationUnitDbId=")
  # A 200 that is not BrAPI JSON (e.g. a login page) also falls back
  html_by_id <- function(req) {
    if (grepl("/observationunits/", req$url)) httr2::response(200, body = charToRaw("<html>DeltaBreed</html>")) else brapi_mock(units)(req)
  }
  expect_identical(lookup(html_by_id, id)$level_name, "specimen")
  # A Base URL pasted with /brapi/v2/ is not doubled
  log <- new.env()
  lookup(brapi_mock(units, log = log), id, url = paste0(brapi_url, "/brapi/v2/"))
  expect_match(log$urls[1], "/v1/programs/abc/brapi/v2/observationunits/", fixed = TRUE)
  expect_no_match(log$urls[1], "brapi/v2/brapi", fixed = TRUE)
})

test_that("deltabreed_unit_level() failures say what went wrong without showing the token", {
  units <- brapi_units(synthetic_fieldbook()$truth)
  id <- units[[1]]$observationUnitDbId
  expect_match(error_text(lookup(function(req) httr2::response(401), id)), "rejected the access token")
  expect_match(error_text(lookup(brapi_mock(units), uuid())), "not found in this DeltaBreed program")
  # DeltaBreed v1.3 answers 500 instead of an empty list when nothing matches
  expect_match(error_text(lookup(brapi_mock(units, path_form = FALSE, status = list(observationunits = 500)), id)), "not found")
  expect_match(error_text(lookup(brapi_mock(units, path_form = FALSE, status = list(observationunits = 503)), id)), "returned status 503")
  network <- error_text(lookup(function(req) stop("Timeout was reached; Bearer ", brapi_token), id))
  expect_match(network, "^Could not reach DeltaBreed")
  expect_match(network, "<token>", fixed = TRUE)
  no_level <- function(req) httr2::response_json(body = list(result = list(observationUnitDbId = id)))
  expect_match(error_text(lookup(no_level, id)), "did not report a level")
})

test_that("deltabreed_units() reads every page of the program", {
  s <- synthetic_fieldbook()
  log <- new.env()
  db <- fetch(synthetic_mock(brapi_units(s$truth), page_size = 50, log = log))
  expect_identical(nrow(db), nrow(s$truth))
  expect_identical(sum(grepl("/observationunits?", log$urls, fixed = TRUE)), as.integer(ceiling(nrow(s$truth) / 50)))
  expect_true(all(grepl("pageSize=10000", log$urls, fixed = TRUE)))
  expect_true(all(log$auth == paste("Bearer", brapi_token)))
  expect_no_match(log$urls, brapi_token, fixed = TRUE)
})

test_that("deltabreed_units() maps BrAPI fields to template columns", {
  s <- synthetic_fieldbook()
  db <- fetch(synthetic_mock(brapi_units(s$truth)))
  plot <- db[observationUnitDbId == s$truth[level == 1]$id[1]]
  expect_identical(plot$unit_name, "101")
  expect_identical(plot$`Germplasm Name`, "G101")
  expect_identical(plot$`Germplasm GID`, "101")
  # BrAPI's CHECK and TEST are C and T in the template
  expect_identical(plot$`Test (T) or Check (C)`, "C")
  expect_identical(unique(db[observationUnitDbId %in% s$truth[level == 1]$id[-1]]$`Test (T) or Check (C)`), "T")
  expect_identical(unlist(plot[, c("Exp Title", "Exp Description", "Exp Type", "Env", "Env Location", "Env Year")], use.names = FALSE),
    c("Synthetic trial", "A synthetic trial", "Phenotyping", "Synthetic env", "Test field", "2026"))
  expect_identical(unlist(plot[, c("Exp Replicate #", "Exp Block #", "Row", "Column", "Treatment Factors")], use.names = FALSE),
    c("1", "2", "101", "1", "6.5"))
  # A unit's parents are its ancestors, not itself
  specimen <- s$truth[level == 4][1]
  expect_identical(db[observationUnitDbId == specimen$id]$parents[[1]], c(specimen$plot, specimen$plant, specimen$cluster))
  expect_length(db[observationUnitDbId == plot$observationUnitDbId]$parents[[1]], 0)
})

test_that("deltabreed_units() failures say what went wrong without showing the token", {
  units <- brapi_units(synthetic_fieldbook()$truth)
  expect_match(error_text(fetch(function(req) httr2::response(401))), "rejected the access token")
  expect_match(error_text(fetch(function(req) stop("timeout talking to Bearer ", brapi_token))), "Could not reach DeltaBreed")
  expect_match(error_text(fetch(synthetic_mock(list(), status = list(observationunits = 500)))), "no observation units")
  expect_match(error_text(fetch(synthetic_mock(units, status = list(trials = 403)))), "status 403 for trials")
  expect_match(error_text(fetch(function(req) httr2::response(200, body = charToRaw("<html>login</html>")))), "did not return BrAPI data")
  # DeltaBreed v1.3 answers 500 for an empty list: no seasons, so no Env Year
  expect_true(all(is.na(fetch(synthetic_mock(units, status = list(seasons = 500)))$`Env Year`)))
})

test_that("backfill_template() fills each row from its own unit and its top-level unit", {
  s <- synthetic_fieldbook()
  r <- convert(s$fb)
  levels <- c("Plot", "Plant", "Cluster", "Specimen")
  db <- fetch(synthetic_mock(brapi_units(s$truth)))
  res <- backfill_template(r$data, levels, db)
  out <- res$data
  ids <- as.matrix(r$data[, paste0(".id", 1:4), with = FALSE])
  depth <- rowSums(!is.na(ids))
  plot <- db[match(ids[, 1], db$observationUnitDbId)]

  expect_identical(res$filled, nrow(out))
  expect_length(res$missing, 0)
  expect_length(res$mismatched, 0)
  expect_identical(length(res$too_deep), sum(depth > 2))
  # Rows, IDs, traits and timestamps are unchanged, and the input is not modified by reference
  expect_identical(out[, -(1:22)], r$data[, -(1:22)])
  expect_true(all(is.na(as.matrix(r$data[, 1:22]))))
  # Sub-units without germplasm, entry type, rep, block or position take their plot's
  for (col in unit_template_cols) expect_identical(out[[col]], plot[[col]], label = col)
  expect_identical(out$`Exp Unit ID`, plot$unit_name)
  expect_true(all(out$`Exp Unit` == "Plot"))
  expect_true(all(out$Env == "Synthetic env" & out$`Env Year` == "2026"))
  # The v1.3 template has one sub-level, so only Plant rows fill it
  expect_true(all(out$`Sub-Obs Unit`[depth == 2] == "Plant"))
  expect_identical(out$`Sub Unit ID`[depth == 2], db$unit_name[match(ids[depth == 2, 2], db$observationUnitDbId)])
  expect_true(all(is.na(out$`Sub-Obs Unit`[depth != 2]) & is.na(out$`Sub Unit ID`[depth != 2])))
  expect_true(all(is.na(as.matrix(out[, c("Lat", "Long", "Elevation", "RTK")]))))
})

test_that("backfill_template() leaves a missing or moved unit blank and reports it", {
  s <- synthetic_fieldbook()
  r <- convert(s$fb)
  levels <- c("Plot", "Plant", "Cluster", "Specimen")
  db <- fetch(synthetic_mock(brapi_units(s$truth)))
  plant <- r$data[!is.na(.id2) & is.na(.id3)][1]
  plant_row <- function(data) data[.id2 == plant$.id2 & is.na(.id3)]

  res <- backfill_template(r$data, levels, db[observationUnitDbId != plant$.id2])
  expect_identical(res$missing, plant$.id2)
  expect_identical(res$filled, nrow(r$data) - 1L)
  expect_true(all(is.na(unlist(plant_row(res$data)[, 1:22]))))

  moved <- copy(db)
  moved[observationUnitDbId == plant$.id2, parents := list(list(setdiff(s$truth[level == 1]$id, plant$.id1)[1]))]
  res <- backfill_template(r$data, levels, moved)
  expect_identical(res$mismatched, plant$.id2)
  expect_identical(res$filled, nrow(r$data) - 1L)
  expect_true(all(is.na(unlist(plant_row(res$data)[, 1:22]))))
})

test_that("backfill_template() keeps the single-level layout", {
  fb <- single_level_fieldbook(empty_col = "Tank")
  r <- convert(fb)
  units <- lapply(unique(fb$ObservationUnitDbId), function(id) {
    list(observationUnitDbId = id, observationUnitName = "101", germplasmName = "Bob",
      observationUnitPosition = list(entryType = "test", observationLevel = list(levelName = "tank", levelOrder = 0L)))
  })
  res <- backfill_template(r$data, r$level_names, fetch(brapi_mock(units)))
  expect_identical(names(res$data)[1:20], top_template_cols)
  expect_false("Sub-Obs Unit" %in% names(res$data))
  expect_identical(res$filled, 3L)
  expect_true(all(res$data$`Exp Unit` == "Tank" & res$data$`Test (T) or Check (C)` == "T"))
  # No trials or seasons in the program: their columns stay blank
  expect_true(all(is.na(res$data$`Env Year`) & is.na(res$data$`Exp Description`)))
})

test_that("backfill_template() reproduces the alfalfa Plant export cell for cell", {
  p <- as.data.table(readxl::read_excel(data_file("pH_alfalfa_AAA_Plant_All_Env_*.xls"), col_types = "text"))
  r <- process_single_file(data_file("synthetic/synthetic_alfalfa_Plot-Plant_database.csv"))
  template <- names(p)[1:22]
  plots <- p[, .SD[1], by = "Plot ObsUnitID"]

  # A mocked DeltaBreed program holding the alfalfa experiment: plots carry rep, block and position, plants do not
  envs <- unique(p[, c("Env", "Env Location", "Env Year", "Exp Title", "Exp Description", "Exp Type")])
  envs[, `:=`(studyDbId = paste0("s", .I), seasonDbId = paste0("season", .I))]
  trials <- unique(envs[, c("Exp Title", "Exp Description", "Exp Type")])[, trialDbId := paste0("t", .I)]
  envs <- trials[envs, on = c("Exp Title", "Exp Description", "Exp Type")]
  make_unit <- function(x, id, name, level, order, parent = NULL) {
    e <- envs[Env == x$Env & `Exp Title` == x$`Exp Title`]
    rel <- if (is.null(parent)) list() else list(list(levelName = "plot", levelCode = "x", observationUnitDbId = parent))
    position <- list(entryType = if (x$`Test (T) or Check (C)` == "C") "CHECK" else "TEST",
      observationLevel = list(levelName = level, levelOrder = order), observationLevelRelationships = rel)
    if (is.null(parent)) {
      position$positionCoordinateX <- x$Row
      position$positionCoordinateY <- x$Column
      position$observationLevelRelationships <- list(list(levelName = "rep", levelCode = x$`Exp Replicate #`),
        list(levelName = "block", levelCode = x$`Exp Block #`))
    }
    list(observationUnitDbId = id, observationUnitName = name, germplasmName = x$`Germplasm Name`,
      additionalInfo = list(gid = as.integer(x$`Germplasm GID`)), trialDbId = e$trialDbId, trialName = x$`Exp Title`,
      studyDbId = e$studyDbId, studyName = x$Env, locationName = x$`Env Location`, observationUnitPosition = position,
      treatments = list(list(factor = "pH", modality = x$`Treatment Factors`)))
  }
  units <- c(
    lapply(seq_len(nrow(plots)), function(i) make_unit(plots[i], plots$`Plot ObsUnitID`[i], plots$`Exp Unit ID`[i], "plot", 0L)),
    lapply(seq_len(nrow(p)), function(i) make_unit(p[i], p$`Plant ObsUnitID`[i], p$`Sub Unit ID`[i], "plant", 1L, p$`Plot ObsUnitID`[i])))
  db <- fetch(brapi_mock(units,
    lapply(seq_len(nrow(trials)), function(i) list(trialDbId = trials$trialDbId[i], trialDescription = trials$`Exp Description`[i],
      additionalInfo = list(experimentType = trials$`Exp Type`[i]))),
    lapply(seq_len(nrow(envs)), function(i) list(studyDbId = envs$studyDbId[i], seasons = list(envs$seasonDbId[i]))),
    lapply(seq_len(nrow(envs)), function(i) list(seasonDbId = envs$seasonDbId[i], year = as.integer(envs$`Env Year`[i])))))

  res <- backfill_template(r$data, c("Plot", "Plant"), db)
  expect_identical(res$filled, 220L)
  as_text <- function(x) as.data.frame(lapply(x, as.character))
  plant_rows <- res$data[!is.na(.id2)]
  expect_equal(as_text(plant_rows[, template, with = FALSE]),
    as_text(p[match(plant_rows$.id2, p$`Plant ObsUnitID`), template, with = FALSE]), ignore_attr = TRUE)
  # Plot rows get their plot's values, with the sub-level columns blank
  plot_rows <- res$data[is.na(.id2)]
  expected <- plots[match(plot_rows$.id1, plots$`Plot ObsUnitID`), template, with = FALSE][, c("Sub-Obs Unit", "Sub Unit ID") := NA]
  expect_equal(as_text(plot_rows[, template, with = FALSE]), as_text(expected), ignore_attr = TRUE)
})
