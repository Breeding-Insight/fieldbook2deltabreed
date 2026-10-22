# Test inputs are generated here; only data_file() reads the gitignored data/ folder

brapi_url <- "https://db.example.invalid/v1/programs/abc"
brapi_token <- "SECRET-TOKEN-123"

# A file in data/, or skip the test (data/ is never there under R CMD check)
data_file <- function(pattern) {
  path <- Sys.glob(test_path("..", "..", "data", pattern))
  skip_if(length(path) != 1, paste("not in data/:", pattern))
  path
}

hex <- function(k) paste(sample(c(0:9, letters[1:6]), k, TRUE), collapse = "")
uuid <- function(n = 1) vapply(seq_len(n), function(i) paste(hex(8), hex(4), paste0("4", hex(3)), hex(4), hex(12), sep = "-"), "")
# Field Book writes each ancestor unit as "<ObsUnitID> [<key>]", and leaves it empty when there is none
tag <- function(id) ifelse(is.na(id), "", paste0(id, " [TEST-1]"))

write_fieldbook <- function(fb, name = "synthetic") {
  path <- file.path(tempdir(), paste0(name, "_database.csv"))
  fwrite(fb, path)
  path
}
convert <- function(fb, name = "synthetic") process_single_file(write_fieldbook(fb, name))

# A Field Book export of a known Plot > Plant > Cluster > Specimen hierarchy. As in real exports, the ancestor
# columns are in alphabetical order and the rows are not in hierarchy order. Every plot and specimen is
# observed, and half the plants and clusters. `truth` holds each unit's level, ancestors and name
synthetic_fieldbook <- function(seed = 20261021) {
  set.seed(seed)
  units <- list()
  add <- function(id, level, plot = NA, plant = NA, cluster = NA, name) {
    units[[length(units) + 1]] <<- data.table(id = id, level = level, plot = plot, plant = plant, cluster = cluster, name = name)
  }
  for (p in 1:6) {
    plot_id <- uuid()
    add(plot_id, 1L, name = as.character(100 + p))
    for (pl in seq_len(sample(2:4, 1))) {
      plant_id <- uuid()
      add(plant_id, 2L, plot_id, name = as.character(pl))
      for (cl in seq_len(sample(1:3, 1))) {
        cluster_id <- uuid()
        add(cluster_id, 3L, plot_id, plant_id, name = as.character(cl))
        for (sp in seq_len(sample(1:4, 1))) add(uuid(), 4L, plot_id, plant_id, cluster_id, name = as.character(sp))
      }
    }
  }
  truth <- rbindlist(units)
  observed <- truth[level %in% c(1L, 4L) | (level %in% 2:3 & seq_len(.N) %% 2 == 0)]
  traits <- list(`1` = "Yield", `2` = "Height", `3` = "ClusterCount", `4` = c("Length", "Width"))
  obs <- observed[, list(trait = traits[[as.character(level)]]), by = c("id", "level", "plot", "plant", "cluster", "name")]
  obs[, value := round(runif(.N, 1, 100), 1)]
  obs[, timestamp := format(as.POSIXct("2026-10-21 08:00:00", tz = "UTC") + seq_len(.N) * 7, "%Y-%m-%d %H:%M:%S.123-04:00")]
  fb <- obs[, list(gid = "1", ObservationUnitDbId = id, germplasmId = uuid(), Cluster = tag(cluster), Plant = tag(plant),
    Plot = tag(plot), expUnitID = "", Germplasm = "G1", EntryType = "TEST", ObservationUnitName = name, Column = 1,
    Block = 1, Row = 1, Rep = 1, Location = "Test", rtk = "", geo_coordinates = NA, trait, value, timestamp,
    person = " ", location = NA, number = 1, device_name = "test")]
  list(truth = truth, obs = obs, fb = fb[sample(.N)])
}

# A Field Book export with one unit level: three units with two traits each. `empty_col` adds blank columns,
# like the Tank column that names the level in Alex's Danby export
single_level_fieldbook <- function(empty_col = NULL) {
  fb <- data.table(ObservationUnitDbId = rep(uuid(3), each = 2), ObservationUnitName = rep(c("1", "2", "3"), each = 2),
    trait = rep(c("Yield", "Height"), 3), value = c(1.5, 20, 2.5, 30, 3.5, 40),
    timestamp = "2026-05-14 08:46:43.123-04:00")
  if (!is.null(empty_col)) fb[, (empty_col) := ""]
  fb
}

# BrAPI observation units for a synthetic_fieldbook() hierarchy. Each unit's relationships list its ancestors and
# itself; as in the alfalfa export, only plots carry germplasm, entry type, position, rep and block, so sub-units
# fall back to them
brapi_units <- function(truth) {
  level_names <- c("plot", "plant", "cluster", "specimen")
  lapply(seq_len(nrow(truth)), function(i) {
    u <- truth[i]
    related <- c(plot = u$plot, plant = u$plant, cluster = u$cluster)
    related <- c(related[!is.na(related)], setNames(u$id, level_names[u$level]))
    rel <- lapply(names(related), function(nm) {
      list(levelName = nm, levelCode = "x", observationUnitDbId = paste0(related[[nm]], " [TEST-1]"))
    })
    position <- list(observationLevel = list(levelName = level_names[u$level], levelOrder = u$level - 1L),
      observationLevelRelationships = rel)
    unit <- list(observationUnitDbId = u$id, observationUnitName = u$name, trialDbId = "t1", trialName = "Synthetic trial",
      studyDbId = "s1", studyName = "Synthetic env", locationName = "Test field",
      treatments = list(list(factor = "pH", modality = "6.5")))
    if (u$level == 1L) {
      unit$germplasmName <- paste0("G", u$name)
      unit$additionalInfo <- list(gid = as.integer(u$name))
      position$entryType <- if (u$name == "101") "CHECK" else "TEST"
      position$positionCoordinateX <- u$name
      position$positionCoordinateY <- "1"
      position$observationLevelRelationships <- c(rel, list(list(levelName = "rep", levelCode = "1"),
        list(levelName = "block", levelCode = "2")))
    }
    unit$observationUnitPosition <- position
    unit
  })
}
synthetic_trials <- list(list(trialDbId = "t1", trialName = "Synthetic trial", trialDescription = "A synthetic trial",
  additionalInfo = list(experimentType = "Phenotyping")))
synthetic_studies <- list(list(studyDbId = "s1", studyName = "Synthetic env", seasons = list("season1")))
synthetic_seasons <- list(list(seasonDbId = "season1", year = 2026L))

# A mock DeltaBreed BrAPI: GET observationunits/{id}, the ?observationUnitDbId= filter, and paged lists.
# `status` forces a status code by path (e.g. list(trials = 403)); `path_form = FALSE` makes GET by ID answer 404.
# Request URLs and Authorization headers are recorded in `log`
brapi_mock <- function(units, trials = list(), studies = list(), seasons = list(), page_size = 1000, status = list(),
                       path_form = TRUE, log = new.env()) {
  log$urls <- character(0)
  log$auth <- character(0)
  by_id <- setNames(units, vapply(units, function(u) u$observationUnitDbId, ""))
  function(req) {
    parsed <- httr2::url_parse(req$url)
    path <- sub(".*/brapi/v2/", "", parsed$path)
    log$urls <- c(log$urls, req$url)
    log$auth <- c(log$auth, httr2::req_get_headers(req, redacted = "reveal")$Authorization)
    if (!is.null(status[[path]])) return(httr2::response(status_code = status[[path]]))
    if (startsWith(path, "observationunits/")) {
      unit <- by_id[[sub("observationunits/", "", path)]]
      if (!path_form || is.null(unit)) return(httr2::response(status_code = 404))
      return(httr2::response_json(body = list(result = unit)))
    }
    records <- switch(path, observationunits = units, trials = trials, studies = studies, seasons = seasons)
    if (!is.null(parsed$query$observationUnitDbId)) {
      records <- Filter(function(u) identical(u$observationUnitDbId, parsed$query$observationUnitDbId), records)
    }
    page <- as.integer(if (is.null(parsed$query$page)) 0 else parsed$query$page)
    chunk <- records[seq_along(records) > page * page_size & seq_along(records) <= (page + 1) * page_size]
    httr2::response_json(body = list(metadata = list(pagination = list(totalPages = max(1, ceiling(length(records) / page_size)))),
      result = list(data = chunk)))
  }
}
synthetic_mock <- function(units, ...) brapi_mock(units, synthetic_trials, synthetic_studies, synthetic_seasons, ...)
