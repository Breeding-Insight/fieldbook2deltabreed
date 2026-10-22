# The Shiny server, driven with shiny::testServer. R CMD check builds the package without the app files,
# so these tests only run from the repo (testthat::test_local())

load_server <- function() {
  skip_if_not_installed("shiny")
  skip_if_not_installed("jsonlite")
  path <- test_path("..", "..", "server.r")
  skip_if_not(file.exists(path), "server.r is not here (the app is not part of the built package)")
  env <- new.env()
  suppressPackageStartupMessages(sys.source(path, envir = env))
  env$server
}
upload <- function(...) {
  paths <- normalizePath(c(...))
  data.frame(name = basename(paths), size = file.size(paths), type = "text/csv", datapath = paths)
}
# Header names and options of the rendered DT widget
widget <- function(json) jsonlite::fromJSON(json, simplifyVector = FALSE)$x
headers <- function(json) {
  container <- widget(json)$container
  regmatches(container, gregexpr("(?<=<th>)[^<]*(?=</th>)", container, perl = TRUE))[[1]]
}
hidden <- function(json) unlist(widget(json)$options$columnDefs[[1]]$targets)
text_of <- function(html) gsub("<[^>]+>", " ", html$html)

test_that("the app converts, names and exports a four-level upload", {
  server <- load_server()
  s <- synthetic_fieldbook()
  four <- write_fieldbook(s$fb, "four_level")
  shiny::testServer(server, {
    session$setInputs(deep_level = "Plant", files = upload(four))
    expect_identical(rv$level_names, c("Plot", "Plant", "Cluster", NA))
    expect_identical(nrow(rv$data), uniqueN(s$obs$id))
    # A typed name that repeats an ancestor's, or a blank one, blocks the table
    expect_error(output$table, "different name")
    session$setInputs(deep_level = "")
    expect_error(output$table, "Type the name")
    session$setInputs(deep_level = "Specimen")
    h <- headers(output$table)
    expect_identical(h[1:22], sub_template_cols)
    expect_identical(h[23:26], c("Plot ObsUnitID", "Plant ObsUnitID", "Cluster ObsUnitID", "Specimen ObsUnitID"))
    buttons <- widget(output$table)$options$buttons
    expect_true("title" %in% names(buttons[[3]]) && is.null(buttons[[1]]$title) && is.null(buttons[[3]]$title))
    # Blank template columns are hidden on screen but still exported
    expect_identical(hidden(output$table), 0:21)
    expect_match(output$levels_ui$html, "Plot &gt; Plant &gt; Cluster &gt; Specimen", fixed = TRUE)
    session$setInputs(clearFiles = 1)
    expect_null(rv$data)
    expect_null(rv$level_names)
  })
})

test_that("the app combines files with the same levels and refuses mixed levels", {
  server <- load_server()
  a <- write_fieldbook(single_level_fieldbook(), "field_a")
  b <- write_fieldbook(single_level_fieldbook(), "field_b")
  shiny::testServer(server, {
    session$setInputs(deep_level = "Plot", files = upload(a, b))
    expect_identical(nrow(rv$data), 6L)
    expect_identical(headers(output$table)[1:21], c(top_template_cols, "Plot ObsUnitID"))
  })
  four <- write_fieldbook(synthetic_fieldbook()$fb, "four_level")
  shiny::testServer(server, {
    session$setInputs(deep_level = "Specimen", files = upload(four, a))
    expect_null(rv$data)
    expect_match(output$feedback_ui$html, "different unit levels")
  })
})

test_that("the app names the deepest level and fills template columns from DeltaBreed", {
  server <- load_server()
  s <- synthetic_fieldbook()
  four <- write_fieldbook(s$fb, "four_level")
  n <- uniqueN(s$obs$id)
  httr2::with_mocked_responses(synthetic_mock(brapi_units(s$truth)), shiny::testServer(server, {
    session$setInputs(deep_level = "Plant", db_url = brapi_url, db_token = brapi_token, files = upload(four))
    expect_identical(rv$db_level, "Specimen")
    expect_match(text_of(output$db_status), "Level name from DeltaBreed: Specimen")
    expect_match(text_of(output$db_status), sprintf("Filled template columns for %d of %d rows", n, n))
    # What updateTextInput does in the browser
    session$setInputs(deep_level = rv$db_level)
    expect_identical(headers(output$table)[26], "Specimen ObsUnitID")
    # Filled template columns are shown; the ones DeltaBreed leaves blank stay hidden
    expect_identical(sort(hidden(output$table)), match(c("Lat", "Long", "Elevation", "RTK"), sub_template_cols) - 1L)
    expect_true(all(is.na(as.matrix(rv$data[, 1:22]))))
    session$setInputs(clearFiles = 1)
    expect_null(rv$db_units)
    expect_null(rv$db_level)
    expect_null(rv$db_message)
  }))
})

test_that("without a URL and token the app sends nothing, and the button looks up later", {
  server <- load_server()
  s <- synthetic_fieldbook()
  four <- write_fieldbook(s$fb, "four_level")
  log <- new.env()
  httr2::with_mocked_responses(synthetic_mock(brapi_units(s$truth), log = log), shiny::testServer(server, {
    session$setInputs(deep_level = "Specimen", db_url = "", db_token = "", files = upload(four))
    expect_length(log$urls, 0)
    expect_null(rv$db_message)
    expect_identical(hidden(output$table), 0:21)
    session$setInputs(db_url = brapi_url, db_token = brapi_token, db_lookup = 1)
    expect_identical(rv$db_level, "Specimen")
    expect_false(is.null(rv$db_units))
  }))
})

test_that("DeltaBreed failures warn, keep the typed name and leave template columns blank", {
  server <- load_server()
  s <- synthetic_fieldbook()
  four <- write_fieldbook(s$fb, "four_level")
  httr2::with_mocked_responses(function(req) httr2::response(401), shiny::testServer(server, {
    session$setInputs(deep_level = "Specimen", db_url = brapi_url, db_token = brapi_token, files = upload(four))
    expect_null(rv$db_level)
    expect_null(rv$db_units)
    expect_match(text_of(output$db_status), "rejected the access token")
    expect_no_match(output$db_status$html, brapi_token, fixed = TRUE)
    expect_identical(headers(output$table)[26], "Specimen ObsUnitID")
  }))
  # The level lookup works but the program-wide read does not
  httr2::with_mocked_responses(synthetic_mock(brapi_units(s$truth), status = list(observationunits = 403)), shiny::testServer(server, {
    session$setInputs(deep_level = "Specimen", db_url = brapi_url, db_token = brapi_token, files = upload(four))
    expect_identical(rv$db_level, "Specimen")
    expect_null(rv$db_units)
    expect_match(text_of(output$db_status), "could not be fetched")
    expect_identical(hidden(output$table), 0:21)
  }))
})

test_that("the app warns when DeltaBreed names a level differently from Field Book", {
  server <- load_server()
  s <- synthetic_fieldbook()
  four <- write_fieldbook(s$fb, "four_level")
  truss <- lapply(brapi_units(s$truth), function(u) {
    rel <- u$observationUnitPosition$observationLevelRelationships
    for (k in seq_along(rel)) if (identical(rel[[k]]$levelName, "cluster")) rel[[k]]$levelName <- "truss"
    u$observationUnitPosition$observationLevelRelationships <- rel
    u
  })
  httr2::with_mocked_responses(synthetic_mock(truss), shiny::testServer(server, {
    session$setInputs(deep_level = "Plant", db_url = brapi_url, db_token = brapi_token, files = upload(four))
    expect_match(text_of(output$db_status), "Field Book calls the parent level 'Cluster', DeltaBreed calls it 'Truss'")
  }))

  # A single-level file named Tank by its empty column, which DeltaBreed calls plot: Field Book's name is kept
  fb <- single_level_fieldbook(empty_col = "Tank")
  tank <- write_fieldbook(fb, "tank")
  units <- lapply(unique(fb$ObservationUnitDbId), function(id) {
    list(observationUnitDbId = id, observationUnitName = "1", observationUnitPosition = list(observationLevel = list(levelName = "plot", levelOrder = 0L)))
  })
  httr2::with_mocked_responses(brapi_mock(units), shiny::testServer(server, {
    session$setInputs(deep_level = "Plot", db_url = brapi_url, db_token = brapi_token, files = upload(tank))
    expect_identical(rv$level_names, "Tank")
    expect_match(text_of(output$db_status), "Field Book calls this level 'Tank', DeltaBreed calls it 'Plot'")
    expect_identical(headers(output$table)[21], "Tank ObsUnitID")
  }))
})
