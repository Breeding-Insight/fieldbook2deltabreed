# Checks against real exports in the gitignored data/ folder; each test skips when its files are missing

test_that("the template columns match DeltaBreed's own alfalfa exports", {
  plot_xls <- names(readxl::read_excel(data_file("pH_alfalfa_AAA_Plot_All_Env_*.xls"), n_max = 0))
  plant_xls <- names(readxl::read_excel(data_file("pH_alfalfa_AAA_Plant_All_Env_*.xls"), n_max = 0))
  expect_identical(plot_xls, c(top_template_cols, "Plot ObsUnitID"))
  expect_identical(plant_xls, c(sub_template_cols, "Plot ObsUnitID", "Plant ObsUnitID"))
})

test_that("single-level exports keep the Plot layout and every value", {
  fixed <- names(readxl::read_excel(data_file("fixed_*_deltabreed.xlsx"), n_max = 0))
  outs <- list()
  for (pattern in c("*WASHINGTON_database.csv", "*LOMAX_database.csv")) {
    path <- data_file(pattern)
    src <- fread(path, colClasses = c(timestamp = "character"))
    r <- process_single_file(path)
    expect_identical(r$level_names, NA_character_)
    out <- name_id_cols(r$data, "Plot")
    expect_identical(dim(out), c(30L, 25L))
    expect_identical(names(out)[1:21], c(fixed[1:20], "Plot ObsUnitID"))
    id <- out$`Plot ObsUnitID`
    expect_identical(mapply(function(u, t) as.character(out[[t]][id == u]), src$ObservationUnitDbId, src$trait, USE.NAMES = FALSE),
      as.character(src$value))
    expect_identical(mapply(function(u, t) out[[paste0("TS:", t)]][id == u], src$ObservationUnitDbId, src$trait, USE.NAMES = FALSE),
      gsub(" ", "T", sub("\\.[0-9]{3}", "", src$timestamp)))
    outs[[pattern]] <- out
  }
  both <- rbindlist(outs, fill = TRUE)
  expect_identical(nrow(both), 60L)
  expect_identical(names(both)[1:21], c(fixed[1:20], "Plot ObsUnitID"))
})

test_that("Alex's Danby export is named Tank and matches her hand-corrected file", {
  r <- process_single_file(data_file("SAT-124/alex_*_database.csv"))
  hc <- fread(data_file("SAT-124/alex_*_handcorrected.csv"))
  expect_identical(r$level_names, "Tank")
  expect_identical(nrow(r$data), 23L)
  expect_setequal(r$data$.id1, hc$`Tank ObsUnitID`)
  # Field Book recorded "08" and "00"; they are kept as written, and the hand-corrected file shows 8 and 0
  matched <- r$data[match(hc$`Tank ObsUnitID`, r$data$.id1)]
  expect_equal(as.numeric(matched$Yield_lbs), as.numeric(hc$Yield_lbs))
})

test_that("the four-tier demo converts to one row per observed unit", {
  r <- process_single_file(data_file("*Four Tier Demo 2026_database.csv"))
  expect_identical(r$level_names, c("Plot", "Plant", "Cluster", NA))
  expect_identical(nrow(r$data), 273L)
})

test_that("a Plot > Plant file gets the alfalfa Plant export's headers and plot IDs", {
  alf <- as.data.table(readxl::read_excel(data_file("pH_alfalfa_AAA_Plant_All_Env_*.xls")))
  r <- process_single_file(data_file("synthetic/synthetic_alfalfa_Plot-Plant_database.csv"))
  expect_identical(names(name_id_cols(r$data, c("Plot", "Plant")))[1:24], names(alf))
  plants <- r$data[!is.na(.id2)]
  expect_identical(nrow(plants), 200L)
  expect_identical(plants$.id1, alf$`Plot ObsUnitID`[match(plants$.id2, alf$`Plant ObsUnitID`)])
  expect_identical(nrow(r$data[is.na(.id2)]), 20L)
})
