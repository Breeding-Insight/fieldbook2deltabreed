test_that("levels are ordered top to bottom from the data, not the column order", {
  s <- synthetic_fieldbook()
  expect_identical(intersect(names(s$fb), c("Plot", "Plant", "Cluster")), c("Cluster", "Plant", "Plot"))
  r <- convert(s$fb)
  expect_true(r$success)
  expect_identical(r$level_names, c("Plot", "Plant", "Cluster", NA))
  expect_length(r$tied, 0)
})

test_that("every observed unit is one row, under its true ancestors", {
  s <- synthetic_fieldbook()
  out <- convert(s$fb)$data
  expect_identical(nrow(out), uniqueN(s$obs$id))
  expect_identical(names(out)[23:26], paste0(".id", 1:4))
  rows <- out[, list(.id1, .id2, .id3, .id4, own = fcoalesce(.id4, .id3, .id2, .id1),
    depth = 1L + (!is.na(.id2)) + (!is.na(.id3)) + (!is.na(.id4)))]
  chk <- s$truth[rows, on = c(id = "own")]
  expect_identical(chk$level, chk$depth)
  expect_true(all(chk[level >= 2, .id1 == plot]))
  expect_true(all(chk[level >= 3, .id2 == plant]))
  expect_true(all(chk[level == 4, .id3 == cluster]))
  expect_true(all(chk[level == 1, is.na(.id2) & is.na(.id3) & is.na(.id4)]))
  # The most-defined level's ID is unique per row; ancestor IDs repeat down their columns
  expect_false(anyDuplicated(na.omit(out$.id4)) > 0)
  expect_gt(max(out[, .N, by = .id1]$N), 1)
})

test_that("values and timestamps land in their unit's trait cells", {
  s <- synthetic_fieldbook()
  out <- convert(s$fb)$data
  own <- out[, fcoalesce(.id4, .id3, .id2, .id1)]
  cell <- function(col) mapply(function(u, t) out[[col(t)]][own == u], s$obs$id, s$obs$trait, USE.NAMES = FALSE)
  expect_equal(cell(identity), s$obs$value)
  expect_identical(cell(function(t) paste0("TS:", t)), gsub(" ", "T", sub("\\.[0-9]{3}", "", s$obs$timestamp)))
})

test_that("each unit's row comes before its sub-units' rows", {
  out <- convert(synthetic_fieldbook()$fb)$data
  ids <- as.matrix(out[, paste0(".id", 1:4), with = FALSE])
  depth <- rowSums(!is.na(ids))
  own <- ids[cbind(seq_along(depth), depth)]
  parent_row <- match(ids[cbind(seq_along(depth), pmax(depth - 1, 1))], own)
  sub <- depth > 1 & !is.na(parent_row)
  expect_true(any(sub))
  expect_true(all(parent_row[sub] < which(sub)))
})

test_that("every template column is written, blank, in DeltaBreed's order", {
  multi <- convert(synthetic_fieldbook()$fb)$data
  expect_identical(names(multi)[1:22], sub_template_cols)
  expect_true(all(is.na(as.matrix(multi[, 1:22]))))
  single <- convert(single_level_fieldbook())$data
  expect_identical(names(single)[1:21], c(top_template_cols, ".id1"))
  expect_true(all(is.na(as.matrix(single[, 1:20]))))
})

test_that("name_id_cols() names the ID columns after the levels and leaves its input alone", {
  r <- convert(synthetic_fieldbook()$fb)
  named <- name_id_cols(r$data, c("Plot", "Plant", "Cluster", "Specimen"))
  expect_identical(names(named)[23:26], c("Plot ObsUnitID", "Plant ObsUnitID", "Cluster ObsUnitID", "Specimen ObsUnitID"))
  expect_identical(names(r$data)[23:26], paste0(".id", 1:4))
  expect_identical(unname(as.list(named)), unname(as.list(r$data)))
})

test_that("a single-level file takes its level name from its one empty column", {
  expect_identical(convert(single_level_fieldbook(empty_col = "Tank"))$level_names, "Tank")
  expect_identical(convert(single_level_fieldbook())$level_names, NA_character_)
  # Two empty columns could each be the name, so it is typed instead
  expect_identical(convert(single_level_fieldbook(empty_col = c("Tank", "Bench")))$level_names, NA_character_)
})

test_that("a path and a fileInput row give the same result", {
  path <- write_fieldbook(synthetic_fieldbook()$fb)
  expect_identical(process_single_file(path), process_single_file(list(datapath = path, name = basename(path))))
})

test_that("bad inputs give a file-specific error and no table", {
  s <- synthetic_fieldbook()
  rejected <- function(fb, pattern, ...) {
    r <- convert(fb, "rejected")
    expect_false(r$success)
    expect_null(r$data)
    expect_match(r$message, pattern, ...)
  }
  rejected(s$fb[, !"value"], "Incorrect table format")
  rejected(s$fb[, !"trait"], "Missing required column(s): trait", fixed = TRUE)
  rejected(s$fb[, !c("ObservationUnitDbId", "trait")], "Missing required column(s): ObservationUnitDbId, trait", fixed = TRUE)
  rejected(copy(s$fb)[1, ObservationUnitDbId := ""], "no ObservationUnitDbId")
  # dcast would count repeats silently
  rejected(rbind(s$fb, s$fb[1]), "repeated observation")
  rejected(copy(s$fb)[trait == "Yield", trait := "RTK"], "clash with DeltaBreed template columns: RTK")
  # A cluster's row without its plant
  rejected(copy(s$fb)[which(Cluster != "")[1], Plant := ""], "do not nest")
  # A plant's own observation listed under a second plot
  plant_row <- s$fb[Plot != "" & Plant == ""][1]
  moved <- copy(plant_row)[, `:=`(trait = "Width", Plot = s$fb[Plot != "" & Plot != plant_row$Plot]$Plot[1])]
  rejected(rbind(s$fb, moved), "more than one parent")

  json <- tempfile(fileext = ".json")
  writeLines("{}", json)
  expect_match(process_single_file(json)$message, "Unknown file type")
  broken <- tempfile(fileext = ".xlsx")
  writeLines("not a workbook", broken)
  expect_match(process_single_file(broken)$message, "^Error reading file")
})

test_that("levels the data cannot order are flagged", {
  s <- synthetic_fieldbook()
  # One specimen row per plot: Plot, Plant and Cluster are filled on the same rows with the same counts
  r <- convert(s$fb[Cluster != ""][, .SD[1], by = "Plot"], "tied")
  expect_true(r$success)
  expect_setequal(r$tied, c("Plot", "Plant", "Cluster"))
})
