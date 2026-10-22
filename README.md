# fieldbook2deltabreed

An R package and Shiny app that converts Field Book `_database` exports into wide
tables for appending trait observations to existing DeltaBreed experiments. It reshapes
observations into one row per observed unit, retains parent identifiers for nested
units, and provides CSV, Excel, and clipboard exports.

The current code targets DeltaBreed v1.3 template layouts. Conversion and mocked
BrAPI checks have run locally; successful imports of the current outputs into
DeltaBreed have not yet been confirmed. Import validation is tracked in
[GitHub issue #5](https://github.com/Breeding-Insight/fieldbook2deltabreed/issues/5).

## Run locally

You need R and a web browser. RStudio is optional; the repository includes an
RStudio project file.

Clone the repository and open it as your working directory:

```sh
git clone https://github.com/Breeding-Insight/fieldbook2deltabreed.git
cd fieldbook2deltabreed
```

In R, start the app from the repository root:

```r
source("run.R")
```

Or run `Rscript run.R` from a terminal. The launcher automatically installs missing
packages listed in `DESCRIPTION` (`Imports` and `Suggests`, except `testthat`),
including Shiny, before starting the app.
Existing packages are kept. Installation needs internet access and a writable R
library; if the default library is not writable, the app uses `R_LIBS_USER`.
Your configured package repositories are used, with `https://cloud.r-project.org`
as the default CRAN mirror when none has been selected.

Once Shiny is installed, RStudio's **Run App** button or `shiny::runApp()` also
checks and installs missing dependencies through `global.R` before loading the UI
and server. Installation progress and failures appear in the R console.

## Convert an export

1. Export observations from Field Book in **database** format (`_database`), rather
   than table format (`_table`).
2. Upload one or more `.csv`, `.txt`, `.xls`, or `.xlsx` files. The app accepts up
   to 10 files per session.
3. Review the status messages and the detected unit levels, listed from top to
   bottom. Set **Most-defined unit level** to the deepest level in your export,
   such as `Plant` for a `Plot > Plant` hierarchy or `Tank` for a single tank level.
   Confirm the name even when the app supplies a default.
4. Review the identifier and trait columns in the table. Blank template columns
   are hidden in the preview and included in the export. Template columns filled
   from DeltaBreed are shown for review.
5. Use **CSV**, **Excel**, or **Copy** above the table. Clear any table search
   before exporting the complete dataset.
6. Append the result to the corresponding DeltaBreed experiment and unit level.
   Confirm the import result and observation counts in DeltaBreed.

Files with matching detected unit-level names are combined into one table;
different trait sets are allowed and missing cells are left empty. Files with
different hierarchies cannot be combined. Failed files are reported in the status
area; a table may still contain the successfully processed files, so check the
status before downloading. Combined files are concatenated without deduplication.
Use **Clear All Files** to start a new batch.

### Optional: connect to DeltaBreed

Enter the program's **BrAPI Base URL** and an access token from DeltaBreed's BrAPI
tab. If both are supplied before upload, the app connects automatically. After
upload, use **Get details from DeltaBreed** to connect. A connection does two things:

1. **Level name.** The app looks up one unit from the deepest level, fills
   **Most-defined unit level** with its DeltaBreed level, and compares its immediate
   parent with the Field Book export. On failure, the app reports a warning, uses
   the typed level name, and skips step 2.
2. **Template columns.** The app reads every observation unit in the program, with
   its experiment, environment, and season, and fills the template columns that a
   DeltaBreed experiment export fills: germplasm, GID, test or check, experiment
   title, description and type, unit levels and unit IDs, environment, location,
   year, replicate, block, row, column, and treatment factors. Rows on a sub-unit
   take germplasm, replicate, block, row, and column from their top-level unit when
   DeltaBreed does not record them on the sub-unit.

A unit that DeltaBreed does not list, or lists under a different parent, keeps
blank template columns; the status reports how many and gives an example. Rows
below the first sub-level leave `Sub-Obs Unit` and `Sub Unit ID` blank. If the
unit details cannot be fetched, the status reports a warning and the template
columns stay blank. Large programs take longer, because the whole program is read.

The token is sent in the Authorization header, rather than the URL, and is not
stored. Both steps have been checked with mocked responses only. Live DeltaBreed
behavior, and whether DeltaBreed accepts filled template columns on append, remain
unverified. To produce blank template columns instead, leave the URL or token empty.

## Use from R

The conversion functions are also an R package, so a script can convert files
without the app:

```r
remotes::install_github("Breeding-Insight/fieldbook2deltabreed")
library(fieldbook2deltabreed)

result <- process_single_file("2025-09-11-03-06-53_WASHINGTON_database.csv")
result$message
result$level_names  # NA marks the most-defined level, which you name
out <- name_id_cols(result$data, "Plot")
```

`process_single_file()` returns the table with identifier columns `.id1` to `.idk`,
from the top level down; `name_id_cols()` renames them to `<Level> ObsUnitID`. To
fill the template columns from DeltaBreed, pass the result of
`deltabreed_units(base_url, token)` to `backfill_template()` before naming the
columns. Writing the file is up to the script; it does not go through the app's
CSV and Excel buttons. See `?process_single_file` for the full return value.

## Input and output

The input must contain `ObservationUnitDbId`, `trait`, and `value`. `timestamp` is
optional. Nested exports identify ancestors in columns named for their levels
(for example, `Plot`), with values in Field Book's `<uuid> [<key>]` format.

The output contains, in order:

| Columns | Behavior |
| --- | --- |
| DeltaBreed template columns | Always present, including `Row`, `Column`, and `RTK`. Blank unless DeltaBreed is connected; then filled from DeltaBreed, except `Lat`, `Long`, `Elevation`, and `RTK`. Single-level exports use 20 template columns; nested exports use 22, adding `Sub-Obs Unit` and `Sub Unit ID`. |
| `<Level> ObsUnitID` | One identifier column per level, ordered from parent to child, such as `Plot ObsUnitID` followed by `Plant ObsUnitID`. Ancestor IDs repeat on descendant rows. |
| Trait columns | One column per trait, populated with the observed values. |
| `TS:<trait>` | Included when timestamps are available. Three-digit milliseconds are removed and spaces are replaced with `T`. |

Without a DeltaBreed connection, only unit identifiers, traits, and timestamps are
populated. Field Book metadata such as germplasm, coordinates, and RTK values is
never copied into the template columns. The blank layout assumes DeltaBreed fills
experiment metadata during append; that behavior still needs confirmation with
the current outputs.

Each observed unit produces one row; an ancestor referenced by a child does not
gain a separate row unless it has observations of its own. Excel exports start
with the column headers, without a title row.

## Troubleshooting

| Message or symptom | What to check |
| --- | --- |
| Incorrect table format or missing required columns | Use a Field Book `_database` export and check for `ObservationUnitDbId`, `trait`, and `value`. |
| Repeated observations | Each unit–trait pair must occur only once within a file. Repeated measures are rejected instead of aggregated; resolve them before conversion. |
| Missing identifier or inconsistent nesting | Every observation needs an ID, and each nested unit must belong to exactly one parent at each ancestor level. |
| Unit-level order cannot be determined | Review the displayed hierarchy. Levels with indistinguishable counts retain their input column order. |
| Blank or duplicate level name | Enter a nonempty deepest-level name that differs from all ancestor level names. |
| Trait name clashes with template columns | Check trait names against the DeltaBreed template headers and resolve the naming conflict before conversion. |
| Units not found in DeltaBreed, or under a different parent | Check that the BrAPI Base URL is for the program the Field Book file came from, and that the experiment in DeltaBreed matches the one loaded into Field Book. Those rows keep blank template columns. |
| DeltaBreed rejects an exported file | Check the unit-level names and target experiment, and retain the input, output, and exact import error when reporting the issue. Local conversion success does not establish import compatibility. |

## Development and validation

| Path | Purpose |
| --- | --- |
| `R/` | Conversion functions, optional BrAPI lookup and template backfill. The package code; the app sources the same files. |
| `server.r` | Shiny server logic. |
| `ui.r` | Upload controls, level-name input, status messages, and table layout. |
| `DESCRIPTION` | Package metadata. `Imports` lists what `R/` needs; `Suggests` lists the app's packages and `testthat`. |
| `NAMESPACE`, `man/` | Generated by `roxygen2::roxygenise()` from the comments in `R/`; do not edit by hand. |
| `tests/testthat/` | Regression tests for the conversion functions, the mocked BrAPI calls, and the Shiny server. |
| `global.R` | Startup check and automatic installation of missing dependencies. |
| `run.R` | Launcher that bootstraps dependencies, including Shiny. |
| `www/` | Static app assets. |
| `data/` | Gitignored local reference inputs; not distributed with the repository. |

The app files (`ui.r`, `server.r`, `global.R`, `run.R`, `www/`) are left out of the
built package by `.Rbuildignore`. Shiny sources `R/` when the app starts and warns
that the directory appears to contain an R package; the warning is expected.

Run the tests from the repository root:

```r
testthat::test_local()
```

The tests generate synthetic Field Book exports and a mocked DeltaBreed BrAPI, so
no real server is contacted. Tests that need files in `data/`, or the app files,
skip when those are missing, as they are under `R CMD check`. Three- and four-level
hierarchies have been exercised with synthetic inputs only. DeltaBreed v1.3
acceptance of more than two levels and mixed depths remains unverified.

For conversion changes, check source unit counts, identifiers, parent links,
values, timestamps, and template headers. Then smoke-test browser downloads and
verify representative top-level and sub-entity imports in DeltaBreed. Keep these
three forms of evidence distinct: static inspection, local R execution, and a
real DeltaBreed import.

Keep reference inputs in `data/` unchanged and write any generated local files
to a separate gitignored directory. Report bugs through
[GitHub issues](https://github.com/Breeding-Insight/fieldbook2deltabreed/issues),
including a reproducible example and the expected output when possible.
