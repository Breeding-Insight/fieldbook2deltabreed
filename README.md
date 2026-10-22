# fieldbook2deltabreed

An R/Shiny app that converts Field Book `_database` exports into wide tables for
appending trait observations to existing DeltaBreed experiments. It reshapes
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
packages listed in `DESCRIPTION`, including Shiny, before starting the app.
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
   for example, set to `Plant` for a `Plot > Plant` hierarchy or `Entry` for a single level.
   Confirm the name even when the app supplies a default.
4. Review the identifier and trait columns in the table. Blank template columns
   are hidden in the preview and included in the export.
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

### Optional: retrieve the level name from DeltaBreed

Enter the program's **BrAPI Base URL** and an access token from DeltaBreed's BrAPI
tab. If both are supplied before upload, the app looks up the deepest unit level
automatically. After upload, use **Get level name from DeltaBreed** to run the lookup.

The lookup uses one unit from the deepest level and compares its immediate parent
with the Field Book export. It does not validate every unit or perform an import.
On lookup failure, the app reports a warning and uses the typed level name.
The token is sent in the Authorization header, rather than the URL. This feature
has been checked with mocked responses; live DeltaBreed behavior remains unverified.

## Input and output

The input must contain `ObservationUnitDbId`, `trait`, and `value`. `timestamp` is
optional. Nested exports identify ancestors in columns named for their levels
(for example, `Plot`), with values in Field Book's `<uuid> [<key>]` format.

The output contains, in order:

| Columns | Behavior |
| --- | --- |
| DeltaBreed template columns | Always present and blank, including `Row`, `Column`, and `RTK`. Single-level exports use 20 template columns; nested exports use 22, adding `Sub-Obs Unit` and `Sub Unit ID`. |
| `<Level> ObsUnitID` | One identifier column per level, ordered from parent to child, such as `Plot ObsUnitID` followed by `Plant ObsUnitID`. Ancestor IDs repeat on descendant rows. |
| Trait columns | One column per trait, populated with the observed values. |
| `TS:<trait>` | Included when timestamps are available. Three-digit milliseconds are removed and spaces are replaced with `T`. |

Only unit identifiers, traits, and timestamps are populated. Source metadata such
as germplasm, coordinates, and RTK values is not copied into the blank template
columns. This layout assumes DeltaBreed fills experiment metadata during append;
that behavior still needs confirmation with the current outputs.

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
| DeltaBreed rejects an exported file | Check the unit-level names and target experiment, and retain the input, output, and exact import error when reporting the issue. Local conversion success does not establish import compatibility. |

## Development and validation

| Path | Purpose |
| --- | --- |
| `server.r` | Conversion functions, optional BrAPI lookup, and Shiny server logic. |
| `ui.r` | Upload controls, level-name input, status messages, and table layout. |
| `DESCRIPTION` | App metadata and R dependencies. |
| `global.R` | Startup check and automatic installation of missing dependencies. |
| `run.R` | Launcher that bootstraps dependencies, including Shiny. |
| `www/` | Static app assets. |
| `data/` | Gitignored local reference inputs; not distributed with the repository. |

Conversion functions currently live outside `server()` in `server.r` and can be
called from an R script. A committed `testthat` suite is still pending; earlier
local regression checks are recorded in the project instructions. Three- and
four-level hierarchies have been exercised with synthetic inputs only. DeltaBreed
v1.3 acceptance of more than two levels and mixed depths remains unverified.

For conversion changes, check source unit counts, identifiers, parent links,
values, timestamps, and template headers. Then smoke-test browser downloads and
verify representative top-level and sub-entity imports in DeltaBreed. Keep these
three forms of evidence distinct: static inspection, local R execution, and a
real DeltaBreed import.

Keep reference inputs in `data/` unchanged and write any generated local files
to a separate gitignored directory. Report bugs through
[GitHub issues](https://github.com/Breeding-Insight/fieldbook2deltabreed/issues),
including a reproducible example and the expected output when possible.
