# Package-level imports. The Shiny app also sources this file, where it does nothing
#' @keywords internal
#' @import data.table
#' @importFrom readxl read_excel
#' @importFrom stats setNames
#' @importFrom tools file_ext
#' @importFrom utils globalVariables
"_PACKAGE"

# Columns named in data.table expressions
utils::globalVariables("ObservationUnitDbId")
