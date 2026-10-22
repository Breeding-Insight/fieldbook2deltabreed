# Shiny sources this before ui.r and server.r. Keep DESCRIPTION as the dependency list:
# Imports for the conversion functions, Suggests for the app (testthat is only for the tests).
local({
  fields <- read.dcf("DESCRIPTION", fields = c("Imports", "Suggests"))[1, ]
  packages <- sub("\\s*\\(.*\\)$", "", trimws(unlist(strsplit(fields, ","))))
  packages <- setdiff(packages, "testthat")
  missing_packages <- function() {
    packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  }
  missing <- missing_packages()
  if (length(missing) == 0) return(invisible(NULL))

  # Avoid install.packages() prompting for a library on a fresh R installation.
  lib <- .libPaths()[1]
  if (file.access(lib, 2) != 0) {
    lib <- strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep, fixed = TRUE)[[1]][1]
    if (is.na(lib) || !nzchar(lib)) {
      stop("No writable R library. Set R_LIBS_USER to a writable directory and restart R.", call. = FALSE)
    }
    lib <- path.expand(lib)
    dir.create(lib, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(lib) || file.access(lib, 2) != 0) {
      stop("Cannot write to R library: ", lib, ". Set R_LIBS_USER to a writable directory and restart R.", call. = FALSE)
    }
    .libPaths(c(lib, .libPaths()))
  }

  # Honour configured repositories; supply CRAN when R would otherwise ask for a mirror.
  repos <- getOption("repos")
  if (length(repos) == 0) repos <- c(CRAN = "https://cloud.r-project.org")
  repos[repos == "@CRAN@"] <- "https://cloud.r-project.org"
  message("Installing missing app dependencies: ", paste(missing, collapse = ", "))
  tryCatch(
    utils::install.packages(missing, lib = lib, repos = repos),
    error = function(e) stop("Dependency installation failed: ", conditionMessage(e), call. = FALSE)
  )

  # install.packages() can warn instead of throwing an error, so verify the result.
  missing <- missing_packages()
  if (length(missing) > 0) {
    stop("Could not load dependencies: ", paste(missing, collapse = ", "),
      ". Check the installation messages, internet connection, and library permissions, then restart the app.",
      call. = FALSE)
  }
})
