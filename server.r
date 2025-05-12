library(shiny)
library(shinydashboard)
library(DT)
library(tools)
library(readxl)
library(openxlsx)
library(data.table)

required_packages <- c("shiny", "shinydashboard", "shinyFiles", "DT", "tools", "readxl", "openxlsx", "data.table")

install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) install.packages(new_packages)
}

server <- function(input, output, session) { # nolint
  observe({
    if (is.null(input$file1)) {
      output$feedback <- renderText({
        "Please upload a Fieldbook output '_database' file in .csv or .txt format."
      })
    } else {
      req(input$file1)
      file1 <- input$file1
      fileType <- file_ext(file1$datapath)
      
      # Upload data AS DATA.TABLES (DTs) using native data.table function fread
      if (fileType == "xls" || fileType == "xlsx") {
        # If pathname for fb is xls
        fb <- as.data.table(read_excel(file1$datapath))
        output$feedback <- renderText({
          ".xlsx or .xls detected, while the file can be converted it is strongly suggested to upload the Fieldbook output file in .csv or .txt to maintain data integrity."
        })
      } else if (fileType == "csv" || fileType == "txt") {
        # If pathname for fb is csv
        fb <- fread(file1$datapath, colClasses = c(timestamp = "character"))
      } else {
        output$feedback <- renderText({
          "Unknown file detected, please upload a Fieldbook output '_database' file in .csv or .txt format."
        })
        fb <- data.table(ObservationUnitDbId = character(), trait = character(), value = numeric())
      }
      
      if (!"value" %in% colnames(fb)) {
        fb <- data.table(ObservationUnitDbId = character(), trait = character(), value = numeric())
        output$feedback <- renderText({
          "Incorrect table format. Check if Fieldbook output was uploaded in table format (original file name ends in '_table'), and try uploading a Fieldbook output in database.csv format (file ending in '_database.csv')."
        })
      }
      
      output$table <- renderTable({
        fb
      }, rownames = FALSE)
      
      # Rename observation unit ID column in Field Book to match Deltabreed
      setnames(fb, "ObservationUnitDbId", "ObsUnitID")
      if (!all(is.na(fb$timestamp))) {
        setnames(fb, "timestamp", "TS")
        # Remove the period and the three digits that follow
        fb$TS <- sub("\\.[0-9]{3}", "", fb$TS)
        # Replace the space with a T
        fb$TS <- gsub(" ", "T", fb$TS)
        # Pivot the data.table wider
        fbWide <- dcast(fb, ObsUnitID ~ trait, value.var = c("value", "TS"))
        setnames(fbWide, gsub("value_", "", names(fbWide)))
        setnames(fbWide, gsub("TS_", "TS:", names(fbWide)))
      }
      
      # Pivot the data.table wider
      fbWide <- dcast(fb, ObsUnitID ~ trait, value.var = c("value"))
      setnames(fbWide, gsub("value_", "", names(fbWide)))
      
      dt <- data.table(
        "Germplasm Name" = NA,
        "Germplasm GID" = NA,
        "Test (T) or Check (C)" = NA,
        "Exp Title" = NA,
        "Exp Description" = NA,
        "Exp Unit" = NA,
        "Sub-Obs Unit" = NA,
        "Exp Type" = NA,
        "Env" = NA,
        "Env Location" = NA,
        "Env Year" = NA,
        "Exp Unit ID" = NA,
        "Exp Replicate #" = NA,
        "Exp Block #" = NA,
        "Row" = NA,
        "Column" = NA,
        "Lat" = NA,
        "Long" = NA,
        "Elevation" = NA,
        "RTK" = NA,
        "Treatment Factors" = NA,
        "ObsUnitID" = unique(fb$ObsUnitID)
      )
      
      # Combine db1 with fb2_wide
      dbReady <- dt[fbWide, on = "ObsUnitID"]
      
      output$download <- downloadHandler(
        filename = function() {
          paste0("deltaBreedFormat_", Sys.time(), ".xlsx")
        },
        content = function(file) {
          write.xlsx(dbReady, file, sheetName = "Data")
        }
      )
    }
  }) %>%
    bindEvent(input$file1, input$file2, input$download)
}
