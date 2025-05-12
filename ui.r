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

ui <- fluidPage(
  titlePanel("Convert Fieldook database output to Deltabreed format"),
  sidebarLayout(
    sidebarPanel(
      h3("User Feedback:"),
      textOutput("feedback"),
      br(),
      fileInput("file1", "Select 'browse' to select a Fieldbook output file that to upload."),
      h5("After Fieldbook output file is selected, click the button below to download the file in Deltabreed-ready format."),
      downloadButton("download", label = "Download"),
    ),
    mainPanel(
      h3("Fieldbook Output Viewer:"),
      tableOutput("table")
    )
  )
)
