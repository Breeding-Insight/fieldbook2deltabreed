library(shiny)
library(shinydashboard)
library(DT)
library(tools)
library(readxl)
library(openxlsx)
library(data.table)
library(bslib)
library(shinycssloaders)
library(shinyjs)

required_packages <- c("shiny", "shinydashboard", "shinyFiles", "DT", "tools", "readxl", "openxlsx", "data.table", "bslib", "shinycssloaders", "shinyjs")

install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) install.packages(new_packages)
}

ui <- page_fluid(
  useShinyjs(),
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2c7be5"
  ),
  tags$head(
    tags$style(HTML("
      .hero {
        background: linear-gradient(90deg, #e9f2ff, #fff);
        border: 1px solid #e6ecf2;
        border-radius: 10px;
        padding: 16px 20px;
        margin-bottom: 16px;
        display: flex;
        align-items: center;
        gap: 16px;
      }
      #top-left-image {
        max-width: 140px;
        max-height: 110px;
        border-radius: 6px;
      }
      .list-group { padding-left: 0; margin-bottom: 0; }
      .list-group-item { padding: 6px 10px; border: 1px solid #e9ecef; }
      .dt-buttons .dt-button {
        background-color: #28a745 !important;
        border-color: #28a745 !important;
        color: #fff !important;
      }
      .dt-buttons .dt-button:hover, .dt-buttons .dt-button:focus {
        background-color: #218838 !important;
        border-color: #1e7e34 !important;
        color: #fff !important;
      }
      .dt-export-prefix {
        margin-right: 10px;
        margin-bottom: 10px;
        font-weight: 600;
        color: #198754 !important;
        background-color: transparent !important;
        border: none !important;
        cursor: default !important;
        pointer-events: none;
        display: inline-block;
        font-size: 14px;
      }
      #export-indicator {
        text-align: left;
        margin-bottom: 5px;
      }
      .empty-state {
        border: 1px dashed #cfe2ff;
        background-color: #f8fbff;
        border-radius: 8px;
        padding: 32px;
        text-align: center;
        color: #6c757d;
      }
      .empty-state h4 { margin-top: 8px; color: #0d6efd; margin-bottom: 8px; }
      .empty-state p { margin: 0; color: #6c757d; }
    "))
  ),
  div(class = "hero",
    img(id = "top-left-image", src = "Slide1.jpeg", alt = "Logo"),
    div(
      h2("Convert Fieldbook database output to Deltabreed format", style = "margin: 0;"),
      div("Upload one or more Fieldbook '_database' files and download a combined, Deltabreed-ready Excel.", style = "color:#6c757d;")
    )
  ),
  sidebarLayout(
    sidebarPanel(width = 4,
      h4("Status"),
      uiOutput("feedback_ui"),
      br(),
      fileInput("files", "Select multiple Fieldbook output files to upload.", multiple = TRUE, accept = c(".csv", ".txt", ".xls", ".xlsx")),
      h5("Uploaded Files"),
      uiOutput("fileList"),
      br(),
      actionButton("clearFiles", label = "Clear All Files", icon = icon("trash"), class = "btn btn-danger"),
      br(), br(),
      
    ),
    mainPanel(width = 8,
      h3("Trait data to be imported into Deltabreed:"),
      uiOutput("main_content")
    )
  )
)
