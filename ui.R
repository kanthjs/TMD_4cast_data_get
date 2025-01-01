# Load required libraries
library(shiny)
library(leaflet)
library(ggplot2)
library(DT)
library(httr2)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinydashboard)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Weather Monitoring"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Weather Data", tabName = "weather", icon = icon("cloud")),
      div(
        class = "sidebar-menu", style = "padding: 10px;",
        numericInput("longitude", "Longitude:", value = 100.10),
        numericInput("latitude", "Latitude:", value = 13.10),
        dateInput("selected_date", "Selected Date:", value = Sys.Date(), max = Sys.Date() + 7),
        div(
          style = "margin: 5px 0;",
          actionButton("run", "Get data and run", class = "btn-primary btn-sm"),
          downloadButton("download_data", "Download CSV", class = "btn-sm")
        ),
        tags$small("Click on map to set location")
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: white; }
        .skin-blue .main-header .logo { font-size: 14px; }
        .form-control { font-size: 12px; height: 28px; padding: 4px 8px; }
        .btn { font-size: 12px; padding: 4px 8px; }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "weather",
              fluidRow(
                column(width = 6,
                       box(
                         width = NULL, leafletOutput("map", height = 350),
                         title = "Location Map", status = "primary", solidHeader = TRUE
                       )
                ),
                column(width = 6,
                       box(
                         width = NULL, dataTableOutput("data_table", height = 350),
                         title = "Weather Data", status = "primary", solidHeader = TRUE
                       )
                )
              ),
              fluidRow(
                column(width = 4,
                       box(
                         width = NULL, plotlyOutput("tc_plot", height = 250),
                         title = "Temperature (°C)", status = "info", solidHeader = TRUE
                       )
                ),
                column(width = 4,
                       box(
                         width = NULL, plotlyOutput("hr_plot", height = 250),
                         title = "Humidity (%)", status = "info", solidHeader = TRUE
                       )
                ),
                column(width = 4,
                       box(
                         width = NULL, plotlyOutput("rain_plot", height = 250),
                         title = "Rainfall (mm)", status = "info", solidHeader = TRUE
                       )
                )
              )
      )
    )
  )
)

# Custom CSS for sidebar
tags$style(HTML("
  .skin-blue .main-sidebar { background-color: #2C3E50; }
  .skin-blue .sidebar-menu > li.active > a {
    background-color: #34495E; border-left-color: #3498DB;
  }
  .skin-blue .sidebar-menu > li:hover > a {
    background-color: #34495E; border-left-color: #3498DB;
  }
  .skin-blue .sidebar-menu > li > a { color: #ECF0F1; }
  .sidebar-menu .form-control {
    background-color: #34495E; color: #ECF0F1; border: 1px solid #445566;
  }
  .sidebar-menu label { color: #ECF0F1; }
  .sidebar-menu .btn-primary {
    background-color: #3498DB; border-color: #2980B9;
  }
  .sidebar-menu .btn-default {
    background-color: #95A5A6; border-color: #7F8C8D; color: #ECF0F1;
  }
  .sidebar-menu small { color: #BDC3C7; }
  .sidebar-menu .form-control:focus {
    background-color: #445566; border-color: #3498DB;
  }
  .skin-blue .main-header .logo { background-color: #2C3E50; }
  .skin-blue .main-header .logo:hover { background-color: #34495E; }
  .skin-blue .main-header .navbar { background-color: #2C3E50; }
"))