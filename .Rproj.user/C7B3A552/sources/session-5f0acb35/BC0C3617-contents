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

# Define UI (User Interface) ####
ui <- dashboardPage(
  ## dashboard header ####
  dashboardHeader(title = "Weather Monitoring"),  # Header with the title of the dashboard
  ## dashboard sidebar ####
  dashboardSidebar(
    sidebarMenu(
      menuItem("Weather Data", tabName = "weather", icon = icon("cloud")),  # Sidebar menu item
      div(
        class = "sidebar-menu", style = "padding: 10px;",
        numericInput("longitude", "Longitude:", value = 100.10),  # Input for longitude
        numericInput("latitude", "Latitude:", value = 13.10),  # Input for latitude
        dateInput("selected_date", "Selected Date:", value = Sys.Date(), max = Sys.Date() + 7),  # Date input
        div(
          style = "margin: 5px 0;",
          actionButton("run", "Get data and run", class = "btn-primary btn-sm"),  # Button to fetch data
          downloadButton("download_data", "Download CSV", class = "btn-sm")  # Button to download data as CSV
        ),
        tags$small("Click on map to set location")  # Instruction to click on map for setting location
      )
    )
  ),
  ## dashboard body ####
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: white; }
        .skin-blue .main-header .logo { font-size: 14px; }
        .form-control { font-size: 12px; height: 28px; padding: 4px 8px; }
        .btn { font-size: 12px; padding: 4px 8px; }
      "))  # Custom CSS to style the dashboard
    ),
    
    tabItems(
      tabItem(tabName = "weather",  # Tab for weather data
              fluidRow(
                column(width = 6,
                       box(
                         width = NULL, leafletOutput("map", height = 350),  # Output map using leaflet
                         title = "Location Map", status = "primary", solidHeader = TRUE  # Box title and styling
                       )
                ),
                column(width = 6,
                       box(
                         width = NULL, dataTableOutput("data_table", height = 350),  # Output data table
                         title = "Weather Data", status = "primary", solidHeader = TRUE  # Box title and styling
                       )
                )
              ),
              fluidRow(
                column(width = 4,
                       box(
                         width = NULL, plotlyOutput("tc_plot", height = 250),  # Output temperature plot
                         title = "Temperature (°C)", status = "info", solidHeader = TRUE  # Box title and styling
                       )
                ),
                column(width = 4,
                       box(
                         width = NULL, plotlyOutput("hr_plot", height = 250),  # Output humidity plot
                         title = "Humidity (%)", status = "info", solidHeader = TRUE  # Box title and styling
                       )
                ),
                column(width = 4,
                       box(
                         width = NULL, plotlyOutput("rain_plot", height = 250),  # Output rainfall plot
                         title = "Rainfall (mm)", status = "info", solidHeader = TRUE  # Box title and styling
                       )
                )
              )
      )
    )
  )
)

### Custom CSS for sidebar ####
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

# Server Definition ####
server <- function(input, output, session) {
  
  ## Map interactions ####
  observe({
    # Update map markers based on longitude and latitude inputs
    leafletProxy("map") %>% 
      clearMarkers() %>%
      addMarkers(lng = input$longitude, lat = input$latitude)
  })
  
  observeEvent(input$map_click, {
    # Update longitude and latitude inputs based on map click
    updateNumericInput(session, "longitude", value = input$map_click$lng)
    updateNumericInput(session, "latitude", value = input$map_click$lat)
  })
  
  output$map <- renderLeaflet({
    # Render initial map with default longitude and latitude
    leaflet() %>% 
      addTiles() %>%
      setView(lng = input$longitude, lat = input$latitude, zoom = 6) %>%
      addMarkers(lng = input$longitude, lat = input$latitude)
  })
  
  # Data fetching
  data_reactive <- eventReactive(input$run, {
    req(input$longitude, input$latitude)  # Ensure longitude and latitude inputs are available
    
    withProgress(message = 'Fetching data...', {
      tryCatch({
        json <- request("https://data.tmd.go.th/nwpapi/v1/forecast/location/hourly/at") |>
          req_auth_bearer_token(Sys.getenv("tmd_token")) |>
          req_url_query(
            lat = input$latitude,
            lon = input$longitude,
            fields = 'cond,tc,rh,rain',
            date = as.character(input$selected_date),
            hour = 0,
            duration = 24
          ) |>
          req_perform() |>
          resp_body_json()
        
        # Process the JSON response to extract weather data
        json |> 
          pluck(1, 1) |> 
          pluck("forecasts") |> 
          map_dfr(\(x) {
            tibble(
              time = ymd_hms(x |> pluck("time")),
              tc = x |> pluck("data", "tc"),
              rh = x |> pluck("data", "rh"),
              rain = x |> pluck("data", "rain"),
              cond = x |> pluck("data", "cond")
            )
          })
      }, error = function(e) {
        # Show notification in case of an error
        showNotification("Error fetching data", type = "error")
        NULL
      })
    })
  })
  
  # Plot theme
  plot_theme <- theme_minimal() +
    theme(
      text = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      plot.margin = margin(2, 2, 2, 2)
    )
  
  # Plots
  output$tc_plot <- renderPlotly({
    req(data_reactive())  # Ensure data is available
    gg <- ggplot(data_reactive(), aes(x = time, y = tc)) +
      geom_line(color = "red") +
      geom_point(color = "darkred") +
      scale_y_continuous(limits = c(0, 50)) +
      labs(x = "Time", y = "°C") +
      plot_theme
    ggplotly(gg) %>% 
      layout(
        showlegend = FALSE,
        margin = list(l = 40, r = 20, t = 20, b = 40),
        font = list(size = 10)
      )
  })
  
  output$hr_plot <- renderPlotly({
    req(data_reactive())  # Ensure data is available
    gg <- ggplot(data_reactive(), aes(x = time, y = rh)) +
      geom_line(color = "blue") +
      geom_point(color = "darkblue") +
      scale_y_continuous(limits = c(50, 100)) +
      labs(x = "Time", y = "%") +
      plot_theme
    ggplotly(gg) %>% 
      layout(
        showlegend = FALSE,
        margin = list(l = 40, r = 20, t = 20, b = 40),
        font = list(size = 10)
      )
  })
  
  output$rain_plot <- renderPlotly({
    req(data_reactive())  # Ensure data is available
    gg <- ggplot(data_reactive(), aes(x = time, y = rain)) +
      geom_line(color = "green") +
      geom_point(color = "darkgreen") +
      scale_y_continuous(limits = c(0, 10)) +
      labs(x = "Time", y = "mm") +
      plot_theme
    ggplotly(gg) %>% 
      layout(
        showlegend = FALSE,
        margin = list(l = 40, r = 20, t = 20, b = 40),
        font = list(size = 10)
      )
  })
  
  # Data table
  output$data_table <- renderDataTable({
    req(data_reactive())  # Ensure data is available
    data_reactive() %>%
      mutate(
        time = format(time, "%Y-%m-%d %H:%M"),
        tc = round(tc, 1),
        rh = round(rh, 1),
        rain = round(rain, 2)
      ) %>%
      datatable(
        options = list(
          pageLength = 24,
          dom = 'tp',
          scrollY = "300px",
          scrollCollapse = TRUE
        ),
        colnames = c("Time", "Temp", "Humid", "Rain", "Cond"),
        class = 'compact'
      )
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("weather_data_", format(Sys.Date(), "%Y%m%d"), ".csv")  # Generate filename based on current date
    },
    content = function(file) {
      write.csv(data_reactive(), file, row.names = FALSE)  # Write data to CSV file
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)