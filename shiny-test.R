# Required packages
library(shiny)
library(leaflet)
library(ggplot2)
library(DT)
library(httr2)
library(tidyverse)
library(lubridate)
library(plotly)

#' UI Definition
#' Organized in a fluid layout with map, data table, and weather plots
ui <- fluidPage(
  # Application title
  titlePanel("Plant Disease Warning Systems"),
  
  # Main content area
  fluidRow(
    # Left sidebar with inputs
    column(width = 3,
           wellPanel(
             numericInput("longitude", "Longitude:", value = 100.10),
             numericInput("latitude", "Latitude:", value = 13.10),
             dateInput("selected_date", "Selected Date:", 
                       value = Sys.Date(),
                       max = Sys.Date() + 7),  # TMD API allows forecast up to 7 days
             actionButton("run", "Get data and run", 
                          class = "btn-primary"),
             br(), br(),
             downloadButton("download_data", "Download CSV"),
             hr(),
             helpText("Click on map to set location")
           )
    ),
    
    # Main panel with map and plots
    column(width = 9,
           # Map and data table row
           fluidRow(
             column(width = 6, 
                    # Interactive map
                    leafletOutput("map", height = 400)
             ),
             column(width = 6,
                    # Data table showing weather data
                    dataTableOutput("data_table", height = 200)
             )
           ),
           
           # Weather plots row
           fluidRow(
             # Temperature plot
             column(width = 4,
                    h4("Temperature Over Time"),
                    plotlyOutput("tc_plot", height = 300)
             ),
             # Humidity plot
             column(width = 4,
                    h4("Humidity Over Time"),
                    plotlyOutput("hr_plot", height = 300)
             ),
             # Rainfall plot
             column(width = 4,
                    h4("Precipitation Over Time"),
                    plotlyOutput("rain_plot", height = 300)
             )
           )
    )
  )
)

#' Server Definition
#' Handles data fetching, map interactions, and plot rendering
server <- function(input, output, session) {
  
  # Update map marker when coordinates change
  observe({
    leafletProxy("map") %>% 
      clearMarkers() %>%
      addMarkers(lng = input$longitude, lat = input$latitude)
  })
  
  # Update coordinates when map is clicked
  observeEvent(input$map_click, {
    updateNumericInput(session, "longitude", value = input$map_click$lng)
    updateNumericInput(session, "latitude", value = input$map_click$lat)
  })
  
  # Render initial map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(lng = input$longitude, lat = input$latitude, zoom = 6) %>%
      addMarkers(lng = input$longitude, lat = input$latitude)
  })
  
  # Fetch weather data from TMD API
  data_reactive <- eventReactive(input$run, {
    req(input$longitude, input$latitude)
    
    # TMD API base URL
    base.url <- "https://data.tmd.go.th/nwpapi/v1/forecast/location/hourly/at"
    
    # Error handling for API request
    withProgress(message = 'Fetching weather data...', {
      tryCatch({
        json <- request(base.url) |>
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
        
        # Transform JSON response to tibble
        data <- json |> 
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
        
        # Return processed data
        data
        
      }, error = function(e) {
        showNotification("Error fetching weather data. Please try again later.", 
                         type = "error")
        NULL
      })
    })
  })
  
  # Render temperature plot
  output$tc_plot <- renderPlotly({
    req(data_reactive())
    gg <- ggplot(data_reactive(), aes(x = time, y = tc)) +
      geom_line(color = "red") +
      geom_point(color = "darkred") +
      scale_y_continuous(limits = c(0, 50)) +
      labs(x = "Time", y = "Temperature (°C)") +
      theme_minimal()
    ggplotly(gg) %>% layout(showlegend = FALSE)
  })
  
  # Render humidity plot
  output$hr_plot <- renderPlotly({
    req(data_reactive())
    gg <- ggplot(data_reactive(), aes(x = time, y = rh)) +
      geom_line(color = "blue") +
      geom_point(color = "darkblue") +
      scale_y_continuous(limits = c(50, 100)) +
      labs(x = "Time", y = "Relative Humidity (%)") +
      theme_minimal()
    ggplotly(gg) %>% layout(showlegend = FALSE)
  })
  
  # Render rainfall plot
  output$rain_plot <- renderPlotly({
    req(data_reactive())
    gg <- ggplot(data_reactive(), aes(x = time, y = rain)) +
      geom_line(color = "green") +
      geom_point(color = "darkgreen") +
      scale_y_continuous(limits = c(0, 10)) +
      labs(x = "Time", y = "Rainfall (mm)") +
      theme_minimal()
    ggplotly(gg) %>% layout(showlegend = FALSE)
  })
  
  # Render data table
  output$data_table <- renderDataTable({
    req(data_reactive())
    data_reactive() %>%
      mutate(
        time = format(time, "%Y-%m-%d %H:%M"),
        tc = round(tc, 1),
        rh = round(rh, 1),
        rain = round(rain, 2)
      ) %>%
      datatable(
        options = list(pageLength = 24),
        colnames = c("Time", "Temperature (°C)", 
                     "Humidity (%)", "Rainfall (mm)", 
                     "Condition")
      )
  })
  
  # Handle CSV download
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("weather_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(data_reactive(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)