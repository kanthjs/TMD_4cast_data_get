library(shiny)
library(leaflet)
library(ggplot2)
library(DT)
library(nasapower)

# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("Plant Disease Warning Systems"),
  
  # Sidebar layout with input controls
  sidebarLayout(
    sidebarPanel(
      # Input for longitude
      numericInput("longitude", "Longitude:", value = 100.10),
      
      # Input for latitude
      numericInput("latitude", "Latitude:", value = 13.10),
      
      # Date picker for plant emergence date
      dateInput("selected_date", "Selected Date:", value = "2023-02-01"),
      
      # Button to fetch data and execute
      actionButton("run", "Get data and run")
    ),
    mainPanel(
      # Output map display
      leafletOutput("map"),
      
      # Tabs to display data and summary
      tabsetPanel(
        tabPanel("Data", dataTableOutput("data_table")),
        tabPanel("Summary Report")
      ),
      plotOutput("tc_plot")
      plotOutput("hr_plot")
      plotOutput("rain_plot")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Observe changes in longitude and latitude inputs and update the map marker
  observe({
    leafletProxy("map") %>% clearMarkers() %>%
      addMarkers(lng = input$longitude, lat = input$latitude)
  })
  
  # Enable picking longitude and latitude by clicking on the map
  observeEvent(input$map_click, {
    updateNumericInput(session, "longitude", value = input$map_click$lng)
    updateNumericInput(session, "latitude", value = input$map_click$lat)
  })
  
  # Render the base map with the initial view set to the input longitude and latitude
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      setView(lng = input$longitude, lat = input$latitude, zoom = 6)
  })
  
  # Reactive function to fetch and process weather data based on user inputs
  data_reactive <- eventReactive(input$run, {
    # Ensure all necessary inputs are provided
    req(input$longitude, input$latitude)
    
    # Fetch weather data from TMD Forecast API
    base.url <- "https://data.tmd.go.th/nwpapi/v1/forecast/location/hourly/at"
    json <- request(base.url) |>
      req_auth_bearer_token(Sys.getenv("tmd_token")) |>
      req_url_query(
        lat = input$latitude,
        lon = input$longitude,
        fields = 'cond,tc,rh,rain',
        date = "2025-01-01",
        hour = 0,
        duration = 24,
        .multi = "explode"
      ) |>
      #    req_throttle(rate = 10 / 60) |>
      req_perform() |>
      resp_body_json()
    
    weather_data <- json |> pluck(1, 1) |> pluck("forecasts") |> map_dfr(\(x) {
      tibble(
        time = x |> pluck("time"),
        tc = x |> pluck("data", "tc"),
        rh = x |> pluck("data", "rh"),
        rain = x |> pluck("data", "rain"),
        cond = x |> pluck("data", "cond")
      )
    })
   weather_data
  })
  
  # Render the data table in the "Data" tab
  output$data_table <- renderDataTable({
    datatable(data_reactive(), options = list(pageLength = 6))
  })
  
  # Render the plot for Daily Severity Values
  output$tc_plot <- renderPlot({
    req(data_reactive())
    ggplot(data_reactive(), aes(x = ymd_hms(time), y = tc)) +
      geom_line(color = "red") + # Line plot for cumulative DSV
      geom_point(color = "darkred") + # Points for individual data values
      labs(
        title = "Temperature", 
        x = "Time", 
        y = "Temperature (Celsius)"
      ) +
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$hr_plot <- renderPlot({
    req(data_reactive())
    ggplot(data_reactive(), aes(x = ymd_hms(time), y = rh)) +
      geom_line(color = "red") + # Line plot for cumulative DSV
      geom_point(color = "darkred") + # Points for individual data values
      labs(
        title = "Temperature", 
        x = "Time", 
        y = "Humidity"
      ) +
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5))
    
    output$rain_plot <- renderPlot({
      req(data_reactive())
      ggplot(data_reactive(), aes(x = ymd_hms(time), y = rain)) +
        geom_line(color = "red") + # Line plot for cumulative DSV
        geom_point(color = "darkred") + # Points for individual data values
        labs(
          title = "Temperature", 
          x = "Time", 
          y = "precipitation"
        ) +
        theme_minimal() + 
        theme(plot.title = element_text(hjust = 0.5))
}

# Run the Shiny app
shinyApp(ui, server)
