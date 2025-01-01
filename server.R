# Server Definition
server <- function(input, output, session) {
  
  # Map interactions
  observe({
    leafletProxy("map") %>% 
      clearMarkers() %>%
      addMarkers(lng = input$longitude, lat = input$latitude)
  })
  
  observeEvent(input$map_click, {
    updateNumericInput(session, "longitude", value = input$map_click$lng)
    updateNumericInput(session, "latitude", value = input$map_click$lat)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(lng = input$longitude, lat = input$latitude, zoom = 6) %>%
      addMarkers(lng = input$longitude, lat = input$latitude)
  })
  
  # Data fetching
  data_reactive <- eventReactive(input$run, {
    req(input$longitude, input$latitude)
    
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
    req(data_reactive())
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
    req(data_reactive())
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
    req(data_reactive())
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
    req(data_reactive())
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
      paste0("weather_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(data_reactive(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
