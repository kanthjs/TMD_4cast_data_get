# get hourly weather data from  specific location
library(httr2)
library(tidyverse)

  base.url <- "https://data.tmd.go.th/nwpapi/v1/forecast/location/hourly/at"
  json <- request(base.url) |>
    req_auth_bearer_token(Sys.getenv("tmd_token")) |>
    req_url_query(
      lat = 13.10,
      lon = 100.10,
      fields = 'cond,tc,rh,rain',
      date = "2025-01-01",
      hour = 0,
      duration = 24,
      .multi = "explode"
    ) |>
#    req_throttle(rate = 10 / 60) |>
    req_perform() |>
    resp_body_json()
  
  weather <- json |> pluck(1, 1) |> pluck("forecasts") |> map_dfr(\(x) {
    tibble(
      time = x |> pluck("time"),
      tc = x |> pluck("data", "tc"),
      rh = x |> pluck("data", "rh"),
      rain = x |> pluck("data", "rain"),
      cond = x |> pluck("data", "cond")
    )
  })
  