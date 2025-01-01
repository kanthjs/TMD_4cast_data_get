### Download and process weather forecast data from the Thai Meteorological Department (TMD) for domains 1 and 2

library(tidyverse)  # Load tidyverse library for data manipulation and visualization
library(lubridate)  # Load lubridate library for working with dates and times

## --- Set the date to yesterday ---
Date <- Sys.Date() - 1
Date <- Date |> as.Date(format = "%Y-%m-%d")

# Format the date to "YYYYMMDD" format for use in file names
today <- paste0(format.Date(Date, "%Y"),
                format.Date(Date, "%m"),
                format.Date(Date, "%d"))

# --- Function to download and process weather data ---
get_tmd_weather <- function(domain, vars) {
  # Base URL for the weather data
  main.domain <- "https://hpc.tmd.go.th/static/csv/"
  
  # Set the date to yesterday
  Date <- Sys.Date() - 1
  today <- format(Date, "%Y%m%d")  # Format the date to "YYYYMMDD"
  when <- "00"  # Time of the forecast run, "00" stands for 00 UTC
  
  # Construct URLs for the specified variables and domain
  urls <- paste0(main.domain,
                 today,
                 when,
                 "/",
                 vars,
                 ".",
                 domain,
                 ".",
                 today,
                 when,
                 ".csv")
  
  # Read CSV files from the constructed URLs and store them in a list
  data_list <- map(urls, read_csv)
  names(data_list) <- vars  # Name each element in the list with the variable name
  
  # Combine the list of data frames into a single data frame with an additional 'var' column
  weather_data <- list_rbind(data_list, names_to = "var")
  return(weather_data)  # Return the combined data frame
}

# --- Download Domain 1 data ---
d01.vars <- c("t2m", "rhum", "p3h")  # Variables for domain 1: temperature, relative humidity, and 3-hour precipitation
weather_d01 <- get_tmd_weather(domain = "d01", vars = d01.vars)  # Download and process the data
saveRDS(weather_d01, file = paste0("weather_forecast_d01_", today, ".rds"))  # Save the processed data to an RDS file

# --- Download Domain 2 data ---
d02.vars <- c("t2m", "rhum", "p1h")  # Variables for domain 2: temperature, relative humidity, and 1-hour precipitation
weather_d02 <- get_tmd_weather(domain = "d02", vars = d02.vars)  # Download and process the data
saveRDS(weather_d02, file = paste0("weather_forecast_d02_", today, ".rds"))  # Save the processed data to an RDS file

# End of script