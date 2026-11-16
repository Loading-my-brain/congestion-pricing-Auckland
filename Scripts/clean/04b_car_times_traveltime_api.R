# =============================================================================
# 04b_car_times_traveltime_api.R
# -----------------------------------------------------------------------------
# STATUS   NOT USED IN THE THESIS (Oct 2026): the TravelTime licence for
#          publication could not be confirmed, so car times now come from
#          r5r free-flow routing (04c). Kept only as a record.
#
# PURPOSE  Morning-peak CAR travel time from each SA2 to the CBD.
#          ALREADY DONE - the CSV exists, so this script does nothing unless
#          the CSV is deleted. Kept as a record of the method.
#
# SOURCE   TravelTime API (https://traveltime.com), "time-filter" endpoint.
#          Driving times use predicted historical traffic for a typical
#          weekday at the given time of day (the date itself does not matter).
#          Licence/attribution: emailed TravelTime - keep their reply on file.
#
# MATCHES THE PT RUN (04a)
#   Origins      SA2 centroids (497 SA2s)
#   Destination  centroid of City Centre SA3 (51150)
#   Departure    8:00 am, Wednesday 13 May 2026; mode = driving only
#
# CREDENTIALS  in ~/.Renviron (never in the script or thesis):
#              TRAVELTIME_APP_ID=...   TRAVELTIME_API_KEY=...
#
# OUTPUT   data/car_time_cbd_20260513_0800.csv
#            usual_residence_statistical_area_2_code_na, car_time_new (minutes)
#
# NEXT     04c_car_freeflow_check.R (optional) or 05_travel_times.R
# =============================================================================

library(sf)
library(tidyverse)
library(httr)
library(jsonlite)

car_csv <- "data/car_time_cbd_20260513_0800.csv"

if (!file.exists(car_csv)) {

  # Credentials 
  readRenviron("~/.Renviron")
  app_id  <- Sys.getenv("TRAVELTIME_APP_ID")
  api_key <- Sys.getenv("TRAVELTIME_API_KEY")
  stopifnot(nzchar(app_id), nzchar(api_key))

  # Origins: SA2 centroids 
  travel_props <- st_read("./data/Saved/travel_props.gpkg", quiet = TRUE)
  xy <- travel_props %>% st_transform(2193) %>% st_geometry() %>%
    st_centroid() %>% st_transform(4326) %>% st_coordinates()
  origins_df <- travel_props %>% st_drop_geometry() %>%
    transmute(id = as.character(usual_residence_statistical_area_2_code_na),
              lng = xy[, 1], lat = xy[, 2])

  # Destination: City Centre SA3 centroid 
  cbd_xy <- st_read("data/boundary/sa3/statistical-area-3-2023-clipped-generalised.shp", quiet = TRUE) %>%
    filter(SA32023_V1 == "51150") %>%
    st_transform(2193) %>% st_geometry() %>% st_centroid() %>%
    st_transform(4326) %>% st_coordinates()

  locations_list <- c(
    lapply(seq_len(nrow(origins_df)), function(i)
      list(id = origins_df$id[i], coords = list(lat = origins_df$lat[i], lng = origins_df$lng[i]))),
    list(list(id = "auckland_cbd", coords = list(lat = cbd_xy[1, 2], lng = cbd_xy[1, 1]))))

  #   Departure time (ISO format with +12:00)
  departure_time <- as.POSIXct("2026-05-13 08:00:00", tz = "Pacific/Auckland")
  dep_iso <- sub("(\\d{2})(\\d{2})$", "\\1:\\2", format(departure_time, "%Y-%m-%dT%H:%M:%S%z"))

  make_search <- function(sa2_id) {
    list(id = sa2_id, departure_location_id = sa2_id,
         arrival_location_ids = list("auckland_cbd"),
         departure_time = dep_iso,
         transportation = list(type = "driving"),
         travel_time = 10800,                       # search limit: 3 hours
         properties = list("travel_time"))
  }

  #  Send in batches of 10 (API limit); wait 65 s if rate-limited (HTTP 429)  
  batches <- split(origins_df$id, ceiling(seq_along(origins_df$id) / 10))
  all_results <- list()
  b <- 1
  while (b <= length(batches)) {
    res <- POST("https://api.traveltimeapp.com/v4/time-filter",
                add_headers("X-Application-Id" = app_id, "X-Api-Key" = api_key,
                            "Content-Type" = "application/json"),
                body = toJSON(list(locations = locations_list,
                                   departure_searches = lapply(batches[[b]], make_search)),
                              auto_unbox = TRUE),
                encode = "json")
    res_data <- content(res, as = "parsed", simplifyVector = FALSE)
    if (status_code(res) == 429) { message("Rate limit - waiting 65 s"); Sys.sleep(65); next }
    if (status_code(res) != 200) { str(res_data, max.level = 3); stop("Batch ", b, " failed") }
    all_results <- c(all_results, res_data$results)
    message("Batch ", b, "/", length(batches), " done")
    b <- b + 1
    Sys.sleep(3)
  }

  #  Extract times (seconds -> minutes) and save 
  get_tt <- function(x) {
    p <- x$properties
    if (!is.null(p$travel_time)) return(p$travel_time)
    if (length(p) > 0 && !is.null(p[[1]]$travel_time)) return(p[[1]]$travel_time)
    NA_real_
  }
  car_times <- data.frame(
    usual_residence_statistical_area_2_code_na =
      vapply(all_results, function(r) r$search_id, character(1)),
    car_time_new = vapply(all_results, function(r)
      if (length(r$locations) == 0) NA_real_ else round(get_tt(r$locations[[1]]) / 60, 1),
      numeric(1)))

  write.csv(car_times, car_csv, row.names = FALSE)
}

# Check
car_times <- read.csv(car_csv, colClasses = c("character", "numeric"))
nrow(car_times)                       # 497
sum(is.na(car_times$car_time_new))    # 0
summary(car_times$car_time_new)       # median 29.4, IQR 21.2-37.0, range 2.1-62.0
