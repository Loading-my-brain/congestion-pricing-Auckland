# Library Package
library(sp)
library(sf)
library(GWmodel)
library(dplyr) 
library(tidyverse)
library(tmap)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(terra)
library(janitor)
library(stringr)
library(r5r)

# Load data
# Load GTFS files dynamically from the data folder
gtf_path <- "data/gtf"
files <- list.files(gtf_path, pattern = "\\.txt$", full.names = TRUE)
varnames <- tools::file_path_sans_ext(basename(files))

for (i in seq_along(files)) {
  assign(varnames[i], read.csv(files[i]))
}

for (i in seq_along(files)) {
  cat("\n--- Glimpse of:", varnames[i], "---\n")
  glimpse(get(varnames[i]))
}

#The Major Urban Area in Auckland - boundary we're using to filter the gtf data
boundary = st_read("./data/boundary/urban-rural-2023-clipped-generalised/urban-rural-2023-clipped-generalised.shp")
akl_boundary = boundary %>%# Filter to Auckland
  filter(IUR2023__1  == "Major urban area")


# Stops
# Ensure the stops spatial object is projected to EPSG:2193
stops_sf_nztm <- stops %>%
  rename(lon = stop_lon, lat = stop_lat) %>%
  select(stop_id, stop_name, lon, lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(2193)


# Build & save the interactive map
tmap_mode("view")
(interactive_study_map <- 
    tm_basemap(c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery")) +
    tm_shape(stops_sf_nztm) +
    tm_dots(fill = "teal", size = 0.1, alpha = 0.5) +
    tm_layout(
      main.title = "Study Area: Auckland SA2s & Transit Infrastructure"
    ))

# tmap_save(
#   tm = interactive_study_map, 
#   filename = "./All_figures/Auckland_Interactive_Study_Area.html"
# )


# Service Frequency Filtering
#Isolate daily weekday scheduled runs from the static GTFS feed.


# Classify each stop ID based on the transport modes serving it,
# using standard GTFS definitions (Route Type 3 is Bus, and 2/4 are Train/Ferry).
# This is necessary because buses and trains have different reasonable walking catchment limits.
stop_modes <- stop_times %>%
  left_join(trips, by = "trip_id") %>%
  left_join(routes, by = "route_id") %>%
  group_by(stop_id) %>%
  summarise(
    is_bus = any(route_type == 3, na.rm = TRUE),
    is_train_ferry = any(route_type %in% c(2, 4), na.rm = TRUE)
  )

# Join our mode classifications back to our projected stops spatial object.
stops_classified <- stops_sf_nztm %>%
  left_join(stop_modes, by = "stop_id")

# Isolate bus stops and rapid rail/ferry stations into separate layers
# so we can apply different walking thresholds to each mode.
bus_stops <- stops_classified %>% filter(is_bus == TRUE)
train_ferry <- stops_classified %>% filter(is_train_ferry == TRUE)


# Extract only service IDs that operate consistently on weekdays (Mon-Fri),
# which represents a typical commuter's travel schedule.
typical_weekdays <- calendar %>%
  filter(monday == 1, 
         tuesday == 1, 
         wednesday == 1, 
         thursday == 1, 
         friday == 1) %>%
  pull(service_id)

# Filter our trips table, retaining only transit runs 
# that are scheduled to operate on our identified typical weekdays.
weekday_trips <- trips %>%
  filter(service_id %in% typical_weekdays)

# Filter the massive stop_times table, keeping only stop arrivals 
# linked to our weekday trips to avoid schedule inflation from weekend or holiday schedules.
weekday_stop_times <- stop_times %>%
  filter(trip_id %in% weekday_trips$trip_id)

# Count the total number of scheduled weekday arrivals per stop,
# which serves as our proxy for transit service frequency and intensity.
stop_frequency <- weekday_stop_times %>%
  group_by(stop_id) %>%
  summarise(daily_services = n())

# Join the daily service frequencies back to our spatial stops dataset,
# replacing any unserved stops (NAs) with zero.
stops_frequency <- stops_classified %>%
  left_join(stop_frequency, by = "stop_id") %>%
  mutate(daily_services = replace_na(daily_services, 0))



# Data Inspection: 
range(ymd(calendar$end_date)) # "2026-05-08" "2026-07-31" #2 months span























stops_projected <- st_transform(stops_frequency, st_crs(travel_props_density))

# patially join stops to the SA2 neighborhoods they fall inside
stops_in_sa2 <- st_join(stops_projected, travel_props_density, join = st_intersects)

# Aggregate the transit supply mcd etrics at the neighborhood level
sa2_transit_metrics <- stops_in_sa2 %>%
  st_drop_geometry() %>%
  # Group by my unique SA2 code column
  group_by(usual_residence_statistical_area_2_code_na) %>% 
  summarise(
    total_bus_stops = sum(is_bus == TRUE, na.rm = TRUE),
    total_train_ferry_stations = sum(is_train_ferry == TRUE, na.rm = TRUE),
    total_daily_services = sum(daily_services, na.rm = TRUE)
  )

#Join these transit metrics back to my master travel_props_density dataset
travel_props_with_transit <- travel_props_density %>%
  left_join(sa2_transit_metrics, by = "usual_residence_statistical_area_2_code_na") %>%
  mutate(
    # Replace NAs (neighborhoods with no transit stops) with 0
    total_bus_stops = replace_na(total_bus_stops, 0),
    total_train_ferry_stations = replace_na(total_train_ferry_stations, 0),
    total_daily_services = replace_na(total_daily_services, 0),
    
    # Calculate Transit Service Density (scheduled services per sq km of land area)
    transit_service_density = total_daily_services / LAND_AREA_
  )








options(java.parameters = "-Xmx8G")
library(r5r)

r5r_network <- build_network(
  data_path = "data/r5_network",
  overwrite = TRUE,
  verbose = TRUE
)















# Run a Pearson correlation
cor_transit <- cor(
  travel_props_with_transit$transit_service_density, 
  travel_props_with_transit$prop_aggregate_public_transport, 
  use = "complete.obs"
)

print(paste("Correlation Coefficient (r):", round(cor_transit, 3)))

























#_______________________________________________________________________________
# the r5r package
#_______________________________________________________________________________

Sys.setenv(JAVA_HOME = "C:/Program Files/Eclipse Adoptium/jdk-11.0.32.9-hotspot")
library(r5r)

#Documentation: I struggled to load this package. I had to download java sdk

# Build the transit and road network
r5r_core <- build_network(data_path = "data/r5_network", verbose = FALSE)
# I notice the warning message: 
#ℹ No raster .tif files found in data/r5_network. Using elevation = "'NONE'".
# Which means: It simply means I did not provide a digital elevation model (DEM) 
# .tif file, so R5 will calculate walking and cycling speeds assuming Auckland is 
# flat. This is completely normal and standard for urban transit models
#  So this is a limitation for me! I'm assuming AUckland is flat. Something to note 
# About in documentation file



#Load the Main data 
travel_income_HO_emp<- st_read("data/Saved/travel_income_HO_emp.gpkg")
#range(ymd(calendar$end_date)) # "2026-05-08" "2026-07-31" #2 months span
# I wanted to note that this is a limitation becuase car movements varys across differe
# seasons, months ... To make an assumption..... We can't make an inference about the 2023 resident population 
# based on this 2 months span in 2026, not even 2023. 


#Format my starting points (Origins) - REPROIECTED TO WGS84 (4326)
origins <- travel_income_HO_emp %>%
  st_centroid() %>%
  st_transform(4326) %>% # r5r requires WGS84 coordinates
  mutate(
    id = usual_residence_statistical_area_2_code_na,
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(id, lon, lat)

# Format my ending point (Destination: Auckland CBD Cordon Centroid)
cbd_centroid <- cbd_poly_nztm %>%
  st_centroid() %>%
  st_transform(4326)

destination <- data.frame(#Make this in the same format as `origins`
  id = "CBD",
  lon = st_coordinates(cbd_centroid)[, 1],
  lat = st_coordinates(cbd_centroid)[, 2]
)


# Set my departure date and time (Simulating AM Peak: 8:00 AM on a typical Tuesday)
library(lubridate)
departure_time <- as.POSIXct("2026-05-9 08:00:00", tz = "Pacific/Auckland")


# 6. Calculate Public Transport Travel Times (Walk + Transit + Walk)
transit_times <- travel_time_matrix(
  r5r_core = r5r_core,
  origins = origins,
  destinations = destination,
  mode = c("WALK", "TRANSIT"), # Walk to stop, ride bus/train/ferry, walk to CBD
  departure_datetime = departure_time,
  max_walk_time = 30,          # Max walking minutes to/from transit
  max_trip_duration = 120,     # Max total commute minutes
  verbose = FALSE
) %>%
  rename(transit_time_cbd_mins = travel_time_p50) %>% # p50 is median travel time
  select(from_id, transit_time_cbd_mins)





# 7. Calculate Car/Driving Travel Times
car_times <- travel_time_matrix(
  r5r_core = r5r_core,
  origins = origins,
  destinations = destination,
  mode = "CAR",
  departure_datetime = departure_time,
  max_trip_duration = 120,
  verbose = FALSE
) %>%
  rename(car_time_cbd_mins = travel_time_p50) %>%
  select(from_id, car_time_cbd_mins)


# 8. Join these new travel times back to my master dataset
travel_props_grouped <- travel_props_grouped %>%

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 # COde to analyse the data: 
library(sf)
library(dplyr)
library(r5r)
library(tidyr)

# Ensure my r5r core is running (using my compiled network)
# r5r_core <- build_network(data_path = "data/r5_network", verbose = FALSE)

# =========================================================================
# 1. PREPARE THE PT STOPS AS ORIGINS (WGS84)
# =========================================================================
# We must reproject my projected stops to WGS84 (4326) for the Java engine
pt_all_origins <- stops_frequency %>%
  st_transform(4326) %>%
  mutate(
    id = as.character(stop_id),
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(id, lon, lat)

# Extract my Custom CBD Cordon Centroid in WGS84
cbd_centroid <- cbd_poly_nztm %>%
  st_centroid() %>%
  st_transform(4326)

destination <- data.frame(
  id = "CBD",
  lon = st_coordinates(cbd_centroid)[, 1],
  lat = st_coordinates(cbd_centroid)[, 2]
)

# Set weekday AM peak departure (8:00 AM on a typical Tuesday)
departure_time <- as.POSIXct("2026-08-11 08:00:00", tz = "Pacific/Auckland")


# =========================================================================
# 2. CALCULATE TRAVEL TIMES FOR ALL PT STOPS (Minutes)
# =========================================================================
pt_stop_times_to_cbd <- travel_time_matrix(
  r5r_core = r5r_core,
  origins = pt_all_origins,
  destinations = destination,
  mode = c("WALK", "TRANSIT"), # Walk to stop, take transit, walk from final stop to CBD center
  departure_datetime = departure_time,
  max_walk_time = 30,          # Max minutes allowed to walk to/from stops
  max_trip_duration = 120,     # Max total travel time
  verbose = FALSE
) %>%
  rename(stop_transit_time_cbd_mins = travel_time_p50) %>%
  select(from_id, stop_transit_time_cbd_mins)


# =========================================================================
# 3. CALCULATE TRUE NETWORK TRAVEL DISTANCES (Kilometers)
# =========================================================================
# We filter for high-frequency hubs or rapid rail/ferry stations to keep routing fast
major_pt_origins <- stops_frequency %>%
  filter(daily_services >= 100 | is_train_ferry == TRUE) %>%
  st_transform(4326) %>%
  mutate(
    id = as.character(stop_id),
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(id, lon, lat)

# detailed_itineraries calculates the exact spatial routes and segment lengths
pt_itineraries <- detailed_itineraries(
  r5r_core = r5r_core,
  origins = major_pt_origins,
  destinations = destination,
  mode = c("WALK", "TRANSIT"),
  departure_datetime = departure_time,
  max_walk_time = 30,
  max_trip_duration = 120,
  shortest_path = TRUE, # Keeps only the absolute fastest/shortest route option
  verbose = FALSE
)

# Aggregate the segment distances to get the total commute distance (km)
pt_stop_distances_to_cbd <- pt_itineraries %>%
  st_drop_geometry() %>%
  group_by(from_id) %>%
  summarise(
    # Sum up the length of all walking and transit legs, converting meters to km
    stop_transit_distance_cbd_km = sum(distance, na.rm = TRUE) / 1000
  )


# =========================================================================
# 4. JOIN RESULTS BACK TO my STOPS FREQUENCY DATASET
# =========================================================================
stops_frequency_analyzed <- stops_frequency %>%
  mutate(stop_id_char = as.character(stop_id)) %>%
  left_join(pt_stop_times_to_cbd, by = c("stop_id_char" = "from_id")) %>%
  left_join(pt_stop_distances_to_cbd, by = c("stop_id_char" = "from_id")) %>%
  select(-stop_id_char)

# View a summary of my mapped stops with travel times and distances
summary(stops_frequency_analyzed$stop_transit_time_cbd_mins)
summary(stops_frequency_analyzed$stop_transit_distance_cbd_km)
































# Allocate 4GB of RAM to Java (MUST be run before library(r5r))
options(java.parameters = "-Xmx4G") 

library(r5r)
library(sf)
library(dplyr)

# Save it as 'r5r_network' to match the modern syntax
r5_path <- "data/r5_network"
r5r_network <- build_network(data_path = r5_path, verbose = FALSE)

# Sanity Check: Ensure this no longer prints NULL!
r5r_network












