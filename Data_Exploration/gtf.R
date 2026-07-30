
# Load data


# Load GTFS files dynamically from the data folder
gtf_path <- "data/gtf"
files <- list.files(gtf_path, pattern = "\\.txt$", full.names = TRUE)
varnames <- tools::file_path_sans_ext(basename(files))

for (i in seq_along(files)) {
  assign(varnames[i], read.csv(files[i]))
}


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


























stops_projected <- st_transform(stops_frequency, st_crs(travel_props_density))

# patially join stops to the SA2 neighborhoods they fall inside
stops_in_sa2 <- st_join(stops_projected, travel_props_density, join = st_intersects)

# Aggregate the transit supply metrics at the neighborhood level
sa2_transit_metrics <- stops_in_sa2 %>%
  st_drop_geometry() %>%
  # Group by your unique SA2 code column
  group_by(usual_residence_statistical_area_2_code_na) %>% 
  summarise(
    total_bus_stops = sum(is_bus == TRUE, na.rm = TRUE),
    total_train_ferry_stations = sum(is_train_ferry == TRUE, na.rm = TRUE),
    total_daily_services = sum(daily_services, na.rm = TRUE)
  )

#Join these transit metrics back to your master travel_props_density dataset
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
























# Run a Pearson correlation
cor_transit <- cor(
  travel_props_with_transit$transit_service_density, 
  travel_props_with_transit$prop_aggregate_public_transport, 
  use = "complete.obs"
)

print(paste("Correlation Coefficient (r):", round(cor_transit, 3)))


