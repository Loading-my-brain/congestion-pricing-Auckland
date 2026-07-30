library(sf)
library(dplyr)

# 1. Calculate the spatial center (centroid) of each neighborhood
sa2_centroids <- travel_props_density %>%
  st_centroid() %>%
  # Transform coordinates to WGS84 (EPSG 4326) for routing engines
  st_transform(crs = 4326) %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  )

# 2. Define the coordinates of Auckland CBD (e.g., Britomart/Queen St intersection)
cbd_lat <- -36.848461
cbd_lon <- 174.763336













# Install if you don't have it:
# install.packages("osrm")
library(osrm)

sa2_centroids_10 <-sa2_centroids[1:10,]

origins <- sa2_centroids_10 %>% 
  st_drop_geometry() %>% 
  select(lon, lat) %>% 
  as.matrix()



destination <- matrix(c(cbd_lon, cbd_lat), ncol = 2)

# Query the OSRM server to calculate durations (in min) for all trips
travel_time_query <- osrmTable(
  src = origins, 
  dst = destination, 
  measure = "duration"
)

travel_time_query$durations[, 1]

# View a summary of the travel times
summary(travel_time_query$durations[, 1])










