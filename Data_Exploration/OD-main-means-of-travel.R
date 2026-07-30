# Library Packages 
library(foreign)


# experience: 
# I really confused myself with the data; I was overloooking the main means of travel
# sheet data in STATSnz. SA2 individual part 2 data and the main 
# menas of travel to work were different, but I was treating them the same; one has the orgin and desintation data, the other  has
# origin data. Which made this very diffcult to find the data.

# Read the database file
od_table <- read.dbf("./data/OD-data/2023-census-main-means-of-travel-to-work-by-statistical-area.dbf")
glimpse(od_table)# Inspect 
# Some Notes: ____________________________________
# SA22023_V1 & SA22023__1  These represent the Origin. 
#SA22023__3 & SA22023__4These represent the Destination 


# Get the list of 503 Auckland neighborhood names from the most recent data set
auckland_sa2s <- unique(travel_income_HO_emp$usual_residence_statistical_area_2_name_na)

# Crop the national dataset down to Auckland only trips
auckland_od_clean <- od_table %>%
  filter(
    SA22023__1 %in% auckland_sa2s, # Keep only if Origin is in Auckland
    SA22023__4 %in% auckland_sa2s  # Keep only if Destination is in Auckland
  ) %>%
  
  # Convert factor columns to numeric and replace -999 suppressions with NA
  mutate(across(
    starts_with("X2023_"), 
    ~ {
      num_vals <- as.numeric(as.character(.x))
      ifelse(num_vals < 0, NA, num_vals)
    }
  )) %>%
  
  # Rename columns to make them readable
  rename(
    origin_code = SA22023_V1,
    origin_name = SA22023__1,
    dest_code = SA22023__3,
    dest_name = SA22023__4,
    
    # 2023 Commuter counts
    od_work_at_home = X2023_Work_,
    od_drive_private = X2023_Drive,
    od_drive_company = X2023_Dri_1,
    od_passenger = X2023_Passe,
    od_public_bus = X2023_Publi,
    od_train = X2023_Train,
    od_bicycle = X2023_Bicyc,
    od_walk_jog = X2023_Walk_,
    od_ferry = X2023_Ferry,
    od_other = X2023_Other,
    od_total_commuters = X2023_Total
  ) %>%
  
  # Keep only the cleaned 2023 columns
  select(
    origin_code, origin_name,
    dest_code, dest_name,
    starts_with("od_")
  ) %>%
  
  # Filter out empty routes (where no one commuted in 2023)
  filter(od_total_commuters > 0)

glimpse(auckland_od_clean)# sanity checz


# ADD SPATAIL DATA TO OD DATA----------------------------------------------------
# EXTRACT SA2 CENTROIDS 
# We use NZTM projection (EPSG:2193) to calculate accurate physical distances in m
sa2_centroids <- travel_income_HO_emp %>%
  st_centroid() %>%
  st_transform(2193) %>% 
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(usual_residence_statistical_area_2_code_na, lon, lat)

glimpse(sa2_centroids) # sanity Check

# MATCH COORDINATES TO THE CLEANED OD TABLE
od_with_coords <- auckland_od_clean %>%
  # Match starting coordinates (Origins)
  left_join(sa2_centroids, by = c("origin_code" = "usual_residence_statistical_area_2_code_na")) %>%
  rename(orig_lon = lon, orig_lat = lat) %>%
  
  # Match ending coordinates (Destinations)
  left_join(sa2_centroids, by = c("dest_code" = "usual_residence_statistical_area_2_code_na")) %>%
  rename(dest_lon = lon, dest_lat = lat) %>%
  
  # Drop any rows where we don't have matching spatial coordinates
  filter(!is.na(orig_lon), !is.na(dest_lon))


#  CONVERT COORDINATES TO SPATIAL LINESTRINGS ---- Hinami will revisit this later as she didn't write this function  herself
# We write a fast function to draw straight lines connecting each origin and destination
create_desire_lines <- function(df) { 
  lines <- vector("list", nrow(df))
  for (i in 1:nrow(df)) {
    lines[[i]] <- st_linestring(
      matrix(c(df$orig_lon[i], df$dest_lon[i], 
               df$orig_lat[i], df$dest_lat[i]), 
             ncol = 2)
    )
  }
  return(st_sfc(lines, crs = 2193))
}

# Apply the function to build the spatial sf object
auckland_od_spatial <- od_with_coords %>%
  mutate(geom = create_desire_lines(.)) %>%
  st_as_sf(sf_column_name = "geom")

#Sanity Check 
glimpse(auckland_od_spatial)




#______________________________________________________________________________
# Now I want to find the od vehicle trips to Auckland cbd
#______________________________________________________________________________

# I've created a city cneter cordon boundary based on FIGURE 36: CITY CENTRE CORDON MAP on 
# file:///C:/Users/hinam/OneDrive%20-%20The%20University%20of%20Auckland/Documents/Univeristy%20Of%20Auckland/2026,%20Semester%201/GEOG%20701/RP%20-%20Research%20Articles/Policy/TheCongestionQuestionsTechnicalReport.pdf


# HINAMI: I need to find the actual CBD cordon polygon. I currenlty, just have
# Defined the CBD cordon myself, but this is not accurate!
# Create the WGS84 Polygon 
cbd_poly_wgs84 <- st_polygon(list(matrix(c(
  174.75007370616186, -36.83552540368747,  
  174.78457764212618, -36.84225725970129,  
  174.77839783269974, -36.863341993162315, 
  174.74844292311874, -36.860595126139465, 
  174.75007370616186, -36.83552540368747   
), ncol = 2, byrow = TRUE))) %>%
  st_sfc(crs = 4326) %>% # Set initial CRS to WGS84 
  st_sf()

# Project the polygon to NZTM (EPSG:2193) to match my dataset
cbd_poly_nztm <- st_transform(cbd_poly_wgs84, 2193)


# Sanity Check - did I properly captured the right coords? Yes I did 
tmap_mode("view")
tm_shape(travel_income_HO_emp) +
  tm_polygons(col = "lightblue", border.col = "white", alpha = 0.5) +
  tm_shape(cbd_poly_nztm) +
  tm_polygons(col = "red", alpha = 0.5, border.col = "black")





#Convert Destination coordinates of my flows into spatial points (NZTM)
dest_sf <- st_as_sf(st_drop_geometry(auckland_od_spatial),
                    coords = c("dest_lon", "dest_lat"), 
                    crs = 2193)

# Convert Origin coordinates of my flows into spatial points (NZTM)
orig_sf <- st_as_sf(st_drop_geometry(auckland_od_spatial), 
                    coords = c("orig_lon", "orig_lat"), 
                    crs = 2193)

# est which destinations and origins lie inside my CBD box
auckland_od_spatial$dest_inside_cbd <- st_intersects(dest_sf, cbd_poly_nztm, sparse = FALSE)[, 1]
auckland_od_spatial$orig_inside_cbd <- st_intersects(orig_sf, cbd_poly_nztm, sparse = FALSE)[, 1]

#Filter for Inbound Commutes (Destination is INSIDE, Origin is OUTSIDE)
cbd_inbound_flows <- auckland_od_spatial %>%
  filter(
    dest_inside_cbd == TRUE, 
    orig_inside_cbd == FALSE
  ) %>%
  mutate(
    # Sum up private and company car drivers
    od_private_vehicles = od_drive_private + od_drive_company
  ) %>%
  filter(od_private_vehicles > 0)








# Calculate the total sum of vehicles across all inbound routes
total_entering_cars <- sum(cbd_inbound_flows$od_private_vehicles, na.rm = TRUE)
print(paste("Total daily private vehicles entering the custom CBD box:", total_entering_cars))








# Filter out small flows to keep the map fast (e.g., routes with 10+ daily cars)
major_inbound_flows <- cbd_inbound_flows %>%
  filter(od_private_vehicles >= 10)

tmap_mode("view")
# Base Layer: All Auckland SA2s
tm_shape(sa2_clipped_clean) +
  tm_polygons(col = "lightgrey", border.col = "white", alpha = 0.3, popup.vars = FALSE) +
  
  # Custom Bounding Box Layer (Yellow transparent polygon)
  tm_shape(cbd_poly_nztm) +
  tm_polygons(col = "yellow", alpha = 0.3, border.col = "black", border.lwd = 2, title = "Custom CBD Box") +
  
  # Inbound Flow Lines Layer (Sized by vehicle count)
  tm_shape(major_inbound_flows) +
  tm_lines(
    col = "od_private_vehicles",
    lwd = "od_private_vehicles",
    scale = 6,
    palette = "Reds",
    title.col = "Entering Vehicles",
    id = "origin_name",
    popup.vars = c(
      "From" = "origin_name",
      "To" = "dest_name",
      "Vehicles" = "od_private_vehicles"
    )
  ) +
  tm_view(
    basemaps = c("Esri.WorldGrayCanvas", "OpenStreetMap")
  )







#Group by origin and calculate total vehicle trips entering your custom CBD box
cbd_inbound_table <- cbd_inbound_flows %>%
  st_drop_geometry() %>% # Drop spatial geometries for a clean, fast data table
  group_by(origin_name, origin_code) %>%
  summarise(
    total_daily_vehicles = sum(od_private_vehicles, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Sort in descending order (highest vehicle volume first)
  arrange(desc(total_daily_vehicles))

# Print the top 15 origin suburbs driving into your CBD box
print(head(cbd_inbound_table, 15))














