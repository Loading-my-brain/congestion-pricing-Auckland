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
#SA22023__3 & SA22023__4 These represent the Destination 


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
    od_total_stated = X2023_Total
  ) %>%
  
  # Keep only the cleaned 2023 columns
  select(
    origin_code, origin_name,
    dest_code, dest_name,
    starts_with("od_")
  ) %>%
  
  # Filter out empty routes (where no one commuted in 2023) - Hinayana, we might not even need this? 
  filter(od_total_stated > 0)

glimpse(auckland_od_clean)# sanity check
summary(auckland_od_clean)










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
  select(usual_residence_statistical_area_2_code_na, usual_residence_statistical_area_2_name_na, lon, lat)
centroids_sf <- st_as_sf(sa2_centroids, coords = c("lon", "lat"), crs = 2193)

glimpse(sa2_centroids) # sanity Checks
# plot(vect(st_as_sf(sa2_centroids)))

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
plot(vect(auckland_od_spatial))# This is essentially the trip from point A to point B in Urban Auckland 
# I want to filter these to identify the number of trips made from point A to B  from the centroids in the Urban Auckladn to Auckland CBD.
#So the number of trips from the centroids to Auckland CBD boundary. 





#SAnity check 









#______________________________________________________________________________
# Now I want to find the od vehicle trips to Auckland cbd
#______________________________________________________________________________

# I've created a city cneter cordon boundary based on FIGURE 36: CITY CENTRE CORDON MAP on 
# file:///C:/Users/hinam/OneDrive%20-%20The%20University%20of%20Auckland/Documents/Univeristy%20Of%20Auckland/2026,%20Semester%201/GEOG%20701/RP%20-%20Research%20Articles/Policy/TheCongestionQuestionsTechnicalReport.pdf


# RESOLVED: I need to find the actual CBD cordon polygon. I currenlty, just have
# Defined the CBD cordon myself, but this is not accurate!
# Create the WGS84 Polygon 
# cbd_poly_wgs84 <- st_polygon(list(matrix(c(
#   174.75007370616186, -36.83552540368747,  
#   174.78457764212618, -36.84225725970129,  
#   174.77839783269974, -36.863341993162315, 
#   174.74844292311874, -36.860595126139465, 
#   174.75007370616186, -36.83552540368747   
# ), ncol = 2, byrow = TRUE))) %>%
#   st_sfc(crs = 4326) %>% # Set initial CRS to WGS84 
#   st_sf()

# Project the polygon to NZTM (EPSG:2193) to match my dataset
# cbd_poly_nztm <- st_transform(cbd_poly_wgs84, 2193)



# I have found the cbd boundary: 
sa3 <- st_read("data/boundary/sa3/statistical-area-3-2023-clipped-generalised.shp")
cbd <- sa3 %>% 
  filter(SA32023_V1 == "51150") #Filter to Auckland City Center

# Ensure this is in NZTM (EPSG:2193) to match my dataset
cbd_poly_nztm <- st_transform(cbd, 2193)



# Identify the number of trips from outside to inside CBD boundary 
# 1. Identify which SA2 codes belong to the CBD
# We use the centroids you already created
cbd_sa2_codes <- sa2_centroids %>%
  st_as_sf(coords = c("lon", "lat"), crs = 2193) %>%
  st_filter(cbd_poly_nztm) %>%
  select(c(usual_residence_statistical_area_2_code_na, usual_residence_statistical_area_2_name_na))

print(cbd_sa2_codes)

#Sanity Check
tmap_mode("view")
  # Custom Bounding Box Layer (Yellow transparent polygon)
  tm_shape(cbd_poly_nztm) +
    tm_polygons(
      fill = "yellow", 
      fill_alpha = 0.3, 
      col = "black", 
      lwd = 2, 
      fill.legend = tm_legend(title = "Custom CBD Box")
    ) +
  tm_shape(cbd_sa2_codes) +
    tm_polygons(
      fill = "red", 
      fill_alpha = 0.3, 
      col = "white", 
      popup.vars = TRUE
    ) 

# Sanity Check: How many SA2s are considered 'CBD'?
length(cbd_sa2_codes$usual_residence_statistical_area_2_code_na)


















# Filter the dataset; We want trips where the Origin is NOT in the list, but the Destination IS in the list.
inbound_to_cbd <- auckland_od_clean %>%
  filter(
    !(origin_code %in% cbd_codes_list), # Origin is OUTSIDE CBD
    dest_code %in% cbd_codes_list       # Destination is INSIDE CBD
  )






# Create a version of the data where NAs are replaced with 3
auckland_od_imputed <- auckland_od_clean %>%
  mutate(across(starts_with("od_"), ~ ifelse(is.na(.), 3, .)))

# Now run your summary on the imputed data
inbound_summary_imputed <- auckland_od_imputed %>%
  filter(!(origin_code %in% cbd_codes_list), dest_code %in% cbd_codes_list) %>%
  summarise(
    total_commuters = sum(od_total_stated), 
    private_vehicle = sum(od_drive_private + od_drive_company),
    public_transport = sum(od_public_bus + od_train + od_ferry),
    active_modes = sum(od_bicycle + od_walk_jog)
  )


