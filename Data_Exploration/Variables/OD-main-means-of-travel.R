# Library Packages 
library(foreign)
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

travel_income_HO_emp<- st_read("data/Saved/travel_income_HO_emp.gpkg")

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
  select(usual_residence_statistical_area_2_code_na, lon, lat, usual_residence_statistical_area_2_name_na)
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



# Sanity Check - did I properly captured the right coords? Yes I did
cbd_sa2_codes <- sa2_centroids %>%
  st_as_sf(coords = c("lon", "lat"), crs = 2193) %>%
  st_filter(cbd_poly_nztm) %>%
  select(c(usual_residence_statistical_area_2_code_na, usual_residence_statistical_area_2_name_na))
print(cbd_sa2_codes)

#Sanity Check
tmap_mode("view")
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

# tmap_save(AOI_cbd_boundary, filename = "Data_Exploration/all_figures/AOI_cbd_boundary.png")
# tmap_save(AOI_cbd_boundary, filename = "Data_Exploration/all_figures/AOI_cbd_boundary.html")


#Convert Destination coordinates of my flows into spatial points (NZTM)
dest_sf <- st_as_sf(st_drop_geometry(auckland_od_spatial),
                    coords = c("dest_lon", "dest_lat"), 
                    crs = 2193)

# Convert Origin coordinates of my flows into spatial points (NZTM)
orig_sf <- st_as_sf(st_drop_geometry(auckland_od_spatial), 
                    coords = c("orig_lon", "orig_lat"), 
                    crs = 2193)

# Find which destinations and origins lie inside my CBD box
auckland_od_spatial$dest_inside_cbd <- st_intersects(dest_sf, cbd_poly_nztm, sparse = FALSE)[, 1]
auckland_od_spatial$orig_inside_cbd <- st_intersects(orig_sf, cbd_poly_nztm, sparse = FALSE)[, 1]

# #Filter for Inbound Commutes (Destination is INSIDE, Origin is OUTSIDE)
# cbd_inbound_flows <- auckland_od_spatial %>%
#   filter(
#     dest_inside_cbd == TRUE, 
#     orig_inside_cbd == FALSE
#   ) %>%
#   mutate(
#     # Sum up private and company car drivers
#     od_private_vehicles = od_drive_private + od_drive_company
#   ) %>%
#   filter(od_private_vehicles > 0)


# Calculate the total sum of vehicles across all inbound routes
# total_entering_cars <- sum(cbd_inbound_flows$od_private_vehicles, na.rm = TRUE)
# print(paste("Total commute private vehicles entering the custom CBD box in 2023:", total_entering_cars))
# SANITY CHECKS - this number looked way too low: _______________________________________
# centroids_sf$inside_cbd <- st_intersects(centroids_sf, cbd_poly_nztm, sparse = FALSE)[, 1]#Check which centroids fall inside my custom CBD box
# 
# caught_neighborhoods <- travel_income_HO_emp %>%
#   filter(usual_residence_statistical_area_2_code_na %in% 
#            (centroids_sf %>% filter(inside_cbd == TRUE) %>% pull(usual_residence_statistical_area_2_code_na))) %>%
#   pull(usual_residence_statistical_area_2_name_na)
# 
# print(caught_neighborhoods) # there are 21 sa2 neighbour hoods wwhich accurate represents the Acukalnd CBD boundaru ;it looked right
# But the number of trips looked different


# I think there were some deletion prcoess somewhere, and I think the probelm was that 
# od_private_vehicles = od_drive_private + od_drive_company. When there's an NA val included, the entire value for the 
# priv vechiels becoms NA. So unless there's no NA value for od_drive_private and od_drive_company, an interger will be calculated, od_private_vehicles
# The fix was to use coalesce, where NA can be  treated as 0. 
#_______________________________________ 






#Filter for Inbound Commutes (Destination is INSIDE, Origin is OUTSIDE)
# Filter for Inbound Commutes (using coalesce to treat NAs as 0 during the addition)
cbd_inbound_flows <- auckland_od_spatial %>%
  filter(
    dest_inside_cbd == TRUE, 
    orig_inside_cbd == FALSE
  ) %>%
  mutate(
    # coalesce(x, 0) replaces any NA value with 0 during the math
    od_private_vehicles = coalesce(od_drive_private, 0) + coalesce(od_drive_company, 0),
    od_public_transport = coalesce(od_ferry,0) + coalesce(od_train,0) + coalesce(od_public_bus,0), 
    od_active = coalesce(od_bicycle,0) + coalesce(od_walk_jog,0)
  ) %>%
  # Now, we safely filter out rows with 0 actual vehicles
  filter(od_private_vehicles > 0)

# Recalculate the total sum of entering vehicles
total_entering_cars <- sum(cbd_inbound_flows$od_private_vehicles, na.rm = TRUE)
print(paste("Total commute private vehicles(Private and Company Vehicle) entering the custom CBD box in 2023:", total_entering_cars))

# Group by origin and calculate total vehicle trips entering the CBD box
cbd_inbound_table <- cbd_inbound_flows %>%
  st_drop_geometry() %>% 
  group_by(origin_name, origin_code) %>%
  summarise(
    total_pc_vehicles = sum(od_private_vehicles, na.rm = TRUE),
    total_pt_vehicles = sum(od_public_transport, na.rm = TRUE),
    total_active = sum(od_active, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_pc_vehicles))

# Print the top 15 origin suburbs driving into the CBD box
print(head(cbd_inbound_table, 15)) #Desc order for total_pc_vehicles 
# Interpretation: 
# As expected, it's the richer areas that mostly commutes to cbd 
# Based on the 2023 Census Origin-Destination travel data, an estimated 252 total employed residents usually drive a private or company vehicle from Remuera Waitaramoa to the custom CBD cordon for work

cbd_inbound_table$total_commute_vehicles%%3#random rounding to base 3 (RR3) rule to all public census tables can be seen 















# 1. Ensure you have run the coalesce fix first so your dataset is fully populated!
cbd_inbound_flows <- auckland_od_spatial %>%
  filter(
    dest_inside_cbd == TRUE, 
    orig_inside_cbd == FALSE
  ) %>%
  mutate(
    # coalesce treats NAs as 0 so we don't accidentally delete active rows
    od_private_vehicles = coalesce(od_drive_private, 0) + coalesce(od_drive_company, 0)
  ) %>%
  filter(od_private_vehicles > 0)

# 2. Plot the map using ALL lines (cbd_inbound_flows instead of major_inbound_flows)
tmap_mode("view")

# Base Layer: All Auckland SA2s in light grey
tm_shape(travel_income_HO_emp) +
  tm_polygons(
    fill = "lightgrey", 
    fill_alpha = 0.3, 
    col = "white", 
    popup.vars = FALSE
  ) +
  
  # Custom Bounding Box Layer (Yellow transparent polygon)
  tm_shape(cbd_poly_nztm) +
  tm_polygons(
    fill = "yellow", 
    fill_alpha = 0.3, 
    col = "black", 
    lwd = 2, 
    fill.legend = tm_legend(title = "Custom CBD Box")
  ) +
  
  # Inbound Flow Lines Layer (Mapping ALL lines now)
  tm_shape(cbd_inbound_flows) +
  tm_lines(
    col = "od_private_vehicles",
    col.scale = tm_scale(values = "brewer.reds"), 
    col.legend = tm_legend(title = "Entering Vehicles"),
    lwd = "od_private_vehicles",
    lwd.scale = tm_scale_continuous(values.scale = 6),
    # Sets line transparency to 40% so overlapping lines are readable
    line_alpha = 0.4, 
    id = "origin_name",
    popup.vars = c(
      "From" = "origin_name",
      "To" = "dest_name",
      "Vehicles" = "od_private_vehicles"
    )
  ) +
  tm_view(
    basemap.server = c("Esri.WorldGrayCanvas",
                       "OpenStreetMap")
  )







#___________________________
# Create a Sankey Diagram 
#____________________________
library(networkD3)
library(htmlwidgets)

#______________________________________________________________________________
# BUILD THE SANKEY DIAGRAM (COMMUTE MODE SPLIT TO CBD)
#______________________________________________________________________________

# 1. Isolate ALL commutes entering the CBD (not just private vehicles)
cbd_all_modes_inbound <- auckland_od_spatial %>%
  filter(
    dest_inside_cbd == TRUE, 
    orig_inside_cbd == FALSE
  )

# "Among the suburbs that contribute the most to CBD vehicle congestion, 
# what is their alternative transit behavior? Do they have established public 
# transport alternatives, or are they almost exclusively reliant on private cars?"
top_10_origins <-  head(cbd_inbound_table, 10)$origin_name
bottom_10_origins <-  tail(cbd_inbound_table, 10)$origin_name

# Aggregate the mode splits for just these Top 10 suburbs
sankey_data <- cbd_all_modes_inbound %>%
  st_drop_geometry() %>%
  filter(origin_name %in% top_10_origins) %>%
  mutate(
    # Safely sum up the modes using coalesce to handle NAs
    Private_Car = coalesce(od_drive_private, 0) + coalesce(od_drive_company, 0),
    Bus = coalesce(od_public_bus, 0),
    Train = coalesce(od_train, 0),
    Ferry = coalesce(od_ferry, 0),
    Active_Modes = coalesce(od_walk_jog, 0) + coalesce(od_bicycle, 0)
  ) %>%
  # Group by the suburb and sum up the totals for each mode
  group_by(origin_name) %>%
  summarise(
    Private_Car = sum(Private_Car, na.rm = TRUE),
    Bus = sum(Bus, na.rm = TRUE),
    Train = sum(Train, na.rm = TRUE),
    Ferry = sum(Ferry, na.rm = TRUE),
    Active_Modes = sum(Active_Modes, na.rm = TRUE)
  ) %>%
  # Pivot the data into a "long" format: Origin -> Mode -> Volume
  pivot_longer(
    cols = c(Private_Car, Bus, Train, Ferry, Active_Modes),
    names_to = "Mode",
    values_to = "Volume"
  ) %>%
  # Remove lines with 0 commuters to keep the diagram clean
  filter(Volume > 0) %>%
  # Clean up the Mode names for the chart display
  mutate(Mode = str_replace_all(Mode, "_", " "))

# 4. Create the 'nodes' dataframe (a unique list of all Suburbs and Modes)
unique_origins <- as.character(unique(sankey_data$origin_name))
unique_modes <- as.character(unique(sankey_data$Mode))
nodes <- data.frame(name = c(unique_origins, unique_modes))

# 5. Create the 'links' dataframe (Must use 0-indexed IDs for D3.js)
links <- sankey_data %>%
  mutate(
    # match() finds the row number in 'nodes'; we subtract 1 because JavaScript starts counting at 0
    source = match(as.character(origin_name), nodes$name) - 1,
    target = match(as.character(Mode), nodes$name) - 1,
    value = Volume
  ) %>%
  select(source, target, value)

# 6. Generate the Interactive Sankey Diagram
cbd_sankey <- sankeyNetwork(
  Links = links, 
  Nodes = nodes, 
  Source = "source",
  Target = "target", 
  Value = "value", 
  NodeID = "name",
  fontSize = 14, 
  nodeWidth = 30,
  sinksRight = FALSE, # Keeps target nodes aligned neatly
  margin = list(left = 100) # Adds padding so suburb names aren't cut off
)

# Display the diagram in your Viewer pane
cbd_sankey

# 7. Save it as an interactive HTML file
# saveWidget(cbd_sankey, file = "./Data_Exploration/all_figures/cbd_commute_sankey.html", selfcontained = TRUE)














































library(shiny)


#______________________________________________________________________________
# SHINY INTERACTIVE DASHBOARD CODE
#______________________________________________________________________________

# 1. Define the User Interface (UI)
ui <- fluidPage(
  theme = shinytheme("cosmo") %>% tryCatch(error = function(e) NULL), # Optional professional theme
  titlePanel("Auckland CBD Commuter Flow Dashboard (2023)"),
  
  sidebarLayout(
    sidebarPanel(
      # User control: Dropdown to select sorting metric
      selectInput("sort_var", "Sort Suburbs By:",
                  choices = c("Private Car Volume" = "total_pc_vehicles",
                              "Public Transport Volume" = "total_pt_vehicles",
                              "Active Modes Volume" = "total_active")),
      
      # User control: Toggle between Top 10 and Bottom 10
      radioButtons("rank_type", "Subset Selection:",
                   choices = c("Top 10 suburbs" = "top", 
                               "Bottom 10 suburbs" = "bottom")),
      
      helpText("This dashboard allows you to dynamically explore the commuting profile ",
               "of Auckland suburbs entering the CBD. Adjusting the controls will ",
               "recalculate both the summary table and the Sankey diagram on the fly.", 
               "Top/bottom 10 Commuters= 10 suburbs arranged based on travel modes")
    ),
    
    # Display panel with two tabs: the diagram and the matching table
    mainPanel(
      tabsetPanel(
        tabPanel("Interactive Sankey Diagram", sankeyNetworkOutput("sankey_plot")),
        tabPanel("Dynamic Data Table", tableOutput("table_view"))
      )
    )
  )
)

# 2. Define the Server Logic (How R calculates the data dynamically)
server <- function(input, output) {
  
  # Reactive function to calculate the selected suburbs based on user controls
  selected_suburbs <- reactive({
    sorted_table <- cbd_inbound_table
    
    # Sort dynamically based on dropdown selection
    if (input$sort_var == "total_pc_vehicles") {
      sorted_table <- sorted_table %>% arrange(desc(total_pc_vehicles))
    } else if (input$sort_var == "total_pt_vehicles") {
      sorted_table <- sorted_table %>% arrange(desc(total_pt_vehicles))
    } else {
      sorted_table <- sorted_table %>% arrange(desc(total_active))
    }
    
    # Pull either the top 10 or bottom 10 names
    if (input$rank_type == "top") {
      suburbs <- head(sorted_table, 10)$origin_name
    } else {
      suburbs <- tail(sorted_table, 10)$origin_name
    }
    
    return(as.character(suburbs))
  })
  
  # Render the dynamic table view
  output$table_view <- renderTable({
    sorted_table <- cbd_inbound_table
    
    # Re-sort table based on selection
    if (input$sort_var == "total_pc_vehicles") {
      sorted_table <- sorted_table %>% arrange(desc(total_pc_vehicles))
    } else if (input$sort_var == "total_pt_vehicles") {
      sorted_table <- sorted_table %>% arrange(desc(total_pt_vehicles))
    } else {
      sorted_table <- sorted_table %>% arrange(desc(total_active))
    }
    
    # Display the correct slice
    if (input$rank_type == "top") {
      head(sorted_table, 10)
    } else {
      tail(sorted_table, 10)
    }
  })
  
  # Render the dynamic Sankey Diagram
  output$sankey_plot <- renderSankeyNetwork({
    # Get the reactive list of suburbs based on user controls
    suburbs <- selected_suburbs()
    
    # Aggregate and pivot the mode splits for just these suburbs
    sankey_data <- cbd_all_modes_inbound %>%
      st_drop_geometry() %>%
      filter(origin_name %in% suburbs) %>%
      mutate(
        Private_Car = coalesce(od_drive_private, 0) + coalesce(od_drive_company, 0), 
        Bus = coalesce(od_public_bus, 0),
        Train = coalesce(od_train, 0),
        Ferry = coalesce(od_ferry, 0),
        Active_Modes = coalesce(od_walk_jog, 0) + coalesce(od_bicycle, 0)
      ) %>%
      group_by(origin_name) %>%
      summarise(
        Private_Car = sum(Private_Car, na.rm = TRUE),
        Bus = sum(Bus, na.rm = TRUE),
        Train = sum(Train, na.rm = TRUE),
        Ferry = sum(Ferry, na.rm = TRUE),
        Active_Modes = sum(Active_Modes, na.rm = TRUE)
      ) %>%
      pivot_longer(
        cols = c(Private_Car, Bus, Train, Ferry, Active_Modes),
        names_to = "Mode",
        values_to = "Volume"
      ) %>%
      filter(Volume > 0) %>%
      mutate(Mode = str_replace_all(Mode, "_", " "))
    
    # Create nodes
    unique_origins <- as.character(unique(sankey_data$origin_name))
    unique_modes <- as.character(unique(sankey_data$Mode))
    nodes <- data.frame(name = c(unique_origins, unique_modes))
    
    # Create links
    links <- sankey_data %>%
      mutate(
        source = match(as.character(origin_name), nodes$name) - 1,
        target = match(as.character(Mode), nodes$name) - 1,
        value = Volume
      ) %>%
      select(source, target, value)
    
    # Generate the Sankey Diagram
    sankeyNetwork(
      Links = links, 
      Nodes = nodes, 
      Source = "source",
      Target = "target", 
      Value = "value", 
      NodeID = "name",
      fontSize = 14, 
      nodeWidth = 30,
      sinksRight = FALSE,
      margin = list(left = 120)
    )
  })
}

# 3. Launch the Application
shinyApp(ui = ui, server = server)
#http://127.0.0.1:5299/










