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
#____________________________________
# MODE OF TRANSPORT ANALYSE!
home_to_work <- st_read("./data/Saved/home_to_work.gpkg")
mode_of_transport = home_to_work[,which(str_detect(colnames(home_to_work), "transportMode_"))]
# View(mode_of_transport)
colnames(mode_of_transport)

all(mode_of_transport$transportMode_total == mode_of_transport$transportMode_total_stated, na.rm=T)
# returned TRUE across all neighborhoods, it means the count for transportMode_not_elsewhere_included is exactly 0
# Proof:summary(mode_of_transport$transportMode_not_elsewhere_included)

# We can also remove home_to_work$transportMode_did_not_go_to_work_today becuase 
# summary(home_to_work$transportMode_did_not_go_to_work_today)

#___________________________________________________________
# tm_view   Calculating Travelling proportions for home-to-work groups
#___________________________________________________________
travel_props <- home_to_work %>%
  select(-c(transportMode_did_not_go_to_work_today, 
            transportMode_not_elsewhere_included)) %>%
  mutate(across(
    #Select all transport columns except the totals
    .cols = starts_with("transportMode_") & !any_of(c("transportMode_total", "transportMode_total_stated")),
    
    #Divide each column by the total_stated
    .fns = ~ .x / transportMode_total_stated,
    
    #Create new column names starting with 'prop_'
    .names = "prop_{.col}")) %>%
  #Clean up the 'prop_transportMode_' prefix to just 'prop_transport_'
  rename_with(~ str_replace(., "prop_transportMode_", "prop_transport_"), 
              starts_with("prop_")) 



travel_props <- home_to_work %>%
  
  #Calculate individual proportions
  mutate(across(
    .cols = starts_with("transportMode_") & !any_of(c("transportMode_total", "transportMode_total_stated")),
    .fns = ~ .x / transportMode_total_stated,
    .names = "prop_{.col}"
  )) %>%
  
  # Clean up prefixes
  rename_with(~ str_replace(., "prop_transportMode_", "prop_transport_"), 
              starts_with("prop_")) %>% 
  
  #Calculate Aggregate Proportions (Private Car vs Public Transport)
  mutate(
    # Private Car proportion
    prop_aggregate_private_car = (
      transportMode_drive_a_private_car_truck_or_van + 
        transportMode_drive_a_company_car_truck_or_van #transportMode_passenger_in_a_car_truck_van_or_company_bus will not inlcude this becuase my research focuses on Traffic congestion
    ) / transportMode_total_stated,
    
    # Public Transport proportion
    prop_aggregate_public_transport = (
      transportMode_public_bus + 
        transportMode_train + 
        transportMode_ferry
    ) / transportMode_total_stated, 
    
    #Active Modes proportion (Bicycle + Walk or Jog)
    prop_aggregate_active_modes = (
      transportMode_walk_or_jog + 
        transportMode_bicycle
    ) / transportMode_total_stated,
    
    # Work at Home proportion
    prop_aggregate_work_at_home = transportMode_work_at_home / transportMode_total_stated,
    
    # Other Modes proportion
    prop_aggregate_other = transportMode_other / transportMode_total_stated
  )

# Summary of the traveling proportios for all modes
travel_props_only = travel_props[, c(2, which(str_detect(colnames(travel_props), "prop_transport")))]
summary(travel_props_only)


# SOME OBSERVATIONS; there are some groups that has 0 PT, 
#iv'e decided to inestigate his further



# 1. FILTER THE 8 SUBSETS DIRECTLY FROM THE MAIN DATASET
no_company_car <- travel_props %>% filter(prop_transport_drive_a_company_car_truck_or_van == 0)
no_passenger   <- travel_props %>% filter(prop_transport_passenger_in_a_car_truck_van_or_company_bus == 0)
no_bus         <- travel_props %>% filter(prop_transport_public_bus == 0)
no_train       <- travel_props %>% filter(prop_transport_train == 0)
no_bicycle     <- travel_props %>% filter(prop_transport_bicycle == 0)
no_walk_jog    <- travel_props %>% filter(prop_transport_walk_or_jog == 0)
no_ferry       <- travel_props %>% filter(prop_transport_ferry == 0)
no_other       <- travel_props %>% filter(prop_transport_other == 0)

# 2.tm_view   H BUILD THE MULTI-LAYERED MAP WITH TOGGLEABLE GROUPS
tmap_mode("view")
zero_modes_map <- tm_shape(travel_props) +#Base Map (All of Auckland in light grey to provide geographical context)
  tm_polygons(
    col = "lightgrey", 
    border.col = "white", 
    alpha = 0.3, 
    id = "usual_residence_statistical_area_2_name_na",
    popup.vars = FALSE # Disables background layer pop-ups
  ) +
  
  # Layer 1: 0% Company Car (Red)
  tm_shape(no_company_car) +
  tm_polygons(col = "#E41A1C", alpha = 0.7, title = "0% Company Car", group = "0% Company Car",
              id = "usual_residence_statistical_area_2_name_na",
              popup.vars = c("Share" = "prop_transport_drive_a_company_car_truck_or_van",
                             "Total Commuters" = "transportMode_total_stated")) +
  
  # Layer 2: 0% Car Passenger (Blue)
  tm_shape(no_passenger) +
  tm_polygons(col = "#377EB8", alpha = 0.7, title = "0% Car Passenger", group = "0% Car Passenger",
              id = "usual_residence_statistical_area_2_name_na",
              popup.vars = c("Share" = "prop_transport_passenger_in_a_car_truck_van_or_company_bus",
                             "Total Commuters" = "transportMode_total_stated")) +
  
  # Layer 3: 0% Public Bus (Green)
  tm_shape(no_bus) +
  tm_polygons(col = "#4DAF4A", alpha = 0.7, title = "0% Public Bus", group = "0% Public Bus",
              id = "usual_residence_statistical_area_2_name_na",
              popup.vars = c("Share" = "prop_transport_public_bus",
                             "Total Commuters" = "transportMode_total_stated")) +
  
  # Layer 4: 0% Train (Purple)
  tm_shape(no_train) +
  tm_polygons(col = "#984EA3", alpha = 0.7, title = "0% Train", group = "0% Train",
              id = "usual_residence_statistical_area_2_name_na",
              popup.vars = c("Share" = "prop_transport_train",
                             "Total Commuters" = "transportMode_total_stated")) +
  
  # Layer 5: 0% Bicycle (Orange)
  tm_shape(no_bicycle) +
  tm_polygons(col = "#FF7F00", alpha = 0.7, title = "0% Bicycle", group = "0% Bicycle",
              id = "usual_residence_statistical_area_2_name_na",
              popup.vars = c("Share" = "prop_transport_bicycle",
                             "Total Commuters" = "transportMode_total_stated")) +
  
  # Layer 6: 0% Walk or Jog (Yellow)
  tm_shape(no_walk_jog) +
  tm_polygons(col = "#FFFF33", alpha = 0.7, title = "0% Walk or Jog", group = "0% Walk or Jog",
              id = "usual_residence_statistical_area_2_name_na",
              popup.vars = c("Share" = "prop_transport_walk_or_jog",
                             "Total Commuters" = "transportMode_total_stated")) +
  
  # Layer 7: 0% Ferry (Brown)
  tm_shape(no_ferry) +
  tm_polygons(col = "#A65628", alpha = 0.7, title = "0% Ferry", group = "0% Ferry",
              id = "usual_residence_statistical_area_2_name_na",
              popup.vars = c("Share" = "prop_transport_ferry",
                             "Total Commuters" = "transportMode_total_stated")) +
  
  # Layer 8: 0% Other (Pink)
  tm_shape(no_other) +
  tm_polygons(col = "#F781BF", alpha = 0.7, title = "0% Other Modes", group = "0% Other Modes",
              id = "usual_residence_statistical_area_2_name_na",
              popup.vars = c("Share" = "prop_transport_other",
                             "Total Commuters" = "transportMode_total_stated")) +
  
  # Set # Views and basemaps
  tm_view(
    basemaps = c("OpenStreetMap", "Esri.WorldGrayCanvas")
  )


# Save the interactive map as an HTML webpage
# tmap_save(
#   tm = zero_modes_map, 
#   filename = "./Data_Exploration/all_figures/zero_modes_map.html"
# )











#_______________________________
# Visualize 
#_______________________________
#prop_aggregate_public_transport, prop_aggregate_private_car
target_var <- "prop_aggregate_public_transport"
summary(travel_props[[target_var]])

#_______________________________________________________________________________
# tm_view   HISTOGRAM OF POPORTIONS FOR ONE TRAVELLING GROUP
#_______________________________________________________________________________
# Calculate stats beforehand to keep the plot code clean and avoid warnings
mean_val <- mean(travel_props[[target_var]], na.rm = TRUE)
median_val <- median(travel_props[[target_var]], na.rm = TRUE)

ggplot(travel_props, aes(x = .data[[target_var]])) +
  # Set histogram y-axis to density
  geom_histogram(aes(y = after_stat(density)), fill = "lightblue", color = "white", bins = 30, alpha = 0.7) +
  
  # Add the density curve (smoother)
  geom_density(color = "darkblue", linewidth = 1) +
  
  # Add Mean (dashed red line) and Median (dotted green line)
  geom_vline(xintercept = mean_val, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = median_val, color = "green", linetype = "dotted", linewidth = 1) +
  
  theme_minimal() +
  labs(
    title = paste("Distribution of\n", target_var),
    #subtitle = "Mean(red),Median(green),Density Curve",
    x = "Proportion of Commuters",
    y = "Density"
  )


#_______________________________________________________________________________
# tm_view    BOXPLOT - IDENTIFY  OUTLIERS
#_______________________________________________________________________________
ggplot(travel_props, aes(y = .data[[target_var]])) +
  geom_boxplot(fill = "lightgreen",
               outlier.color = "red", 
               outlier.shape = 16) +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  labs(
    title = paste("Boxplot & Outliers of\n", target_var),
    y = "Proportion"
  )



#_______________________________________________________________________________
# tm_view    INVESTIGATE THE OUTLIERS
#_______________________________________________________________________________
#Calculate the statistical outlier boundaries using the IQR method
q1 <- quantile(travel_props[[target_var]], 0.25, na.rm = TRUE)
q3 <- quantile(travel_props[[target_var]], 0.75, na.rm = TRUE)
iqr <- q3 - q1
upper_boundary <- q3 + 1.5 * iqr
lower_boundary <- q1 - 1.5 * iqr

#Extract the actual outlier rows
outlier_neighborhoods <- travel_props %>%
  st_drop_geometry() %>%
  filter(.data[[target_var]] > upper_boundary | .data[[target_var]] < lower_boundary) %>%
  select(
    usual_residence_statistical_area_2_name_na, 
    .data[[target_var]], 
    transportMode_total_stated
  ) %>%
  arrange(desc(.data[[target_var]]))

#Print the outliers
print(outlier_neighborhoods)



#_______________________________________________________________________________
# tm_view    MAP THE POPORTIONS
#_______________________________________________________________________________
tmap_mode("plot")
tm_shape(travel_props) +
  tm_polygons(target_var, 
              style = "quantile", 
              n = 5,
              palette = "Reds", 
              title = "Proportion") +
  tm_title(paste("Spatial Distribution of\n", target_var))

#________________________
# tm_view  IDENTIFY NEIGHBOURHOODS
#________________________
# Identify the Top 5 neighborhoods for this variable (NAs are excluded automatically)
(top_5_neighborhoods <- travel_props %>%
  st_drop_geometry() %>%
  select(usual_residence_statistical_area_2_name_na, .data[[target_var]]) %>%
  slice_max(order_by=.data[[target_var]], n=5))

# Bottom 5 neighborhoods 
bottom_5_neighborhoods <- travel_props %>%
  st_drop_geometry() %>%
  select(usual_residence_statistical_area_2_name_na, .data[[target_var]]) %>%
  slice_min(order_by=.data[[target_var]], n=5)

summary(travel_props$prop_aggregate_public_transport)



#_______________________________________________________________________________
# tm_view   Summarize the average proportion of each mode across all 627 neighborhoods
#_______________________________________________________________________________
regional_shares <- travel_props %>%
  st_drop_geometry() %>%
  select(starts_with("prop_transport_")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Mode",
    values_to = "Share" 
    ) %>%
  # Group and calculate summary stats including Margin of Error (95% CI)
  group_by(Mode) %>%
  summarise(
    Average_Share = mean(Share, na.rm = TRUE),
    SD = sd(Share, na.rm = TRUE),
    n = sum(!is.na(Share)),
    SE = SD / sqrt(n),
    MoE = 1.96 * SE
  ) %>%
  # Format names for display
  mutate(Mode = Mode %>% 
           str_remove("prop_transport_") %>% 
           str_replace_all("_", " ") %>% 
           tools::toTitleCase(.))

#-----------------------------------
# tm_view    Plot with Error Bars and Percentages
#-----------------------------------
ggplot(regional_shares, aes(x = reorder(Mode, Average_Share), y = Average_Share, fill = Mode)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  
  # Add 95% CI Error Bars
  geom_errorbar(aes(ymin = Average_Share - MoE, ymax = Average_Share + MoE), 
                width = 0.2, color = "red", alpha = 0.7) +
  
  # Add Percentage text labels
  geom_text(aes(label = scales::percent(Average_Share, accuracy = 0.1)), 
            hjust = -0.3, size = 3.5, fontface = "bold") +
  
  coord_flip() + 
  theme_minimal() +
  labs(
    title = "Auckland Regional Commuting Mode Split (2023)",
    subtitle = "Mean share across 627 neighbourhoods with 95% Margin of Error",
    x = "",
    y = "Average Share (%)"
  )

#_______________________________________________________________________________
# tm_view   Summarize the average proportion of PT vs PC for 627 neighborhoods
#_______________________________________________________________________________
shares_5modes <- travel_props %>%
  st_drop_geometry() %>%
  select(starts_with("prop_aggregate_")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Mode",
    values_to = "Share"
  ) %>%
  # Group and calculate summary stats including Margin of Error (95% CI)
  group_by(Mode) %>%
  summarise(
    Average_Share = mean(Share, na.rm = TRUE),
    SD = sd(Share, na.rm = TRUE),
    n = sum(!is.na(Share)),
    SE = SD / sqrt(n),
    MoE = 1.96 * SE
  ) %>%
  # Format names beautifully (corrected prefix removal to 'prop_aggregate_')
  mutate(Mode = Mode %>% 
           str_remove("prop_aggregate_") %>% 
           str_replace_all("_", " ") %>% 
           tools::toTitleCase(.))

# Plot with Error Bars and Percentages
ggplot(shares_5modes, aes(x = reorder(Mode, Average_Share), 
                         y = Average_Share, fill = Mode)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  
  # Add 95% Confidence Interval Error Bars
  geom_errorbar(aes(ymin = Average_Share - MoE, ymax = Average_Share + MoE), 
                width = 0.15, color = "red", alpha = 0.7) +
  
  #Add Percentage text labels
  geom_text(aes(label = scales::percent(Average_Share, accuracy = 0.1)), 
            hjust = -0.3, size = 4, fontface = "bold") +
  
  coord_flip() + 
  theme_minimal() +
  labs(
    title = "Auckland Private Vehicle vs. Public Transport Split (2023)",
    subtitle = "Mean share across 627 neighbourhoods with 95% Margin of Error",
    x = "",
    y = "Average Share (%)"
  )
#Active Modes proportion (Bicycle + Walk or Jog)
# Other: Travel modes of outside of the groups in the data=motorcycles, escooter, taxi/rideshaes
# 68.7 + 18.3 + 7.5 + 4.3 + 1.2 ==100 



# NEXT PLANS ------------
# Is there a signficant differece between the 5 mode shares; PC, WFM, PT, Active modes and other? Perhaps we can use the anova table to make the comparison yea? 

# I want to create a chloropeth map; 
#(1) Identify top 5 SA2 neighbourhood with high and low PC  
#(2)top 5 SA2 neighbourhood with high and low PT





#___________________________________________
# tm_viewIS THERE A SIG DIF BETWEEN THE 5 MODES?
#___________________________________________
travel_props_aggregated_only = travel_props[, c(2, which(str_detect(colnames(travel_props), "prop_aggregate_")))]
# Save reshaped long-format data
travel_props_anova_data <- travel_props_aggregated_only %>% 
  st_drop_geometry() %>%
  pivot_longer(
    cols = starts_with("prop_aggregate_"),
    names_to = "Mode",
    values_to = "Share"
  ) %>%
  mutate(
    Mode = Mode %>% 
      str_remove("prop_aggregate_") %>% 
      str_replace_all("_", " ") %>% 
      tools::toTitleCase(.),
    Mode = factor(Mode),
    usual_residence_statistical_area_2_name_na = factor(usual_residence_statistical_area_2_name_na)
  )

# Run the Repeated Measures ANOVA - one-way repeated measures ANOVA Method
anova_repeated <- aov(
  Share ~ Mode + Error(usual_residence_statistical_area_2_name_na/Mode), 
  data = travel_props_anova_data
)
summary(anova_repeated)## View the results

#  ✍️  _ Interpretations _____________
#Are the regional averages of these five travel modes different from each other, 
#or are they roughly the same?
# F(7154,p<0.001). There is an extremely significant difference in the average
# shares of these five commuting modes across Auckland's neighborhoods. 
#They are not even remotely equal.



#_________________some revision notes___________
# Hinami's Note: I was taught to use the lm() and then use abova, however lm() assumed iid, which is not true for this case; propostions are highly dependant to each other. 
# We can see in my anova(fit), the F-value is 8937.5, which is > than in the aov. 
# In the one way anova: 
#SSTotal=SSMode+SSResiduals
#DfResiduals=N−k=3090−5=3085

# In the Repeated Measures ANOVA: 
# SS Total  =SS Mode +SS Neighborhoods +SS Within-Subject Residuals
# DfNeighborhoods=n−1= 618−1 =617, DfMode=k−1=5−1=4, Df Within-Subject Residuals=(n−1)×(k−1)=617×4=2468
# fit <- lm(Share ~ Mode, data=travel_props_anova_data)
# summary(fit)
# anova(fit)

#travel_props_anova_data %>%   group_by(usual_residence_statistical_area_2_name_na ) %>%count()
# we see that here (n=617 neighbourhoods )* (k=5 mode types) = (N=3085)

#_________________some revision notes END___________



#_______________________________________________________________________________
# tm_viewI want to create a chloropeth map for each 5 Mode shares; but mainly PC vs PT
#_______________________________________________________________________________
#  THE INTERACTIVE MAP FUNCTION
create_interactive_map <- function(data, variable, title, palette = "Purples") {
  # Set tmap to interactive # Viewing mode
  tmap_mode("view")
  
  map <- tm_shape(data) +
    tm_polygons(
      col = variable,
      style = "quantile",
      n = 5,
      palette = palette,
      title = title,
      alpha = 0.7,
      # Sets the hover/click label to the neighborhood's name
      id = "usual_residence_statistical_area_2_name_na", 
      # Customize the pop-up table fields
      popup.vars = c(
        "Proportion" = variable,
        "Total Commuters" = "transportMode_total_stated"
      )
    ) +
    # Set default basemaps and reasonable zoom levels for Auckland
    tm_view(
      basemaps = c("OpenStreetMap", "Esri.WorldGrayCanvas"),
      set.zoom.limits = c(9, 14)
    )
  
  return(map)
}

#_________________________________________________________________
# GENERATE THE 5 INTERACTIVE MAPS
#_________________________________________________________________
# Map 1: Private Car Share
map_private_car <- create_interactive_map(
  data = travel_props, 
  variable = "prop_aggregate_private_car", 
  title = "Private Car Share", 
  palette = "Blues"
)

# Map 2: Public Transport Share
map_public_transport <- create_interactive_map(
  data = travel_props, 
  variable = "prop_aggregate_public_transport", 
  title = "PT Share", 
  palette = "Oranges"
)

# Map 3: Active Modes Share
map_active_modes <- create_interactive_map(
  data = travel_props, 
  variable = "prop_aggregate_active_modes", 
  title = "Active Modes Share", 
  palette = "Greens"
)

# Map 4: Work at Home Share
map_work_at_home <- create_interactive_map(
  data = travel_props, 
  variable = "prop_aggregate_work_at_home", 
  title = "Work at Home Share", 
  palette = "Purples"
)

# Map 5: Other Modes Share
map_other_modes <- create_interactive_map(
  data = travel_props, 
  variable = "prop_aggregate_other", 
  title = "Other Modes Share", 
  palette = "Reds"
)




# Save the interactive map as a standalone, self-contained HTML file
# tmap_save(map_private_car, filename = "./Data_Exploration/all_figures/map_private_car.html")

# however there are issues with this; density bias


# THE FIXED --------------------------------------------------------------------
#_________________________________________________________________
# tm_view    Calculate raw counts and densities for all 5 aggregate groups
#_________________________________________________________________
travel_props_density <- travel_props %>%
  mutate(
    # 1. Private Car
    raw_private_car = transportMode_drive_a_private_car_truck_or_van + 
      transportMode_drive_a_company_car_truck_or_van , 
    # I didn't include transportMode_passenger_in_a_car_truck_van_or_company_bus, 
    #because I'm analysing for traffic congestion
    density_private_car = raw_private_car / LAND_AREA_,
    
    # 2. Public Transport
    raw_public_transport = transportMode_public_bus + 
      transportMode_train + 
      transportMode_ferry,
    density_public_transport = raw_public_transport / LAND_AREA_,
    
    # 3. Active Modes
    raw_active_modes = transportMode_walk_or_jog + transportMode_bicycle,
    density_active_modes = raw_active_modes / LAND_AREA_,
    
    # 4. Work at Home
    raw_work_at_home = transportMode_work_at_home,
    density_work_at_home = raw_work_at_home / LAND_AREA_,
    
    # 5. Other Modes
    raw_other = transportMode_other,
    density_other = raw_other / LAND_AREA_
  )




#tm_view Quick Summary of the Density  for  eeach mode share s
travlel_density_only <- travel_props_density[, c(2, 
                                                 which(stringr::str_detect(colnames(travel_props_density), "^density_")))]
summary(travlel_density_only)


# ✍️  Some interpretations of the summary of the density summary of the 5 mode shares: 
# Min of 0 vehciles/km^2 for PT ; maybe not pt access in the region? 
# Huge ranges for each which makes sense, where areas like CBD wll have greter PT access. 





# IDENTIFY THE NEIGHBRHODS WITH 
# 1. PRIVATE CAR (PC) EXTREMES

# By Proportion (Mode Share % - from travel_props
pc_share_top5 <- travel_props %>%
  st_drop_geometry() %>%
  select(usual_residence_statistical_area_2_name_na, prop_aggregate_private_car) %>%
  slice_max(order_by = prop_aggregate_private_car, n = 5, with_ties = FALSE)

pc_share_bottom5 <- travel_props %>%
  st_drop_geometry() %>%
  select(usual_residence_statistical_area_2_name_na, prop_aggregate_private_car) %>%
  slice_min(order_by = prop_aggregate_private_car, n = 5, with_ties = FALSE)

#  By Density (Vehicles/sq km - from travel_props_density) ---
pc_density_top5 <- travel_props_density %>%
  st_drop_geometry() %>%
  select(usual_residence_statistical_area_2_name_na, density_private_car) %>%
  slice_max(order_by = density_private_car, n = 5, with_ties = FALSE)

pc_density_bottom5 <- travel_props_density %>%
  st_drop_geometry() %>%
  select(usual_residence_statistical_area_2_name_na, density_private_car) %>%
  slice_min(order_by = density_private_car, n = 5, with_ties = FALSE)


# 2. PUBLIC TRANSPORT (PT) EXTREMES
pt_share_top10 <- travel_props %>%
  st_drop_geometry() %>%
  select(usual_residence_statistical_area_2_name_na, prop_aggregate_public_transport) %>%
  slice_max(order_by = prop_aggregate_public_transport, n = 10, with_ties = FALSE)

pt_share_bottom10 <- travel_props %>%
  st_drop_geometry() %>%
  select(usual_residence_statistical_area_2_name_na, prop_aggregate_public_transport) %>%
  slice_min(order_by = prop_aggregate_public_transport, n = 10, with_ties = FALSE)

#  (Commuters/sq km - from travel_props_density)
pt_density_top10 <- travel_props_density %>%
  st_drop_geometry() %>%
  select(usual_residence_statistical_area_2_name_na, density_public_transport) %>%
  slice_max(order_by = density_public_transport, n = 10, with_ties = FALSE)

pt_density_bottom10 <- travel_props_density %>%
  st_drop_geometry() %>%
  select(usual_residence_statistical_area_2_name_na, density_public_transport) %>%
  slice_min(order_by = density_public_transport, n = 10, with_ties = FALSE)



















#_________________________________________________________________
# The interactive Map function
#_________________________________________________________________
create_interactive_map <- function(data, variable, title, palette = "Purples", style = "jenks") {
  # Set tmap to interactive # Viewing mode
  tmap_mode("view")
  
  map <- tm_shape(data) +
    tm_polygons(
      col = variable, 
      style = style, # 'jenks' style is ideal for heavily skewed density data, but you can override with quantile
      n = 5,
      palette = palette,
      title = title,
      alpha = 0.7,
      id = "usual_residence_statistical_area_2_name_na",
      
      # Dynamically matches the variable name inside the interactive popup
      popup.vars = c(
        "Value" = variable,
        "Total Commuters" = "transportMode_total_stated",
        "Land Area (sq km)" = "LAND_AREA_"
      )
    ) +
    tm_view(
      basemaps = c("Esri.WorldGrayCanvas", "OpenStreetMap"),
      set.zoom.limits = c(9, 14)
    )
  
  return(map)
}

#_________________________________________________________________
#tm_view QUsing the function to create the maps for the 5 mode shares;
#this time we are accuounting for the population/land area
#_________________________________________________________________
#  Private Car Density Map
map_density_car <- create_interactive_map(
  data = travel_props_density, 
  variable = "density_private_car", 
  title = "Private Car Density (Cars/sq km)", 
  palette = "Reds",
  style = "jenks"
)


#Public Transport Density Map
map_density_pt <- create_interactive_map(
  data = travel_props_density, 
  variable = "density_public_transport", 
  title = "PT Density (Commuters/sq km)", 
  palette = "Oranges",
  style = "jenks"
)

# Active Modes Density Map
map_density_active <- create_interactive_map(
  data = travel_props_density, 
  variable = "density_active_modes", 
  title = "Active Density (Commuters/sq km)", 
  palette = "Greens",
  style = "jenks"
)


# Work At Home Density Map
map_density_WAH<- create_interactive_map(
  data = travel_props_density, 
  variable = "density_work_at_home", 
  title = "Work At Home Density (Commuters/sq km)", 
  palette = "Blues",
  style = "jenks"
)

# Other Modes Density Map
map_density_other <- create_interactive_map(
  data = travel_props_density, 
  variable = "density_other", 
  title = "Other Modes Density (Commuters/sq km)", 
  palette = "Purples",
  style = "jenks"
)

# 4. Private Car Proportion Map
map_prop_car <- create_interactive_map(
  data = travel_props_density, 
  variable = "prop_aggregate_private_car", 
  title = "Private Car Share (%)", 
  palette = "Blues",
  style = "quantile"
)


# SEE THEM ALL STAKCED
tmap_mode("view")
map_all_modes_stacked <- 
  # Base Map Layer
  tm_shape(travel_props_density) +
  tm_polygons(col = "lightgrey", border.col = "white", alpha = 0.2, popup.vars = FALSE) +
  
  # Layer 1: Private Car
  tm_shape(travel_props_density) +
  tm_polygons("density_private_car", palette = "Blues", style = "jenks",
              group = "Private Car Density", title = "Private Car") +
  
  # Layer 2: Public Transport
  tm_shape(travel_props_density) +
  tm_polygons("density_public_transport", palette = "Oranges", style = "jenks",
              group = "PT Density", title = "Public Transport") +
  
  # Layer 3: Active Modes
  tm_shape(travel_props_density) +
  tm_polygons("density_active_modes", palette = "Greens", style = "jenks",
              group = "Active Density", title = "Active Modes") +
  
  # Layer 4: Work at Home
  tm_shape(travel_props_density) +
  tm_polygons("density_work_at_home", palette = "Purples", style = "jenks",
              group = "Work at Home Density", title = "Work at Home") +
  
  # Layer 5: Other Modes
  tm_shape(travel_props_density) +
  tm_polygons("density_other", palette = "Reds", style = "jenks",
              group = "Other Density", title = "Other") +
  
  tm_view(
    basemaps = c("Esri.WorldGrayCanvas", "OpenStreetMap"),
    set.zoom.limits = c(9, 14)
  )



# Save the interactive map as a standalone, self-contained HTML file
# tmap_save(map_density_car, filename = "./Data_Exploration/all_figures/map_density_car.html")
# tmap_save(map_density_pt, filename = "./Data_Exploration/all_figures/map_density_pt.html")
# tmap_save(map_all_modes_stacked, filename = "./Data_Exploration/all_figures/map_all_modes_stacked.html")




































