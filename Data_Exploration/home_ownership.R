
#_________________________________
# INDIVIDUAL HOME OWNERSHIP GROUPS
#_________________________________

# Clean the travelling groups colnames
sum(str_detect(colnames(travel_props_density), "individual_home_ownership")) #Inspecting the num of cols of travel groups
indiv_HO_inspect = travel_props_density[,which(str_detect(colnames(travel_props_density), "individual_home_ownership"))]
# View(indiv_HO_inspect) #What these are....





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SOME NOTES
# There are essentially 3 groups within the HO(home ownership data):

# 1. hold_in_a_family_trust
# 2. own_or_partly_own
# 3. do_not_own_and_do_not_hold_in_a_family_trust
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# (1) The INSPECTION Function
variable_name_inspect <- function(data, str_detection) {
  
  # Find columns that match the search string
  matching_cols <- colnames(data)[str_detect(colnames(data), str_detection)]
  num_vars <- length(matching_cols)
  
  #Print a helpful summary and list of matching columns to the console
  cat("Found", num_vars, "columns matching pattern '", str_detection, "':\n\n", sep = "")
  print(matching_cols)
  
  #Subset the data frame to keep only those columns
  # (For 'sf' spatial objects, this will automatically keep the 'geometry' column too)
  subset_data <- data[, matching_cols, drop = FALSE]
  
  return(subset_data)
}
#  Home Ownership columns ()  
ho_vars <- travel_props_density %>%
  variable_name_inspect(str_detection = "individual_home_ownership_")




# Renaming Home ownership cols for convenience 
home_ownership_clean <- travel_props_density %>%
  rename_with(
    ~ .x %>% 
      str_remove("^individual_home_ownership_") %>% 
      str_remove("_2023$") %>% 
      paste0("homeownership_", .), # Adds 'homeownership_' to the front
    .cols = starts_with("individual_home_ownership_")
  )
home_ownserhip_only = home_ownership_clean[, which(str_detect(colnames(home_ownership_clean), "homeownership_"))]
# glimpse(home_ownserhip_only)





library(dplyr)

# Calculate Homeownership Proportions and Densities
home_ownership_analysis <- home_ownership_clean %>%
  mutate(
    # -
    # 1. PROPORTIONS (Socioeconomic Shares %)
    # -
    # Proportion of Homeowners (Own + Trust)
    prop_homeownership_owners = (
      homeownership_own_or_partly_own + 
        homeownership_hold_in_a_family_trust
    ) / homeownership_total_stated,
    
    # Proportion of Renters/Non-Owners
    prop_homeownership_renters = 
      homeownership_do_not_own_and_do_not_hold_in_a_family_trust / 
      homeownership_total_stated,
    
    # -
    # 2. DENSITIES (Physical Concentration per sq km)
    # -
    # Density of Homeowners
    density_homeownership_owners = (
      homeownership_own_or_partly_own + 
        homeownership_hold_in_a_family_trust
    ) / LAND_AREA_,
    
    # Density of Renters
    density_homeownership_renters = 
      homeownership_do_not_own_and_do_not_hold_in_a_family_trust / 
      LAND_AREA_
  )


ho_only =  home_ownership_analysis[, which(str_detect(colnames(home_ownership_analysis), "homeownership_"))]
summary(ho_only)










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



# Map: Renter Share
# This shows where renting is the dominant housing choice (eg:student/central areas)
map_renter_share <- create_interactive_map(
  data = home_ownership_analysis, 
  variable = "prop_homeownership_renters", 
  title = "Renter Share ", 
  palette = "YlOrRd",
  style = "quantile"
)

# Map:Renter Density (Renters per sq km)
# This shows where physical rental housing pressure and demand is concentrated (eg: CBD apartments)
map_renter_density <- create_interactive_map(
  data = home_ownership_analysis, 
  variable = "density_homeownership_renters", 
  title = "Renter Density (Renters/sq km)", 
  palette = "Reds",
  style = "jenks"
)







# 1. OWNER AND RENTER SHARES 


#  A. Homeowners 
top_10_owner_share <- home_ownership_analysis %>%
  st_drop_geometry() %>%
  select(usual_residence_statistical_area_2_name_na, prop_homeownership_owners) %>%
  slice_max(order_by = prop_homeownership_owners, n = 10, with_ties = FALSE)

bottom_10_owner_share <- home_ownership_analysis %>%
  st_drop_geometry() %>%
  select(usual_residence_statistical_area_2_name_na, prop_homeownership_owners) %>%
  slice_min(order_by = prop_homeownership_owners, n = 10, with_ties = FALSE)

#  B. Renters 
top_10_renter_share <- home_ownership_analysis %>%
  st_drop_geometry() %>%
  select(usual_residence_statistical_area_2_name_na, prop_homeownership_renters) %>%
  slice_max(order_by = prop_homeownership_renters, n = 10, with_ties = FALSE)

bottom_10_renter_share <- home_ownership_analysis %>%
  st_drop_geometry() %>%
  select(usual_residence_statistical_area_2_name_na, prop_homeownership_renters) %>%
  slice_min(order_by = prop_homeownership_renters, n = 10, with_ties = FALSE)



# 2. OWNER AND RENTER DENSITIES (People/sq km)

#  A. Homeowners 
top_10_owner_density <- home_ownership_analysis %>%
  st_drop_geometry() %>%
  select(usual_residence_statistical_area_2_name_na, density_homeownership_owners) %>%
  slice_max(order_by = density_homeownership_owners, n = 10, with_ties = FALSE)

bottom_10_owner_density <- home_ownership_analysis %>%
  st_drop_geometry() %>%
  select(usual_residence_statistical_area_2_name_na, density_homeownership_owners) %>%
  slice_min(order_by = density_homeownership_owners, n = 10, with_ties = FALSE)

#  B. Renters (matching my exact column spelling) 
top_10_renter_density <- home_ownership_analysis %>%
  st_drop_geometry() %>%
  select(usual_residence_statistical_area_2_name_na, density_homeownership_renters) %>%
  slice_max(order_by = density_homeownership_renters, n = 10, with_ties = FALSE)

bottom_10_renter_density <- home_ownership_analysis %>%
  st_drop_geometry() %>%
  select(usual_residence_statistical_area_2_name_na, density_homeownership_renters) %>%
  slice_min(order_by = density_homeownership_renters, n = 10, with_ties = FALSE)



#  Print Shares  
print(" TOP 10 HOMEOWNER SHARES  ")
print(top_10_owner_share)

print(" TOP 10 RENTER SHARES  ")
print(top_10_renter_share)

#  Print Densities (People/sq km) 
print(" TOP 10 HOMEOWNER DENSITIES ")
print(top_10_owner_density)

print(" TOP 10 RENTER DENSITIES ")
print(top_10_renter_density)











# Homeowner vs Renters
# Standard paired t-test comparing the two columns directly
housing_t_test <- t.test(
  home_ownership_analysis$prop_homeownership_owners, 
  home_ownership_analysis$prop_homeownership_renters, 
  paired = TRUE
)

# # View results
print(housing_t_test)


# Interpretation: 
# Yes there's a significant difference between the 2 groups
# On average across Auckland's neighborhoods, the proportion of renters is 12.17 percentage points higher than the proportion of homeowners.
