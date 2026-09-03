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


#__________________________________________________________
# Load Data
# Name of the data: 2023 Census totals by topic for individuals by statistical area 2 – part 2
#__________________________________________________________
sa2_ind <- st_read("./data/SA2-part2/2023-census-totals-by-topic-for-individuals-by-statistical-a.shp")
look_up = read.csv("./data/SA2-part2/2023_census_totals_by_topic_for_individuals_by_sa2_part_2_lookup_table.csv")


# Search for columns related to travel to work
travel_vars <- look_up %>%
  filter(str_detect(Variable1, "Main means of travel to")) %>%
  select(c(Shapefile_name, Variable1, Variable1_category, Year, Field_name_alias))

#Mode Types
(unique(travel_vars$Variable1_category))



look_up[which(is.na(look_up$Year)), ]# Inspect the rows where years show NAs; we keep these

# Filter the lookup table FIRST to keep only 2023 variables and geographic metadata
look_up_2023 <- look_up %>%
  filter(Year == 2023 | is.na(Year)) # %>%# Keep variables from 2023 OR variables with NA years (geographic codes/names)





# Build descriptive names for the 2023 dataset
look_up_cleaned_2023 <- look_up_2023 %>%
  mutate(
    descriptive_name = case_when(
      # If both main variable and category exist, combine them with the year
      !is.na(Variable1) & !is.na(Variable1_category) ~ paste(Variable1, Variable1_category, Year, sep = "_"),
      
      # Otherwise use the field name alias
      !is.na(Field_name_alias) ~ paste(Field_name_alias, Year, sep = "_"),
      
      # Fallback to the original shapefile name
      TRUE ~ Shapefile_name
    )
  ) %>%
  mutate(clean_name = make_clean_names(descriptive_name)) # (eg: "usual_residence_public_bus_2023")
# glimpse(look_up_cleaned_2023)



rename_map_2023 <- setNames(look_up_cleaned_2023$clean_name, look_up_cleaned_2023$Shapefile_name)# Create a named vector mapping old names to new names: c("old_name" = "new_name")

#Explicitly define which spatial columns must be preserved
spatial_cols <- c("SA22023_V1", "SA22023__1", "SA22023__2", "AREA_SQ_KM", "LAND_AREA_SQ_KM") # del->, "Shape_Leng", "geometry"
# spatial_cols %in% names(rename_map_2023) #Sanity Check



# Subset the dataset to keep ONLY spatial columns and 2023 columns, then rename them
sa2_renamed_2023 <- sa2_ind %>%
  
  # subset columns first (dropping 2013 and 2018)
  select(any_of(c(spatial_cols, names(rename_map_2023)))) %>%
  
  # rename remaining columns in bulk
  rename_with(~ recode(., !!!rename_map_2023), .cols = any_of(names(rename_map_2023)))





#________________________________
# Clip By Boundary
#________________________________
# Boundary Layer 
# boundary = st_read("./data/boundary/TA-2023-clipped.shp") #Not Akl boundary anymore
# akl_boundary = boundary %>%# Filter to Auckland
#   filter(TA2023_V1_ == "076")

boundary = st_read("./data/boundary/urban-rural-2023-clipped-generalised/urban-rural-2023-clipped-generalised.shp")

akl_boundary = boundary %>%# Filter to Auckland
  filter(IUR2023__1  == "Major urban area")


#Align projections-Ensure the Auckland boundary uses the exact same projection as the SA2 variables
akl_boundary_proj <- st_transform(akl_boundary, st_crs(sa2_renamed_2023) )

# This removes all national data and oceanic data outside the Auckland TA boundary line
sa2_clipped <- st_intersection(sa2_renamed_2023, akl_boundary_proj)

# Clean up the clipped boundaries (This fixes the terra plotting error)
sa2_clipped_clean <- sa2_clipped %>%
  st_make_valid() %>%# Repair self-intersections/invalid border shapes
  st_collection_extract("POLYGON") %>%# Drop points and lines created on the boundary edges
  st_cast("MULTIPOLYGON") %>%# Standardize geometry types to MULTIPOLYGON
  filter(!st_is_empty(.))# Remove any empty geometry rows

# Convert and plot
# This will now convert and plot with terra without any geometry mismatch errors
plot(vect(sa2_clipped_clean), 
     col = "lightblue", 
     main = "Auckland Region | Major Urban Area Only (Clipped)")




#__________________________________________________________
# Clean Sentinel Values (Negative Census Codes)
#__________________________________________________________
#summary(sa2_clipped_clean) #we notice -999, -997 vals (typical for statsNZ dataset)
# Replace all negative values (-999, -997) with NA across all numeric columns
sa2_clipped_final <- sa2_clipped_clean %>%
  mutate(across(
    where(is.numeric), 
    ~ ifelse(. < 0, NA, .)
  ))






sf::st_write(
  sa2_clipped_final,
  dsn = "./data/Saved/sa2_clipped_clean.gpkg",
  layer = "sa2_clipped_clean",
  delete_dsn = TRUE # Overwrites the file if it already exists
)




# UNDERSTANDNING THE DATA - CONTEXT 
# How many neighbourhoods in my Auckland SA2?
length(unique(sa2_clipped_final$usual_residence_statistical_area_2_name_na))



























# _______________________________________________________
#  STARTING POINT - CLEANED DATA SET
# _______________________________________________________
sa2_clipped_clean <- st_read("./data/Saved/sa2_clipped_clean.gpkg")




#____________________
# TRAVELLING GROUPS
#____________________
# Inspection
sum(str_detect(colnames(sa2_clipped_clean), "main_means_of_travel_to")) #Inspecting the num of cols of travel groups
travel_groups_inspect = sa2_clipped_clean[,which(str_detect(colnames(sa2_clipped_clean), "main_means_of_travel_to"))]
# View(travel_groups_inspect) #What these are....
sum(str_detect(colnames(travel_groups_inspect), "main_means_of_travel_to"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SOME NOTES
# There are essentially 4 groups in these data:

# # 1. Trip to work
# - main_means_of_travel_to_work_by_usual_residence
# - main_means_of_travel_to_work_by_workplace_address
# 
# # Trip to education
# - main_means_of_travel_to_education_by_usual_residence_address
# - main_means_of_travel_to_education_by_education_address #What does this mean? 

# I will be focusing on the home->work dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Inspect Home-to-work data set and filter
home_to_work <- sa2_clipped_clean %>%
  select(!contains(c(
    "main_means_of_travel_to_education_by_usual_residence_address", 
    "main_means_of_travel_to_education_by_education_address", 
    "main_means_of_travel_to_work_by_workplace_address"
  ))) 

# Renaming Travelling cols for convenience 
home_to_work_clean <- home_to_work %>%
  rename_with(
    ~ .x %>% 
      str_remove("^main_means_of_travel_to_work_by_usual_residence_address_") %>% 
      str_remove("_2023$") %>% 
      paste0("transportMode_", .), # Adds 'transport_' to the front
    .cols = starts_with("main_means_of_travel_to_work_by_usual_residence_address_")
  )




# Save this
sf::st_write(
  home_to_work_clean,
  dsn = "./data/Saved/home_to_work.gpkg",
  layer = "home_to_work",
  delete_dsn = TRUE # Overwrites the file if it already exists
)










#_________________________________
# INDIVIDUAL HOME OWNERSHIP GROUPS - 
#_________________________________

# Clean the travelling groups colnames
sum(str_detect(colnames(home_to_work_clean), "individual_home_ownership")) #Inspecting the num of cols of travel groups
indiv_HO_inspect = home_to_work_clean[,which(str_detect(colnames(home_to_work_clean), "individual_home_ownership"))]
# View(indiv_HO_inspect) #What these are....

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SOME NOTES
# There are essentially 3 groups within the HO(home ownership data):

# 1. hold_in_a_family_trust
# 2. own_or_partly_own
# 3. do_not_own_and_do_not_hold_in_a_family_trust
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Renaming Home ownership cols for convenience 
home_ownership_clean <- home_to_work_clean %>%
  rename_with(
    ~ .x %>% 
      str_remove("^individual_home_ownership_") %>% 
      str_remove("_2023$") %>% 
      paste0("home_ownership_", .), # Adds 'home_ownership_' to the front
    .cols = starts_with("individual_home_ownership_")
  )



#_______________________________________________________________________________
# AUTOMATION FOR THE REST OF THE VARIABLES -  INSPECTION 
#_______________________________________________________________________________

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



# (2)The RENAMING VARIABLES Function - delete
# clean_census_cols <- function(data, prefix_to_remove, prefix_to_add, suffix_to_remove = "_2023$") {
#   
#   # Ensure pattern matches from the beginning of the string
#   clean_prefix_pattern <- paste0("^", prefix_to_remove)
#   
#   data %>%
#     rename_with(
#       ~ .x %>% 
#         str_remove(clean_prefix_pattern) %>% 
#         str_remove(suffix_to_remove) %>% 
#         paste0(prefix_to_add, .),
#       .cols = starts_with(prefix_to_remove)
#     )
# }


#_______________________________________________________________________________
# RUNNING THE FUNCTIONS - INSPECTION
#_______________________________________________________________________________

# (1)RUNNING THE INSPECTION FUNCTION 
#prints the count & names to the console, and saves the columns to var names

# Home Ownership  
ho_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "individual_home_ownership_")

#  Usual residence 1 year ago indicator 
res1_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "usual_residence_1_year_ago_indicator_")

# Usual residence 5 years ago indicator 
res5_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "usual_residence_5_years_ago_indicator_")

# years at usual residence   
yrs_res_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "years_at_usual_residence_")

# Years since arrival in New Zealand 
arrival_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "years_since_arrival_in_new_zealand_")

# Study participation 
study_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "study_participation_")

# Highest qualification 
highest_qual_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "highest_qualification_")

# Post school qualification in New Zealand indicator 
post_school_nz_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "post_school_qualification_in_new_zealand_indicator_")

#. Highest secondary school qualification 
sec_qual_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "highest_secondary_school_qualification_")

# Post school qualification level of attainment 
attainment_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "post_school_qualification_level_of_attainment_")

# Sources of personal income (total responses) 
inc_sources_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "sources_of_personal_income_total_responses_")

# Total personal income 
total_inc_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "total_personal_income_")

# Work and labour force status 
labour_status_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "work_and_labour_force_status_")

# Job search methods (total responses) 
job_search_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "job_search_methods_total_responses_")

# Status in employment 
emp_status_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "status_in_employment_")

# Unpaid activities (total responses) 
unpaid_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "unpaid_activities_total_responses_")

# Hours worked in employment per week 
hours_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "hours_worked_in_employment_per_week_")

# Industry by usual residence address 
ind_res_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "industry_by_usual_residence_address_")

# Industry by workplace address 
ind_work_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "industry_by_workplace_address_")

# Occupation by usual residence address 
occ_res_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "occupation_by_usual_residence_address_")

# Occupation by workplace address 
occ_work_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "occupation_by_workplace_address_")

# Main means of travel to work
transport_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "transportMode_")

# Sector of ownership 
sector_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "sector_of_ownership_")

# (24) Individual unit data source 
source_vars <- home_to_work_clean %>%
  variable_name_inspect(str_detection = "individual_unit_data_source_")