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
library(stringr)
library(car)
library(dplyr)

#Load Data (created from income2-poportions.R !) - I had to prepare the dataset, so that it was GWR ready. 
gwr_ready_dataset<-st_read("./data/Saved/gwr_ready_dataset.gpkg")


gwr_analysis_sf <- gwr_ready_dataset %>%
  select(
    usual_residence_statistical_area_2_name_na,
    prop_aggregate_public_transport,  # Dependent Variable (Y)
    prop_income_low,                  
    prop_homeownership_renters,  
    prop_emp_full_time,
    transit_time_cbd_mins,           
    geom
  ) %>%
  na.omit() # GWR will crash if any NAs exist in my target columns!?

# Run the global OLS model
ols_model <- lm(
  prop_aggregate_public_transport ~ prop_income_low + prop_homeownership_renters + transit_time_cbd_mins + prop_emp_full_time, 
  data = gwr_analysis_sf
)
summary(ols_model)

# Check the VIF (must be below 3, and strictly below 5)
vif(ols_model)



# Run the revised OLS model (without full-time employment)
ols_model_revised <- lm(
  prop_aggregate_public_transport ~ prop_income_low + prop_homeownership_renters + transit_time_cbd_mins, 
  data = gwr_analysis_sf
)
summary(ols_model_revised)
vif(ols_model_revised)#looks great



# Convert to the 'sp' class object required by GWmodel
gwr_dataset_sp <- as(gwr_analysis_sf, "Spatial")

# Calculate the optimal adaptive bandwidth (number of nearest neighbors)
optimized_bandwidth <- bw.gwr(
  formula = prop_aggregate_public_transport ~ prop_income_low + prop_homeownership_renters + transit_time_cbd_mins,
  data = gwr_dataset_sp,
  approach = "AICc",      
  kernel = "bisquare",    
  adaptive = TRUE         
)

# Run the Basic GWR Model
gwr_results <- gwr.basic(
  formula = prop_aggregate_public_transport ~ prop_income_low + prop_homeownership_renters + transit_time_cbd_mins,
  data = gwr_dataset_sp,
  bw = optimized_bandwidth,
  kernel = "bisquare",
  adaptive = TRUE
)

print(gwr_results)



