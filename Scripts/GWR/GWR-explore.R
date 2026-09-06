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
library(car) 
library(s20x)
library(interactions)

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


# TO look into each plot - EDA ----------------
plot(prop_aggregate_public_transport ~ prop_income_low + prop_homeownership_renters + transit_time_cbd_mins + prop_emp_full_time, 
     data = gwr_analysis_sf)

# Run the global OLS model
ols_model <- lm(
  prop_aggregate_public_transport ~ prop_income_low + prop_homeownership_renters + transit_time_cbd_mins + prop_emp_full_time, 
  data = gwr_analysis_sf
)


# Check the VIF (must be below 3, and strictly below 5)
vif(ols_model)
cor(gwr_analysis_sf$prop_income_low, gwr_analysis_sf$prop_emp_full_time, use = "complete.obs")
#Some observations: 
# there's a negative coorelation between, -0.8010405 -> drop one of the terms. 
# we drop prop_emp_full_time becuase income is more of my interest as it was in the policy documents.


#Dropping the terms
ols_model_dropped_emp <- lm(
  prop_aggregate_public_transport ~ prop_income_low + prop_homeownership_renters + transit_time_cbd_mins, 
  data = gwr_analysis_sf
)

#  STANDARD MODEL CHECKS - after dropping the employement term.
summary(ols_model_dropped_emp)

modelcheck(ols_model_dropped_emp)
summary(ols_model_dropped_emp$residuals)

#simple way of examining is just to plot the standardised residuals in order:
s.resids <- rstudent(ols_model_dropped_emp)
plot(s.resids,type='h', main="Studentized Residuals for OLS Model")
abline(h=c(-2,2),col='lightskyblue')
qqPlot(ols_model_dropped_emp)# There are 2 points in the very extreme end that may be a signficnt outlier
# Note in the R command: 
# 344 458   <- These are the ROW NAMES (the character labels of the outliers)
# 335 447   <- These are the physical row numbers in my dataset - Becuase my  dataset (gwr_analysis_sf) has had some rows removed (via na.omit), the row names and row numbers no longer match.


# investigating the outliers
as.data.frame(gwr_analysis_sf[c(335, 447), ])#These are valid outliers!


#From the tutorual page ...
gwr_analysis_NoOut = gwr_analysis_sf %>% filter(abs(rstudent(ols_model_dropped_emp)) <= 2)
ols_model_dropped_emp2 <- lm(prop_aggregate_public_transport ~ prop_income_low + 
    prop_homeownership_renters + transit_time_cbd_mins, data = gwr_analysis_NoOut)

#Look at the summary
summary(ols_model_dropped_emp2) #Note that I don't think it's apporpriate to delete these observations; these outliers are valid!


# Check for non-linearity (using crPlots). If a variable curves, consider adding a non-linear term (like a log or squared term).
crPlots(ols_model_dropped_emp) # well income is non-linear 

# Running OLS with an interaction term
ols_interaction <- lm(
  prop_aggregate_public_transport ~ prop_income_low * transit_time_cbd_mins + prop_homeownership_renters, 
  data = gwr_analysis_sf
)

#Quick Summary check - (R^2 = improved, P-vals=see sig, interaction effect=yes!)
summary(ols_interaction)

# To Visualize interaction
library(interactions)
interact_plot(
  ols_interaction, 
  pred = transit_time_cbd_mins, 
  modx = prop_income_low, 
  plot.points = T # to see the underlying data points
)# yes there's an interaction  term 

#We can't interpret the coefficents just yet - we need to do some model checks
modelcheck(ols_interaction)

#Investigating the influential point - from cooks distance 
as.data.frame(gwr_analysis_sf[c(154), ])


par(mfrow=c(3,1))
plot(ols_interaction, which = 1)# Residuals vs Fitted (Checks for Linearity and Homoscedasticity)
plot(ols_interaction, which = 2)# Normal Q-Q Plot (Checks for Normality of Errors)
plot(ols_interaction, which = 3)# Scale-Location Plot (Checks for constant variance / Homoscedasticity)

vif(ols_interaction) #This is called structural multicollinearity.
# Because the interaction term prop_income_low:transit_time_cbd_mins is literally 
# made by multiplying my income variable by my travel time variable, it is mathematically 
# guaranteed to be highly correlated with both of them.



















#Exploring the beta regression model - will not include in my thesis, but this was fun to explore. 
# library(betareg)
# 
# # Beta regression naturally models proportions between 0 and 1, but not eaxtly 0 and 1 
# model_beta <- betareg(
#   I(prop_aggregate_public_transport) ~ prop_income_low + 
#     prop_homeownership_renters + transit_time_cbd_mins , 
#   data = gwr_analysis_sf
# )
# 
# summary(model_beta)














#_______________________________________________________________________________
# ________________GWR ______Geographically Weighted Regression _________________
#_______________________________________________________________________________

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



