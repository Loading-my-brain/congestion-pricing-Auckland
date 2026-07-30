library(spdep)
library(sf)
library(tmap)

home_ownership_analysis

sum(str_detect(colnames(home_ownership_analysis), "income")) #Inspecting the num of cols of travel groups
income_inspect = home_ownership_analysis[,which(str_detect(colnames(home_ownership_analysis), "income"))]
str(income_inspect) #What these are....
colnames(income_inspect)




# EXPLAINING - how I defined the 4 income groups for my research --------------
# At first, I had 3 quantiles I used ntile to evenly group the median income vals 
# in the data into 3 groups; low, medium and high.
# But then this wasn't a justifiable method, which was pointed out by my supervisor. 
# Hence, I then moved onto using5 quantiles; Lowst quintile, second quintile, 
# Third quintile, Fourth quintile and Highest quintile as this was used in 
# Stats NZ. But for the sake of peer-reviewed academic literature, 
# I will use the percentiles to explain as this would be easier to explain;  
# I can explicitly state the exact dollar amounts used to define theeconomic tier 
# (e.g., "Low-income neighborhoods were defined as those with median incomes below
# $38,950, which represents the 25th percentile of the regional Auckland dataset").
#  -------------- -------------- -------------- -------------- --------------


# Calculate the exact dollar thresholds for your 4 quartiles (0%, 25%, 50%, 75%, 100%)
income_breaks <- quantile(
  home_ownership_analysis$total_personal_income_median_2023, 
  probs = seq(0, 1, 0.25), #(0%, 25%, 50%, 75%, 100%)
  na.rm = TRUE
)
print(income_breaks)# View the exact dollar thresholds in my console (e.g., Q1 cap, Median, Q3 cap)

# Cut the neighborhoods into groups using those exact dollar-value boundaries
travel_HO_income <- home_ownership_analysis %>%
  filter(!is.na(total_personal_income_median_2023)) %>%
  mutate(
    income_tier = cut(
      total_personal_income_median_2023, 
      breaks = income_breaks, 
      include.lowest = TRUE, # Ensures the minimum value is included in Q1
      labels = c("Q1:Low Income", "Q2:Lower Middle", "Q3:Upper Middle", "Q4:High Income")
    )
  )


#  Run an ANOVA: Does Private Car Share differ significantly between these 5 tiers?
pc_anova <- aov(prop_aggregate_private_car ~ income_tier, data = travel_HO_income)
summary(pc_anova)# Yes it does!

# Run the Post-Hoc Tukey HSD to identify where the significant differences lie
TukeyHSD(pc_anova)











#______________________________________________________________________
#  I want to identify  where my income groups lie spatially
#______________________________________________________________________
# Remove any missing values to ensure the spatial matrix can calculate
income_spatial <- travel_HO_income %>%
  filter(!is.na(total_personal_income_median_2023))

#Define spatial neighbors (Queen contiguity - sharing a boundary or vertex)
neighbors <- poly2nb(income_spatial, queen = TRUE)

neighbors_self <- include.self(neighbors)#For Getis-Ord Gi*, we must include each neighborhood as its own neighbor
weights_list <- nb2listw(neighbors_self, style = "B")#Convert neighbors to a spatial weights list (using binary style "B")

# Calculate the Getis-Ord Gi* statistic (generates Z-scores)
gi_z_scores <- localG(income_spatial$total_personal_income_median_2023, weights_list)

#Bind the Z-scores back to the spatial dataset
income_spatial$gi_z_score <- as.numeric(gi_z_scores)

# 7. Classify Z-scores into Hot Spots, Cold Spots, and Not Significant
# A Z-score greater than 1.96 = a Hot Spot at the 95% confidence level
# A Z-score less than -1.96 = a Cold Spot at the 95% confidence level  

income_spatial <- income_spatial %>%
  mutate(
    hot_spot_class = case_when(
      gi_z_score >= 1.96  ~ "Hot Spot (High Wealth Cluster)",
      gi_z_score <= -1.96 ~ "Cold Spot (Low Wealth Cluster)",
      TRUE                ~ "Not Statistically Significant"
    ), 
    hot_spot_class = as.factor(hot_spot_class))
  

# Plot the Statistical Hot Spot Map
tmap_mode("view")
wealth_clusters<- tm_shape(income_spatial) +
  tm_polygons("hot_spot_class",
              palette = c("blue", "red", "lightgrey"), # Blue for cold, Grey for neutral, Red for hot
              title = "Getis-Ord Gi* Wealth Clusters",
              id = "usual_residence_statistical_area_2_name_na",
              popup.vars = c(
                "Z-Score" = "gi_z_score",
                "Median Income ($)" = "total_personal_income_median_2023"
              )) +
  tm_view(
    basemaps = c("Esri.WorldGrayCanvas", "OpenStreetMap"),
    set.zoom.limits = c(9, 14)
  )


# tmap_save(wealth_clusters, filename = "./Data_Exploration/all_figures/wealth_clusters.html")
