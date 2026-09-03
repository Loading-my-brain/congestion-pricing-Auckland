


# Poportions of Income for GWR:

colnames(travel_income_HO_emp)[str_detect(colnames(travel_income_HO_emp), "income")]
# While median personal income provides a useful baseline measure of regional wealth,
# my research will use something like the "Low-Income Earner Share" (prop_income_low_earners) as 
# its primary predictor of economic vulnerability. In transport equity research,
# neighborhood medians can mask localized pockets of poverty, as they do not capture 
# the distribution or absolute size of the low-income population. 
# 
# By aggregating the census counts of 
# residents earning $30,000 or less and dividing by the total stated population,
# this metric directly measures the concentration of financially vulnerable residents 
# (such as low-wage workers, students, and welfare beneficiaries) within each SA2.Additionally,
# formatting this variable as a proportion ensures mathematical 
# consistency with the dependent transit variables in the (GWR) model.


#How did I choose this threshold? Justfications?
#https://www.stats.govt.nz/infographics/aotearoa-new-zealand-as-a-village-of-100-people/#:~:text=and%20drivers%3A%203-,Personal%20income,-The%20annual%20incomes



travel_income_HO_emp_time <- travel_income_HO_emp_time %>%
  mutate(
    # 1. Low Income Share ($30,000 or less)
    prop_income_low = (
      total_personal_income_10_000_or_less_2023 + 
        total_personal_income_10_001_20_000_2023 + 
        total_personal_income_20_001_30_000_2023
    ) / total_personal_income_total_stated_2023,
    
    # 2. Middle Income Share ($30,001 to $70,000)
    prop_income_middle = (
      total_personal_income_30_001_50_000_2023 + 
        total_personal_income_50_001_70_000_2023
    ) / total_personal_income_total_stated_2023,
    
    # 3. Upper-Middle Income Share ($70,001 to $100,000)
    prop_income_upper_middle = 
      total_personal_income_70_001_100_000_2023 / 
      total_personal_income_total_stated_2023,
    
    # 4. High Income Share ($100,001 or more)
    prop_income_high = 
      total_personal_income_100_001_or_more_2023 / 
      total_personal_income_total_stated_2023
  ) #I have calcualted for the other income tiers becyase my Tukey test from my previous analysis showed that there were 
# some differences between low vs high, middle vs low and .. etc income groups.Hence, I might later compare GWR models using low and then for high, and then middle? 
# does that make seense? idk 




#Select our variables of our interest for the GWR component


# Create the slimmed-down, analysis-ready dataset
gwr_ready_dataset <- travel_income_HO_emp_time %>%
  # Select only the essential columns for my analysis
  select(
    # Geographic Identifiers & Area Controls
    usual_residence_statistical_area_2_code_na,
    usual_residence_statistical_area_2_name_na,
    LAND_AREA_, # Land area in sq km
    AREA_SQ_KM,
    
    # Travel Mode Shares (Proportions)
    starts_with("prop_aggregate_"),
    
    # Travel Mode Densities (Volume proxies)
    density_private_car,
    density_public_transport,
    density_active_modes,
    density_work_at_home,
    density_other,
    
    # Housing Variables
    prop_homeownership_owners,
    prop_homeownership_renters,
    density_homeownership_owners,
    density_homeownership_renters,
    
    # Income Variables
    total_personal_income_median_2023,
    income_tier, # my quartile groups
    prop_income_low, 
    prop_income_middle, 
    prop_income_upper_middle,
    prop_income_high,

    
    # Employment/Labor Variables
    prop_emp_full_time,
    prop_emp_part_time,
    prop_unemployed_stated,
    prop_unemployment_rate,
    
    # Accessibility Variables (GTFS Travel Times)
    transit_time_cbd_mins,
    
    # Keeps spatial geometry column automatically
    geom
  )

glimpse(gwr_ready_dataset)


# Save this clean master file
# sf::st_write(
#   gwr_ready_dataset,
#   dsn = "./data/Saved/gwr_ready_dataset.gpkg",
#   layer = "gwr_ready_dataset",
#   delete_dsn = TRUE # Overwrites any previous version
# )



