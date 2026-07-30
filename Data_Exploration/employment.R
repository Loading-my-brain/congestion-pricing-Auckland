
library(spdep)
library(sf)
library(dplyr)
library(tmap)

#The Theory #1:  
#Commuters working long hours (40+ or 50+ hours per week) face severe "time poverty." 
# They simply do not have the time to walk, cycle, or wait for a bus. 
# They are highly motivated to drive because driving is often the fastest, 
# most direct option, even in heavy congestion.


# 13. Work and labour force status columns
labour_status_vars <- travel_HO_income %>%
  variable_name_inspect(str_detection = "work_and_labour_force_status_")

# 15. Status in employment columns
emp_status_vars <- travel_HO_income %>%
  variable_name_inspect(str_detection = "status_in_employment_")

# 16. Unpaid activities (total responses) columns
unpaid_vars <- travel_HO_income %>%
  variable_name_inspect(str_detection = "unpaid_activities_total_responses_")

# 17. Hours worked in employment per week columns
hours_vars <- travel_HO_income %>%
  variable_name_inspect(str_detection = "hours_worked_in_employment_per_week_")


# 19. Industry by workplace address columns
ind_work_vars <- travel_HO_income %>%
  variable_name_inspect(str_detection = "industry_by_workplace_address_")

# 20. Occupation by usual residence address columns
occ_res_vars <- travel_HO_income %>%
  variable_name_inspect(str_detection = "occupation_by_usual_residence_address_")

# 21. Occupation by workplace address columns
occ_work_vars <- travel_HO_income %>%
  variable_name_inspect(str_detection = "occupation_by_workplace_address_")




# Add employment metrics to your master dataset
travel_income_HO_emp <- travel_HO_income %>%
  mutate(
    #  Full-Time Employment Rate (Share of working-age population) 
    # - proportion of the total usually resident working-age population (aged 15+) actively engaged in those labor categories
    prop_emp_full_time = 
      work_and_labour_force_status_employed_full_time_2023 / 
      work_and_labour_force_status_total_stated_2023,
    
    #  Part-Time Employment Rate (Share of working-age population)
    prop_emp_part_time = 
      work_and_labour_force_status_employed_part_time_2023 / 
      work_and_labour_force_status_total_stated_2023,
    
    # Calculate a standardized unemployment proportion using the same denominator
    prop_unemployed_stated = 
      work_and_labour_force_status_unemployed_2023 / 
      work_and_labour_force_status_total_stated_2023,
    
    # 3. Standard Unemployment Rate (Calculated using the active Labour Force as the denominator)
    # Labor Force = Employed (FT + PT) + Unemployed
    #'Unemployment Rate' was calculated using the economically active labor force 
    #'(Employed + Unemployed) as the denominator, directly aligning with international 
    #'International Labour Organization (ILO) standards and the Ministry of Social Development’s 
    #'(MSD) policy frameworks
    prop_unemployment_rate = 
      work_and_labour_force_status_unemployed_2023 / (
        work_and_labour_force_status_employed_full_time_2023 + 
          work_and_labour_force_status_employed_part_time_2023 + 
          work_and_labour_force_status_unemployed_2023  
        #Stats NZ defines an unemployed person within the work and 
        # labour force status variable as someone aged 15 or older who has no paid job, 
        # is available to work, and has actively looked for work or has a new job starting soon
      )
  )







#________________________________________________________________________________
# I WANT TO COMPARE THESE 3 EMPLOYEMENT STATUS GROUPS - HOW ARE THEY DIFFERENT? 
#________________________________________________________________________________

# Reshape the data to long format
labour_anova_data <- travel_HO_income %>%
  st_drop_geometry() %>%
  select(
    usual_residence_statistical_area_2_name_na,
    prop_emp_full_time,
    prop_emp_part_time,
    prop_unemployed_stated
  ) %>%
  pivot_longer(
    cols = c(prop_emp_full_time, prop_emp_part_time, prop_unemployed_stated),
    names_to = "Labor_Status",
    values_to = "Share"
  ) %>%
  mutate(
    Labor_Status = factor(
      Labor_Status, 
      levels = c("prop_emp_full_time", "prop_emp_part_time", "prop_unemployed_stated"),
      labels = c("Full-Time", "Part-Time", "Unemployed")
    ),
    usual_residence_statistical_area_2_name_na = factor(usual_residence_statistical_area_2_name_na)
  )

#  Run the Repeated Measures ANOVA
labour_anova <- aov(
  Share ~ Labor_Status + Error(usual_residence_statistical_area_2_name_na/Labor_Status), 
  data = labour_anova_data
)

summary(labour_anova)

#_________________________________________________________________
# Run the pairwise paired t-tests with Bonferroni correction
# labour_posthoc <- pairwise.t.test(
#   x = labour_anova_data$Share, 
#   g = labour_anova_data$Labor_Status, 
#   paired = TRUE, 
#   p.adjust.method = "bonferroni"
# )
# print(labour_posthoc)


