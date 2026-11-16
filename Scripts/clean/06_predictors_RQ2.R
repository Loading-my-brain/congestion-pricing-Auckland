# =============================================================================
# 06_predictors_RQ2.R
# -----------------------------------------------------------------------------
# PURPOSE  RQ2 set-up: create the candidate predictor variables, check
#          unusual SA2s, build the regression dataset (490 SA2s), and run the
#          Spearman correlation matrix used to choose the final predictors.
#
# INPUT    data/Saved/travel_props_Times.gpkg   (from 05)
# OUTPUT   data/Saved/travel_props_vars.gpkg    (497 SA2s, all predictors - maps)
#          data/Saved/model_data.gpkg           (490 SA2s - OLS / GWR)
#          all_figures/fig_corr_matrix.png
#
# NEXT     07_ols_gwr_RQ2.R
# =============================================================================

library(sf)
library(tidyverse)

travel_props_Times <- st_read("./data/Saved/travel_props_Times.gpkg", quiet = TRUE)



#Candidate predictors (all 2023 Census, by usual residence)
# mutate() only ADDS columns, so it is safe to keep the same object name.
travel_props_Times <- travel_props_Times %>%
  mutate(
    # INCOME
    # (median personal income already exists; Wiri West set to NA in 05)
    # Low-income share: adults (15+) with personal income <= $30,000
    # (the lowest three Census income bands). Captures concentrations of
    # low-income residents that a median can hide. NB: includes students and
    # retirees, so it partly reflects age, not only poverty (Limitations).
    prop_income_low = (total_personal_income_10_000_or_less_2023 +
                         total_personal_income_10_001_20_000_2023 +
                         total_personal_income_20_001_30_000_2023) /
      total_personal_income_total_stated_2023,
    
    # HOUSIN
    # Individual home ownership, adults 15+ (own / partly own / family trust).
    # NOT "renters": non-owners also include adult children living with
    # parents, flatmates, boarders. Only ONE of owner / non-owner can be used
    # (they add up to 100%).
    prop_home_owner = (individual_home_ownership_own_or_partly_own_2023 +
                         individual_home_ownership_hold_in_a_family_trust_2023) /
      individual_home_ownership_total_stated_2023,

    # EMPLOYMENT
    # Share of adults (15+) employed full-time
    prop_emp_full_time = work_and_labour_force_status_employed_full_time_2023 /
      work_and_labour_force_status_total_stated_2023,
    
    # Unemployment rate (ILO definition: unemployed / labour force)
    prop_unemployment_rate = work_and_labour_force_status_unemployed_2023 /
      (work_and_labour_force_status_employed_full_time_2023 +
         work_and_labour_force_status_employed_part_time_2023 +
         work_and_labour_force_status_unemployed_2023),
    
    # Long hours ("time poverty" - cite a source if used): employed people
    # working 50+ hours per week
    prop_hours_50plus = (hours_worked_in_employment_per_week_50_59_hours_worked_2023 +
                           hours_worked_in_employment_per_week_60_hours_or_more_worked_2023) /
      hours_worked_in_employment_per_week_total_stated_2023,
    
    # Blue-collar occupations (by usual RESIDENCE, not workplace): tools,
    # shifts and job sites make these jobs harder to reach by PT
    prop_blue_collar = (occupation_by_usual_residence_address_technicians_and_trades_workers_2023 +
                          occupation_by_usual_residence_address_machinery_operators_and_drivers_2023 +
                          occupation_by_usual_residence_address_labourers_2023) /
      occupation_by_usual_residence_address_total_stated_2023,
    
    # OUTCOME (robustness only
    # Car share among those who TRAVELLED to work (WFH removed from denominator).
    # Main outcome stays prop_aggregate_private_car (all workers, incl. WFH).
    prop_car_commuters = (transportMode_drive_a_private_car_truck_or_van +
                            transportMode_drive_a_company_car_truck_or_van) /
      (transportMode_total_stated - transportMode_work_at_home),
    
    # TRAVEL TIME
    # How many times longer PT takes than driving (travel TIMES, not mode shares)
    pt_car_time_ratio = PT_transit_time_cbd_mins / car_travel_time_cbd_mins
  )

# NOT used (and why) - so you can explain choices to your supervisor:
#  - Owner AND non-owner shares together: they are complements (sum to 100%)
#  - Owner / renter DENSITIES: mostly measure population density, not tenure
#  - Middle / upper / high income shares: complements of the low-income share;
#    running a separate GWR per income band would be "fishing"
#  - Industry / occupation by WORKPLACE: describe jobs located in an SA2, not
#    the people who live there
#  - Vehicle access: household variable, not in this (individual) Census table;
#    also decided jointly with car commuting -> Limitations



# Descriptive summary of the candidates
cand <- c("total_personal_income_median_2023", "prop_income_low", "prop_home_owner",
          "prop_emp_full_time", "prop_unemployment_rate", "prop_hours_50plus",
          "prop_blue_collar", "PT_transit_time_cbd_mins", "car_travel_time_cbd_mins",
          "pt_car_time_ratio")
summary(st_drop_geometry(travel_props_Times)[, cand])
# Income median $44,700 ($13,000-$80,500); home ownership 41% (7-70%);
# full-time 54% (13-76%); unemployment 4.4% (0-29%); 50+ hrs 14% (8-26%);
# blue-collar 24% (6-51%).
# PT/car time ratio (r5r free-flow car): median 3.27, IQR 2.91-3.67,
# range 1.50-5.13 -> PT takes ~3x as long as driving on clear roads.
# (With TravelTime peak times: median 2.27, range 1.51-4.61.)
# Free-flow car times are a lower bound, so these ratios are an UPPER bound.
# PT is never as fast as driving; no SA2 below 1.5x (minimum = 1.50 exactly)

# Unusual SA2s - check they are real places, not data problems
travel_props_Times %>%
  st_drop_geometry() %>%
  filter(prop_emp_full_time < 0.3 | prop_unemployment_rate > 0.15 |
           total_personal_income_median_2023 < 15000) %>%
  select(name = usual_residence_statistical_area_2_ascii_name_na,
         total_personal_income_median_2023, prop_emp_full_time, prop_unemployment_rate,
         n_commuters = transportMode_total_stated)
# - Symonds Street East ($13,000), Akoranga: student areas (universities) -> keep
# - Papakura Eastburn (16% unemployed): genuinely deprived -> keep
# - Henderson Lincoln East (13% full-time, 0% unemployed, 51 commuters): likely
#   a retirement area -> keep, but watch for a large residual in OLS / GWR
# - Wiri West: prison population -> excluded (income NA, see 05)


#  Supporting: car share by income quartile
car_income <- travel_props_Times %>%
  st_drop_geometry() %>%
  filter(!is.na(total_personal_income_median_2023)) %>%
  mutate(income_tier = cut(total_personal_income_median_2023,
                           breaks = quantile(total_personal_income_median_2023,
                                             probs = seq(0, 1, 0.25)),
                           include.lowest = TRUE,
                           labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")))

car_income %>% group_by(income_tier) %>%
  summarise(n = n(), median_car_share = median(prop_aggregate_private_car))

pc_anova <- aov(prop_aggregate_private_car ~ income_tier, data = car_income)
summary(pc_anova)
TukeyHSD(pc_anova)
# Result (Wiri West excluded): F(3, 492) = 25.6, p < 0.001, eta^2 = 0.14.
# Median car share: Q1 73.5%, Q2 70.5%, Q3 66.5%, Q4 58.7%.
# Q1 = Q2; Q3 ~4.7 points lower than Q1/Q2; Q4 ~11 points lower than Q1/Q2
# and 6.3 lower than Q3.
# -> Higher-income SA2s drive LESS. Possible reasons: more central (better PT,
#    shorter trips) and more working from home - checked below.

# Is the lower car share in rich areas partly working from home?
# (WFH is in the denominator of car share)
cor.test(car_income$total_personal_income_median_2023,
         car_income$prop_aggregate_work_at_home,
         method = "spearman", exact = FALSE)
# rho = 0.72, p < 0.001 -> strongly linked. Part of "rich areas drive less" is
# "rich areas commute less". Checked with prop_car_commuters in 07 (section 12c).


# Save the full dataset (497 SA2s) - for maps and descriptives
st_write(travel_props_Times, "./data/Saved/travel_props_vars.gpkg",
         delete_dsn = TRUE, quiet = TRUE)


# Regression dataset: 490 SA2s with complete data
# filter() REMOVES rows -> always a NEW object.
# Excluded: 6 SA2s with no PT stop within a 30-min walk + Wiri West (income NA)
model_data <- travel_props_Times %>%
  filter(!is.na(PT_transit_time_cbd_mins),
         !is.na(total_personal_income_median_2023)) %>%
  select(sa2_code = usual_residence_statistical_area_2_code_na,
         sa2_name = usual_residence_statistical_area_2_ascii_name_na,
         prop_aggregate_private_car,# MAIN OUTCOME
         prop_car_commuters, prop_aggregate_work_at_home, # robustness (12c in 07)
         all_of(cand))# candidate predictors (+ geometry)

nrow(model_data)# 490
colSums(is.na(st_drop_geometry(model_data)))# all 0

# Robustness outcome should be a bit HIGHER than the main one (smaller denominator)
summary(model_data$prop_car_commuters)

st_write(model_data, "./data/Saved/model_data.gpkg", delete_dsn = TRUE, quiet = TRUE)

# Methods sentence:
# "Of the 497 SA2s, seven were excluded from the regression analysis: six with no
#  PT stop within a 30-minute walk and Wiri West, whose population is
#  predominantly institutional. The models were estimated on 490 SA2s."


#  Spearman correlation matrix (outcome + all candidates)
corr_vars <- model_data %>%
  st_drop_geometry() %>%
  transmute(`Car share`          = prop_aggregate_private_car,
            `Median income`      = total_personal_income_median_2023,
            `Low-income share`   = prop_income_low,
            `Home ownership`     = prop_home_owner,
            `Full-time employed` = prop_emp_full_time,
            `Unemployment rate`  = prop_unemployment_rate,
            `Works 50+ hrs`      = prop_hours_50plus,
            `Blue-collar`        = prop_blue_collar,
            `PT time`            = PT_transit_time_cbd_mins,
            `Car time`           = car_travel_time_cbd_mins,
            `PT/car time ratio`  = pt_car_time_ratio)

corr_mat <- cor(corr_vars, method = "spearman")
round(corr_mat, 2)
# With car share: PT time 0.81, blue-collar 0.80, car time 0.69 (r5r free-flow),
#   median income -0.51, low-income 0.46, unemployment 0.44,
#   PT/car ratio 0.25 (expected direction but weak -> not used as a predictor).
# Too much overlap (|rho| > 0.7): median income vs low-income -0.98 (keep median);
#   PT time vs car time 0.84 (keep PT time); unemployment vs home ownership -0.78,


corr_long <- as.data.frame(as.table(corr_mat)) %>%
  rename(var1 = Var1, var2 = Var2, rho = Freq) %>%
  filter(as.integer(var1) > as.integer(var2))   # lower triangle only

(p_corr <- ggplot(corr_long, aes(x = var2, y = var1, fill = rho)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = sprintf("%.2f", rho)), size = 2.8) +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",   # colour-blind safe
                         midpoint = 0, limits = c(-1, 1), name = "Spearman \u03c1") +
    scale_y_discrete(limits = rev) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank()))

ggsave("all_figures/fig_corr_matrix.png", p_corr,
       width = 19, height = 16, units = "cm", dpi = 300, bg = "white")