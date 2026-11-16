# =============================================================================
# 05_travel_times.R
# -----------------------------------------------------------------------------
# PURPOSE  Join PT and car travel times to the 497 SA2s, describe them, map
#          them, and compare them (RQ2 predictor description). Supporting
#          results: do travel times differ by income quartile? (also used in
#          RQ3.2 as context)
#
# INPUT    data/Saved/travel_props.gpkg                (from 02/03)
#          data/pt_time_cbd_r5r_20260513_0800.csv      (from 04a, Part A)
#          data/car_time_cbd_r5r_freeflow_inbound.csv  (from 04c; r5r free-flow car)

# OUTPUT   data/Saved/travel_props_Times.gpkg          (497 SA2s + travel times)
#          all_figures/fig_car_pt_time.png             (Results: 1x2 map)
#          all_figures/fig_time_by_income_pt_car.png   (supporting / RQ3.2)
#          all_figures/fig_appendix_time_hist.png      (Appendix)
#
# NEXT     06_predictors_RQ2.R
# =============================================================================

library(sf)
library(tidyverse)
library(tmap)


#  Join travel times (matched by SA2 code, stored as text)
 travel_props <- st_read("./data/Saved/travel_props.gpkg", quiet = TRUE) %>%
  mutate(usual_residence_statistical_area_2_code_na =
           as.character(usual_residence_statistical_area_2_code_na),
         lisa_car = factor(lisa_car, levels = c("High-High", "Low-Low", "High-Low",
                                                "Low-High", "Not significant")))

pt_times <- read.csv("data/pt_time_cbd_r5r_20260513_0800.csv",
                     colClasses = c("character", "numeric", "numeric")) %>%
  rename(usual_residence_statistical_area_2_code_na = sa2_code,
         PT_transit_time_cbd_mins = pt_time_cbd_mins,
         PT_time_sensitivity      = pt_time_cbd_mins_walk90)

# CAR TIMES: r5r free-flow (no traffic), SA2 -> CBD, same OSM network as PT.
# Used instead of the TravelTime API times because the TravelTime licence for
# publication could not be confirmed. Open data (OpenStreetMap, ODbL) + open
# software (r5r), so the results are fully reproducible.
# NB: free-flow = no congestion -> times are a LOWER BOUND for 8 am driving
#     (Limitations). Values are whole minutes.
# r5r free-flow car times (SA2 -> CBD), from 04c Part A
car_times <- read.csv("data/car_time_cbd_r5r_freeflow_inbound.csv",
                      colClasses = c("character", "numeric")) %>%
  rename(usual_residence_statistical_area_2_code_na = SA22023_V1_00,
         car_travel_time_cbd_mins = car_time_r5r_inbound)

travel_props_Times <- travel_props %>%
  left_join(pt_times,  by = "usual_residence_statistical_area_2_code_na") %>%
  left_join(car_times, by = "usual_residence_statistical_area_2_code_na")

nrow(travel_props_Times)                                  # 497
sum(is.na(travel_props_Times$PT_transit_time_cbd_mins))   # 6: no stop within a 30-min walk
sum(is.na(travel_props_Times$car_travel_time_cbd_mins))   # 0

#   Data fix: Wiri West income 
# Median personal income = $0 with 1,335 people stating income: more than half
# report no income. Wiri West contains Auckland South Corrections Facility, so
# the population is mainly institutional, not a residential neighbourhood.
# -> Income set to NA: excluded from all income analyses and the regression.
#    (Kept on maps and in RQ1 mode-share results.)

# Median personal income = $0. 882 of 1,335 people (66%) have income <= $10,000,
# and 615 (46%) report no source of income.
travel_props_Times <- travel_props_Times %>%
  mutate(total_personal_income_median_2023 =
           if_else(usual_residence_statistical_area_2_ascii_name_na == "Wiri West",
                   NA_real_, as.numeric(total_personal_income_median_2023)))


#   Descriptive summary
summary(travel_props_Times$PT_transit_time_cbd_mins)
# median 66, IQR 50-80, range 4-151 (n = 491)
summary(travel_props_Times$car_travel_time_cbd_mins)
# r5r free-flow: median 20, IQR 16-24, range 2-39 (n = 497)
# (Previous TravelTime 8 am times: median 29.4 - congestion adds ~45%.)
# -> A typical PT commute takes about three times as long as driving on clear roads.

# The 6 SA2s with no PT time
travel_props_Times %>% st_drop_geometry() %>%
  filter(is.na(PT_transit_time_cbd_mins)) %>%
  select(usual_residence_statistical_area_2_ascii_name_na, PT_time_sensitivity)
# Middlemore, Harbour View Heights, Bremner Ridge, Tuscany Heights,
# Ormiston South East, Massey Red Hills (mostly newer fringe housing)

# Appendix: distributions of both travel times
(p_time_hist <- travel_props_Times %>% st_drop_geometry() %>%
  select(`Public transport` = PT_transit_time_cbd_mins, Car = car_travel_time_cbd_mins) %>%
  pivot_longer(everything(), names_to = "Mode", values_to = "mins") %>%
  filter(!is.na(mins)) %>%
  mutate(Mode = factor(Mode, levels = c("Public transport", "Car"))) %>%
  ggplot(aes(mins)) +
  geom_histogram(bins = 30, fill = "grey60", colour = "white") +
  facet_wrap(~ Mode, scales = "free_x") +
  labs(x = "Travel time to CBD (minutes)", y = "Number of SA2s") +
  theme_minimal())
ggsave("all_figures/fig_appendix_time_hist.png", p_time_hist,
       width = 16, height = 8, units = "cm", dpi = 300, bg = "white")


# Map: PT and car travel time side by side (Results figure)
# Each map uses its OWN quintile breaks (to show the pattern within each mode),
# so colours are NOT comparable between panels - say so in the caption.
# Blue for PT and red for car also signals "different scales".
tmap_mode("plot")

legend_style <- function(title) {
  tm_legend(title = title, title.size = 0.7, text.size = 0.55,
            position = tm_pos_in("left", "bottom"))
}

map_pt <- tm_shape(travel_props_Times) +
  tm_polygons(fill = "PT_transit_time_cbd_mins",
              fill.scale = tm_scale_intervals(style = "quantile", n = 5,
                                              values = "brewer.blues",   # light = quick
                                              value.na = "grey60",
                                              label.na = "No stop ≤ 30-min walk",
                                              label.format = list(digits = 0)),
              fill.legend = legend_style("PT travel time\nto CBD (min)"),
              col = "white", lwd = 0.1) +
  tm_layout(frame = FALSE)

map_car <- tm_shape(travel_props_Times) +
  tm_polygons(fill = "car_travel_time_cbd_mins",
              fill.scale = tm_scale_intervals(style = "quantile", n = 5,
                                              values = "brewer.reds",
                                              label.format = list(digits = 0)),
              fill.legend = legend_style("Car travel time\nto CBD (min)"),
              col = "white", lwd = 0.1) +
  tm_compass(type = "arrow", position = c("right", "top"), size = 1.5) +
  tm_scalebar(breaks = c(0, 5, 10), text.size = 0.6, position = c("right", "top")) +
  tm_layout(frame = FALSE)

(map_pt_car <- tmap_arrange(map_pt, map_car, ncol = 2))

tmap_save(map_pt_car, "all_figures/fig_car_pt_time.png",
          width = 24, height = 14, units = "cm", dpi = 300)
# -> Both shortest in the central isthmus. PT longest in the west and south
#    (83-151 min); North Shore relatively quick by PT (Northern Busway? - check).
#    Car (r5r free-flow): [describe pattern from the new map]

#  How closely do PT and car times move together?
cor.test(travel_props_Times$PT_transit_time_cbd_mins,
         travel_props_Times$car_travel_time_cbd_mins,
         method = "spearman", exact = FALSE)
sum(complete.cases(travel_props_Times$PT_transit_time_cbd_mins,
                   travel_props_Times$car_travel_time_cbd_mins))
# n = 491. rho = 0.84 (p < 0.001) with r5r free-flow car times
# (was 0.90 with TravelTime peak times).


# Mismatches: quick by car (2 fastest quintiles) but slow by PT (2 slowest)
travel_props_Times %>%
  st_drop_geometry() %>%
  filter(!is.na(PT_transit_time_cbd_mins)) %>%
  mutate(pt_q  = ntile(PT_transit_time_cbd_mins, 5),   # 5 = slowest PT
         car_q = ntile(car_travel_time_cbd_mins, 5)) %>% # 1 = fastest car
  filter(car_q <= 2, pt_q >= 4) %>%
  transmute(name = usual_residence_statistical_area_2_ascii_name_na,
            pt_time = PT_transit_time_cbd_mins, 
            car_time = car_travel_time_cbd_mins,
            extra_mins = pt_time - car_time, 
            ratio = round(pt_time / car_time, 1)) %>%
  arrange(desc(extra_mins))
# 9 SA2s quick by car but slow by PT, mostly west (Royal Heights, Ferguson,
# New Lynn, Blockhouse Bay, New Windsor) and south (Otahuhu East, Papatoetoe NE,
# Mt Wellington SE). PT takes 56-65 min longer (4.1-5.1x).
# (With TravelTime peak times there were only 3.)

#Supporting: travel time by income quartile (PT and car)
# Wiri West (income NA) and the 6 SA2s without PT time are excluded automatically.
income_breaks <- quantile(travel_props_Times$total_personal_income_median_2023,
                          probs = seq(0, 1, 0.25), na.rm = TRUE)
income_breaks   # dollar cut-offs for Q1-Q4 (report in Methods)

time_income <- travel_props_Times %>%
  st_drop_geometry() %>%
  filter(!is.na(total_personal_income_median_2023)) %>%
  mutate(income_tier = cut(total_personal_income_median_2023, breaks = income_breaks,
                           include.lowest = TRUE,
                           labels = c("Q1\n(lowest)", "Q2", "Q3", "Q4\n(highest)")))

# Figure: PT and car side by side, same y-axis
time_income_long <- time_income %>%
  select(income_tier, `Public transport` = PT_transit_time_cbd_mins,
         Car = car_travel_time_cbd_mins) %>%
  pivot_longer(-income_tier, names_to = "mode", values_to = "time_mins") %>%
  filter(!is.na(time_mins)) %>%
  mutate(mode = factor(mode, levels = c("Public transport", "Car")))

count(time_income_long, mode)   # PT 490, car 496

(p_time_income <- ggplot(time_income_long, aes(x = income_tier, y = time_mins)) +
    geom_boxplot(fill = "grey85", outlier.size = 0.8, outlier.alpha = 0.5) +
    facet_wrap(~ mode) +
    labs(x = "Median personal income quartile (SA2)",
         y = "Travel time to CBD (minutes)") +
    theme_minimal() +
    theme(strip.text = element_text(face = "bold", size = 11)))
ggsave("all_figures/fig_time_by_income_pt_car.png", p_time_income,
       width = 18, height = 10, units = "cm", dpi = 300, bg = "white")

# Tests - one helper so PT and car are tested the same way
test_by_income <- function(var) {
  d   <- time_income %>% filter(!is.na(.data[[var]]))
  fit <- aov(reformulate("income_tier", var), data = d)
  ss  <- summary(fit)[[1]][["Sum Sq"]]
  list(
    n            = nrow(d),
    group_median = d %>% group_by(income_tier) %>%
                     summarise(n = n(), median = median(.data[[var]]), .groups = "drop"),
    anova        = summary(fit),
    eta_squared  = round(ss[1] / sum(ss), 2),          # share of variation explained
    tukey        = TukeyHSD(fit),                      # which quartiles differ
    kruskal      = kruskal.test(reformulate("income_tier", var), data = d),  # no normality assumed
    spearman     = cor.test(d$total_personal_income_median_2023, d[[var]],
                            method = "spearman", exact = FALSE))
}

(pt_by_income  <- test_by_income("PT_transit_time_cbd_mins"))
(car_by_income <- test_by_income("car_travel_time_cbd_mins"))

# Results (Wiri West excluded):
#   PT  (n = 490): F(3, 486) = 16.5, p < 0.001, eta^2 = 0.09. Q4 vs Q1 = -15.6 min
#        (95% CI -22.6 to -8.7); Q4 also shorter than Q2 (-17.1) and Q3 (-10.5).
#        Q1-Q3 do not differ. KW chi2 = 60.9. rho = -0.32.  (unchanged)
#   Car (n = 496, r5r free-flow): F(3, 492) = 9.5, p < 0.001, eta^2 = 0.05.
#        Medians Q1 22, Q2 21, Q3 20, Q4 17 min. Q4 vs Q1 = -3.6 min
#        (95% CI -5.6 to -1.6); Q4 also shorter than Q2 (-3.5) and Q3 (-2.2).
#        Q1-Q3 do not differ. KW chi2 = 31.3. rho = -0.24.
#        (TravelTime peak times gave Q4 vs Q1 = -9.0 min.)
#   -> The income gap is ~4x larger for PT (-15.6 min) than for car (-3.6 min):
#      lower-income SA2s lose more time by switching from car to PT (RQ3.2).



#  Save
st_write(travel_props_Times, "./data/Saved/travel_props_Times.gpkg",
         delete_dsn = TRUE, quiet = TRUE)
