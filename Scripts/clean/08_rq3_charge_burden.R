# =============================================================================
# 08_rq3_charge_burden.R
# PURPOSE  RQ3: What are the implications for the equity of a city-centre
#          cordon charge?
#            RQ3.1  How large would the charge be relative to neighbourhood income?
#            RQ3.2  Do these neighbourhoods have a competitive PT alternative?
#
#            Step 1  Who would pay? City-centre drivers by income quartile
#            Step 2  Charge burden: annual charge as % of median income (RQ3.1)
#            Step 3  PT alternative by income quartile (RQ3.2)
#            Step 4  Combine: income x PT alternative (cf. Santos & Rojey, 2004)
#            Step 5  Figures
#
# INPUT    data/Saved/travel_props_vars.gpkg   (497 SA2s, from 06)
#          data/Saved/od_cbd_by_sa2.csv        (from 02b)
# OUTPUT   data/Saved/rq3_by_sa2.csv
#          all_figures/RQ3/fig_rq3_who_pays.png        (Results)
#          all_figures/RQ3/fig_rq3_burden_typology.png (Results: 1x2 map)
#
# ASSUMPTIONS (state in Methods 3.6 and Limitations)
#   - Stylised charge of NZ$5 per day, paid on every commuting day (as in the
#     proposal). No Auckland scheme has a confirmed price yet. The burden is
#     linear in the charge, so $3.50 and $10 are shown as sensitivity checks.
#   - 228 commuting days per year (260 weekdays in 2023 - 12 public holidays
#     - 20 days' annual leave under the Holidays Act 2003).
#   - Income = median annual personal income of residents aged 15+ (Census
#     2023). This includes people who are not working, so it is lower than
#     the drivers' own incomes -> the burden is an INDICATIVE upper estimate
#     for a typical resident, not the burden on an actual driver.
#   - Same income quartile cut-offs as 05 (all SA2s with an income value).
#   - SA2s inside the city centre are excluded (their trips do not cross the
#     cordon). Wiri West is excluded (income NA). -> 479 SA2s.
# =============================================================================

library(sf)
library(tidyverse)
library(tmap)
library(ppcor)

CHARGE <- 5 # NZ$ per day (main scenario)
(DAYS <- 260-(12+20)) # commuting days per year = 260 Business days -(12 public.H + 20 Holiday Act) 
MIN_TRIPS <- 30 # min. car + PT trips to the CBD before a PT share is used

dir.create("all_figures/RQ3", showWarnings = FALSE, recursive = TRUE)

# Days was calcualted: https://www.calculate.co.nz/business-days-calculator.php
# : There are 260 business days between Sun 1 Jan 2023 and Sun 31 Dec 2023. 
# 364 calendar days, 104 weekend days, 0 public holidays excluded.
# Public holidays on weekdays + Holidays Act 2003 minimum in Auckland: 2–3 Jan(New Year, observed), 
# 30 Jan (Auckland Anniversary), 6 Feb (Waitangi), 7 and 10 Apr (Easter), 
# 25 Apr (Anzac), 5 Jun (King’s Birthday), 14 Jul (Matariki), 23 Oct (Labour), 
# 25–26 Dec (Christmas, Boxing Day)



#   Data
 sa2 <- st_read("./data/Saved/travel_props_vars.gpkg", quiet = TRUE) %>%
  mutate(sa2_code = as.character(usual_residence_statistical_area_2_code_na))

od <- read.csv("data/Saved/od_cbd_by_sa2.csv",
               colClasses = c(sa2_code = "character"))
glimpse(od)
# Same quartile cut-offs as 05
(income_breaks <- quantile(sa2$total_personal_income_median_2023,
                          probs = seq(0, 1, 0.25), na.rm = TRUE))
#    0%   25%   50%   75%  100% 
# 13000 38800 44700 51725 80500 


rq3_sf <- sa2 %>%
  dplyr::select(sa2_code,
                sa2_name = usual_residence_statistical_area_2_ascii_name_na,
                income   = total_personal_income_median_2023,
                pt_time  = PT_transit_time_cbd_mins,
                car_time = car_travel_time_cbd_mins) %>%
  left_join(dplyr::select(od, -sa2_name), by = "sa2_code") %>%
  mutate(income_tier = cut(income, breaks = income_breaks, include.lowest = TRUE,
                           labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")),
         ratio          = pt_time / car_time,                 # PT/car time ratio
         drivers_per100 = 100 * cbd_car_driver / workers, # exposure to the charge, 
         # = % of workers who drive to the city centre
         
         burden_pct     = 100 * (CHARGE * DAYS) / income)#Section 4.3.2 Burden (RQ3.1)
glimpse(rq3_sf)

# Analysis set: outside the city centre, with an income value
rq3 <- rq3_sf %>%
  st_drop_geometry() %>%
  filter(!in_cbd, !is.na(income))

nrow(rq3)   #  479


# Who would pay? (incidence)
# Compare each quartile's share of ALL workers with its share of the
# CBD-bound car drivers (the people who would actually pay).
(who_pays <- rq3 %>%
   group_by(income_tier) %>%
   summarise(n_sa2         = n(),
             median_income = median(income),
             workers       = sum(workers),
             cbd_drivers   = sum(cbd_car_driver),
             cbd_pt        = sum(cbd_pt)) %>%
   mutate(pct_of_workers     = round(100 * workers / sum(workers), 1),
          pct_of_cbd_drivers = round(100 * cbd_drivers / sum(cbd_drivers), 1),
          drivers_per_100    = round(100 * cbd_drivers / workers, 2),
          pt_share_cbd_trips = round(100 * cbd_pt / (cbd_pt + cbd_drivers), 1)))#within this income group, how many city-centre commuters take PT instead of driving?
#  Who would pay? (RQ3, before RQ3.1)
  # Q1 = 19.7% of workers but 7.5% of CBD drivers;
  # Q4 = 26.4% of workers but 44.1% of CBD drivers.

# Do richer SA2s send more drivers into the city centre (per 100 workers)?
cor.test(rq3$income, rq3$drivers_per100, method = "spearman", exact = FALSE)
# Expected rho ~ +0.59

# Is the income link just distance? Partial Spearman, controlling for car time
ppcor::pcor.test(rq3$income, rq3$drivers_per100, rq3$car_time, method = "spearman")
# Result: partial rho = 0.55, p < 0.001 (n = 479) -> income link holds after controlling for distance


#   summary by income quartile (Results 4.3.1-4.3.3)
summ <- function(df) {
  df %>% summarise(workers  = sum(workers),
                   drivers  = sum(cbd_car_driver),
                   cbd_pt   = sum(cbd_pt),
                   burden   = median(burden_pct),
                   pt_time  = median(pt_time, na.rm = TRUE),
                   car_time = median(car_time),
                   ratio    = median(ratio, na.rm = TRUE))
}
tot_w <- sum(rq3$workers)
tot_d <- sum(rq3$cbd_car_driver)

(table_4_3 <- bind_rows(
  rq3 %>% group_by(income_tier = as.character(income_tier)) %>% summ(),
  rq3 %>% summ() %>% mutate(income_tier = "All SA2s")) %>%
    transmute(`Income quartile`                                = income_tier,
              `Workers (%)`                                    = round(100 * workers / tot_w, 1),
              `City-centre drivers (n)`                        = drivers,
              `City-centre drivers (% of all drivers)`         = round(100 * drivers / tot_d, 1),
              `Workers who drive to the city centre (%)`       = round(100 * drivers / workers, 1),
              `Charge burden (% of income)`                    = round(burden, 1),
              `PT time (min)`                                  = pt_time,
              `Car time (min)`                                 = car_time,
              `PT/car ratio`                                   = round(ratio, 2),
              `City-centre commuters using PT (% of car + PT)` = round(100 * cbd_pt / (cbd_pt + drivers), 1)))
write.csv(table_4_3, "all_figures/RQ3/table_4_3_rq3_by_quartile.csv", row.names = FALSE)
# Have a look at Q1; Lower income groups, don't even travel to CBD for work anyway. 
# A city-centre cordon would largely miss South Auckland’s car-dependent workers, so it wouldn’t charge them, but its revenue wouldn’t reach them either. And a motorway charge could affect them far more


# Charge burden (RQ3.1)
CHARGE * DAYS   # annual cost for a daily commuter: $1140

(burden_tbl <- rq3 %>%
   group_by(income_tier) %>%
   summarise(median_income = median(income),
             burden_3.50   = round(median(100 * 3.5 * DAYS / income), 2),
             burden_5      = round(median(100 * 5   * DAYS / income), 2),
             burden_10     = round(median(100 * 10  * DAYS / income), 2)))
# Result at $5 (228 days): Q1 3.27%, Q2 2.73%, Q3 2.35%, Q4 2.03%
summary(rq3$burden_pct)   # range across SA2s at $5


# Is there a competitive PT alternative? (RQ3.2)
# (a) Modelled: PT time and PT/car ratio by income quartile
(pt_alt <- rq3 %>%
   group_by(income_tier) %>%
   summarise(median_pt_time  = median(pt_time, na.rm = TRUE),
             median_car_time = median(car_time),
             median_ratio    = round(median(ratio, na.rm = TRUE), 2),
             no_pt_access    = sum(is.na(pt_time))))
kruskal.test(ratio ~ income_tier, data = rq3)
# Expected: ratio ~3.2-3.4 in every quartile, p ~ 0.11 -> PT is uncompetitive
# EVERYWHERE; but absolute PT time is longer for Q1 (74 min) than Q4 (51 min).

# (b) Observed vs modelled: where PT is relatively slower, do fewer CBD
#     commuters take it?  (moved here from the end of 02b)
chk <- rq3 %>% filter(cbd_car_driver + cbd_pt >= MIN_TRIPS, !is.na(ratio))
nrow(chk)   #  309
cor.test(chk$ratio, chk$cbd_pt_share, method = "spearman", exact = FALSE)
# Expected rho ~ -0.52: a worse PT/car ratio goes with a lower observed PT share
# -> the modelled travel times match real behaviour.


# Combine income and PT alternative (2 x 2)
# Split at the medians of the 479 SA2s. The 6 SA2s with no PT stop within a
# 30-min walk count as "weaker PT".
med_income <- median(rq3$income)
med_ratio  <- median(rq3$ratio, na.rm = TRUE)
med_expo   <- median(rq3$drivers_per100)
c(med_income, med_ratio, med_expo)   # expect 44,700 / 3.28 / 2.17

type_levels <- c("Lower income, weaker PT", "Lower income, better PT",
                 "Higher income, weaker PT", "Higher income, better PT")

rq3 <- rq3 %>%
  mutate(low_income = income < med_income,
         weaker_pt  = is.na(ratio) | ratio > med_ratio,
         type = factor(case_when(low_income & weaker_pt ~ type_levels[1],
                                 low_income             ~ type_levels[2],
                                 weaker_pt              ~ type_levels[3],
                                 TRUE                   ~ type_levels[4]),
                       levels = type_levels),
         # Most exposed: lower income + weaker PT + above-median share driving in
         most_exposed = low_income & weaker_pt & drivers_per100 > med_expo)

(type_tbl <- rq3 %>%
   group_by(type) %>%
   summarise(n_sa2 = n(),
             cbd_drivers  = sum(cbd_car_driver),
             median_burden = round(median(burden_pct), 2),
             median_ratio  = round(median(ratio, na.rm = TRUE), 2)) %>%
   mutate(pct_of_cbd_drivers = round(100 * cbd_drivers / sum(cbd_drivers), 1)))

# The most exposed SA2s (named examples for the text)
sum(rq3$most_exposed) # expect 52
sum(rq3$cbd_car_driver[rq3$most_exposed]) # expect ~2,691 (14%)
rq3 %>% filter(most_exposed) %>%
  arrange(desc(cbd_car_driver)) %>%
  dplyr::select(sa2_name, income, cbd_car_driver, drivers_per100, ratio, burden_pct) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  head(10)
# Expected: mostly west / inner-west (Avondale, Pt Chevalier West, Te Atatu
# South, Kelston, New Lynn, New Windsor, Massey)

write.csv(rq3, "data/Saved/rq3_by_sa2.csv", row.names = FALSE)


#  Figures
#  Who pays: share of workers vs share of CBD-bound drivers
# x-axis labels show each quartile's income range (from income_breaks)
fmt <- function(x) scales::dollar(round(x, -2))     # e.g. 38800 -> "$38,800"
tier_labels <- c(
  "Q1 (lowest)"  = paste0("Q1\nunder ", fmt(income_breaks[2])),
  "Q2" = paste0("Q2\n", fmt(income_breaks[2]), "–", fmt(income_breaks[3])),
  "Q3" = paste0("Q3\n", fmt(income_breaks[3]), "–", fmt(income_breaks[4])),
  "Q4 (highest)" = paste0("Q4\nover ", fmt(income_breaks[4])))

p_who <- who_pays %>%
  dplyr::select(income_tier,
                `All workers` = pct_of_workers,
                `Car drivers to the city centre` = pct_of_cbd_drivers) %>%
  pivot_longer(-income_tier, names_to = "group", values_to = "pct") %>%
  ggplot(aes(income_tier, pct, fill = group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(pct), "%")),
            position = position_dodge(width = 0.8), vjust = -0.4, size = 3) +
  scale_x_discrete(labels = tier_labels) +
  scale_fill_manual(values = c("grey70", "#D55E00")) +
  labs(x = "Neighbourhood (SA2) median personal income", y = "Share (%)", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")
p_who
ggsave("all_figures/RQ3/fig_rq3_who_pays.png", p_who,
       width = 16, height = 10, units = "cm", dpi = 300, bg = "white")

# 6b. Maps: (a) charge burden, (b) income x PT typology
map_data <- rq3_sf %>%
  dplyr::select(sa2_code) %>%
  left_join(dplyr::select(rq3, sa2_code, burden_pct, type, most_exposed),
            by = "sa2_code")

cbd_outline <- rq3_sf %>% filter(in_cbd) %>% st_union()

# High-High car-commuting clusters from RQ1 (saved by 03 as column lisa_car)
hh <- st_read("./data/Saved/travel_props.gpkg", quiet = TRUE) %>%
  filter(lisa_car == "High-High") %>%
  st_union()
exposed     <- map_data %>% filter(most_exposed %in% TRUE)

map_burden <- tm_shape(map_data) +
  tm_polygons(fill = "burden_pct",
              fill.scale = tm_scale_intervals(style = "quantile", n = 5,
                                              values = "brewer.yl_or_rd",
                                              value.na = "grey85",
                                              label.na = "City centre / excluded",
                                              label.format = list(digits = 1)),
              fill.legend = tm_legend(title = "Annual charge\n(% of median income)",
                                      position = tm_pos_in("left", "bottom"),
                                      title.size = 0.7, text.size = 0.6,
                                      frame = FALSE),
              col = "white", lwd = 0.1) +
  tm_shape(cbd_outline) + tm_borders(col = "grey40", lwd = 1, lty = "dashed") +
  tm_title(paste0("(a) Burden of a $", CHARGE, "/day charge"), size = 0.9) +
  tm_layout(frame = FALSE, legend.frame = FALSE)

map_type <- tm_shape(map_data) +
  tm_polygons(fill = "type",
              fill.scale = tm_scale_categorical(
                values = c("#D55E00", "#E69F00", "#56B4E9", "#0072B2"),
                value.na = "grey85", label.na = "City centre / excluded"),
              fill.legend = tm_legend(title = "Income and PT alternative",
                                      position = tm_pos_in("left", "bottom"),
                                      title.size = 0.7, text.size = 0.6,
                                      frame = FALSE),
              col = "white", lwd = 0.1) +
  tm_shape(hh) + tm_borders(col = "black", lwd = 1.2) +                         # High-High car clusters (RQ1): black solid, as in Figure 4.5
  tm_shape(cbd_outline) + tm_borders(col = "grey40", lwd = 1, lty = "dashed") +  # city centre
  tm_shape(st_point_on_surface(exposed)) + tm_dots(fill = "black", size = 0.12) + # 52 most exposed SA2s: black dot
  tm_add_legend(type = "lines", labels = c("High-High car share", "City centre"),
                col = c("black", "grey40"), lwd = c(1.2, 1), lty = c("solid", "dashed"),
                position = tm_pos_in("right", "bottom")) +
  tm_add_legend(type = "symbols", labels = "Most exposed SA2 (n = 52)", fill = "black",
                size = 0.4, position = tm_pos_in("right", "bottom")) +
  tm_title("(b) Income and PT alternative", size = 0.9) +
  tm_compass(type = "arrow", position = c("right", "top"), size = 0.8, text.size = 0.5) +
  tm_scalebar(breaks = c(0, 5, 10), text.size = 0.5, position = c("right", "top")) +
  tm_layout(frame = FALSE, legend.frame = FALSE)

(map_rq3 <- tmap_arrange(map_burden, map_type, ncol = 2))
tmap_save(map_rq3, "all_figures/RQ3/fig_rq3_burden_typology.png",
          width = 24, height = 14, units = "cm", dpi = 300)


# link the RQ1 High-High car clusters to RQ2 and RQ3 
 hh_lisa <- st_read("./data/Saved/travel_props.gpkg", quiet = TRUE) %>%
  st_drop_geometry() %>%
  transmute(sa2_code = as.character(usual_residence_statistical_area_2_code_na), lisa_car)

# RQ2: how many High-High SA2s have a significant PT-time coefficient (GWR, from 07)?
gwr_res <- st_read("./data/Saved/gwr_results.gpkg", quiet = TRUE) %>%
  st_drop_geometry() %>% mutate(sa2_code = as.character(sa2_code))
gwr_res %>% left_join(hh_lisa, by = "sa2_code") %>% filter(lisa_car == "High-High") %>%
  summarise(n_hh_in_gwr = n(),
            pt_sig_pos  = sum(pt_time_10_sig > 0, na.rm = TRUE),
            pt_sig_neg  = sum(pt_time_10_sig < 0, na.rm = TRUE))

# RQ3: city-centre drivers and neighbourhood types inside High-High clusters
rq3_df <- if (inherits(rq3, "sf")) st_drop_geometry(rq3) else rq3
rq3_df %>% left_join(hh_lisa, by = "sa2_code") %>%
  mutate(in_hh = lisa_car %in% "High-High") %>%
  group_by(in_hh) %>%
  summarise(n_sa2          = n(),
            drivers        = sum(cbd_car_driver),
            pct_of_drivers = round(100 * drivers / sum(rq3_df$cbd_car_driver), 1),
            most_exposed   = sum(most_exposed),
            lowinc_weakpt  = sum(type == "Lower income, weaker PT"))
# -> in_hh = TRUE row: SA2s in High-High clusters. Put n_sa2, drivers,
#    pct_of_drivers, most_exposed and lowinc_weakpt into Section 4.3.4.
