# =============================================================================
# 02_mode_shares_RQ1.R

# PURPOSE  RQ1 (part 1): how does car commuting vary across SA2s compared
#          with public transport? Calculates mode shares and describes them.
#
# INPUT    data/Saved/sa2_clean.gpkg                 (from 01)
# OUTPUT   data/Saved/travel_props.gpkg              (497 SA2s + mode shares)
#          all_figures/fig_regional_split.png        (boxplots of mode shares)
#          all_figures/fig_outlier_car_mode_mix.png  (low-car outliers)
#          all_figures/fig_car_pt_maps.png           (car vs PT share maps)
#          all_figures/car_vs_PT_shares.png          (Appendix: histograms)
#
# NEXT     03_spatial_clustering_RQ1.R
# =============================================================================

library(sf)
library(tidyverse)
library(tmap)

sa2_clean <- st_read("./data/Saved/sa2_clean.gpkg", quiet = TRUE)



# 1. Calculate mode shares

# Denominator = total stated (includes people who work from home).
# Groupings used throughout the thesis:
#   Car driver = drive a private car + drive a company car
#                (passengers excluded: they do not add a vehicle to congestion)
#   PT         = bus + train + ferry
#   Active     = walk/jog + bicycle

# "Total" equals "total stated" in every SA2 -> "not elsewhere included" is always 0
all(sa2_clean$transportMode_total == sa2_clean$transportMode_total_stated, na.rm = TRUE)

travel_props <- sa2_clean %>%
  select(-c(transportMode_did_not_go_to_work_today,       # all NA in 2023
            transportMode_not_elsewhere_included)) %>%    # always 0
  # Drop SA2s whose mode counts are suppressed (confidential) -> cannot compute shares
  filter(!is.na(transportMode_total_stated),
         if_all(starts_with("transportMode_"), ~ !is.na(.x))) %>%
  # Share for every individual mode, e.g. prop_transport_train
  mutate(across(starts_with("transportMode_") &
                  !any_of(c("transportMode_total", "transportMode_total_stated")),
                ~ .x / transportMode_total_stated,
                .names = "prop_{.col}")) %>%
  rename_with(~ str_replace(., "prop_transportMode_", "prop_transport_"), starts_with("prop_")) %>%
  # Aggregate groups
  mutate(
    prop_aggregate_private_car      = (transportMode_drive_a_private_car_truck_or_van +
                                         transportMode_drive_a_company_car_truck_or_van) / transportMode_total_stated,
    prop_aggregate_car_passenger    = transportMode_passenger_in_a_car_truck_van_or_company_bus / transportMode_total_stated,
    prop_aggregate_public_transport = (transportMode_public_bus + transportMode_train +
                                         transportMode_ferry) / transportMode_total_stated,
    prop_aggregate_active_modes     = (transportMode_walk_or_jog + transportMode_bicycle) / transportMode_total_stated,
    prop_aggregate_work_at_home     = transportMode_work_at_home / transportMode_total_stated,
    prop_aggregate_other            = transportMode_other / transportMode_total_stated)

nrow(sa2_clean) - nrow(travel_props)   # SA2s dropped because of suppressed counts
nrow(travel_props)                     # 497 SA2s used from here on

# Which SA2s were dropped? (shown in grey on the maps)
suppressed <- sa2_clean %>%
  filter(!usual_residence_statistical_area_2_code_na %in%
           travel_props$usual_residence_statistical_area_2_code_na)
suppressed$usual_residence_statistical_area_2_ascii_name_na



# 2. Quick checks

travel_props %>% st_drop_geometry() %>% select(starts_with("prop_aggregate_")) %>% summary()

# How many SA2s have a 0% share for each mode?
# (Zero shares are real in small SA2s, e.g. no ferry outside ferry suburbs.)
travel_props %>%
  st_drop_geometry() %>%
  summarise(across(c(prop_transport_drive_a_company_car_truck_or_van,
                     prop_transport_passenger_in_a_car_truck_van_or_company_bus,
                     prop_transport_public_bus, prop_transport_train, prop_transport_ferry,
                     prop_transport_bicycle, prop_transport_walk_or_jog, prop_transport_other),
                   ~ sum(.x == 0))) %>%
  pivot_longer(everything(), names_to = "mode", values_to = "n_SA2s_with_0pct")

# Random rounding: Stats NZ rounds counts to base 3, so modes do not always add
# up exactly to the total. Difference was within +/-12 for every SA2, so regional
# shares sum to 99.99% rather than 100%. Mention in Methods (data).
travel_props %>% st_drop_geometry() %>%
  mutate(diff = (transportMode_work_at_home +
                   transportMode_drive_a_private_car_truck_or_van +
                   transportMode_drive_a_company_car_truck_or_van +
                   transportMode_passenger_in_a_car_truck_van_or_company_bus +
                   transportMode_public_bus + transportMode_train + transportMode_ferry +
                   transportMode_bicycle + transportMode_walk_or_jog + transportMode_other) -
           transportMode_total_stated) %>%
  count(diff)



# 3. Regional mode split (all commuters pooled) vs SA2 distribution

# Regional split = total commuters by mode / total commuters (weighted by size).
# This differs from the SA2 MEDIAN, which treats every SA2 equally.
(regional_split <- travel_props %>% st_drop_geometry() %>%
   summarise(
     `Car driver`       = sum(transportMode_drive_a_private_car_truck_or_van +
                                transportMode_drive_a_company_car_truck_or_van),
     `Car passenger`    = sum(transportMode_passenger_in_a_car_truck_van_or_company_bus),
     `Public transport` = sum(transportMode_public_bus + transportMode_train + transportMode_ferry),
     `Active modes`     = sum(transportMode_walk_or_jog + transportMode_bicycle),
     `Work at home`     = sum(transportMode_work_at_home),
     Other              = sum(transportMode_other),
     total              = sum(transportMode_total_stated)) %>%
   pivot_longer(-total, names_to = "Mode", values_to = "n") %>%
   mutate(Share = n / total) %>%
   arrange(desc(Share)))
# -> Report these regional shares in Results (car driver is by far the largest).

# Boxplot of SA2 shares for each mode, with the regional share in a side column
shares_long <- travel_props %>%
  st_drop_geometry() %>%
  select(starts_with("prop_aggregate_")) %>%
  pivot_longer(everything(), names_to = "Mode", values_to = "Share") %>%
  mutate(Mode = Mode %>% str_remove("prop_aggregate_") %>%
           str_replace_all("_", " ") %>% tools::toTitleCase(),
         Mode = fct_reorder(Mode, Share, .fun = median))

medians <- shares_long %>%
  group_by(Mode) %>%
  summarise(med = median(Share)) %>%
  mutate(y = as.numeric(Mode))

regional_col <- regional_split %>%
  mutate(Mode = recode(Mode, "Car driver" = "Private Car", "Car passenger" = "Car Passenger",
                       "Public transport" = "Public Transport", "Active modes" = "Active Modes",
                       "Work at home" = "Work at Home"),
         Mode = factor(Mode, levels = levels(shares_long$Mode)))

x_col <- max(shares_long$Share) + 0.08   # position of the right-hand column

(p_spread <- ggplot(shares_long, aes(x = Share, y = Mode)) +
    geom_boxplot(fill = "grey85", outlier.size = 0.8, outlier.alpha = 0.5) +
    geom_text(data = medians, aes(x = med, y = y + 0.38,
                                  label = scales::percent(med, accuracy = 0.1)),
              size = 3, vjust = -0.3) +
    geom_text(data = regional_col, aes(x = x_col, y = Mode,
                                       label = scales::percent(Share, accuracy = 0.1)),
              size = 3.2, fontface = "bold", hjust = 0) +
    annotate("text", x = x_col, y = length(levels(shares_long$Mode)) + 0.7,
             label = "Urban\nAuckland\noverall", size = 3, hjust = 0,
             fontface = "bold", lineheight = 0.9) +
    scale_x_continuous(labels = scales::percent, breaks = seq(0, 0.8, 0.2)) +
    coord_cartesian(xlim = c(0, max(shares_long$Share)), clip = "off") +
    labs(x = "Share of commuters (per SA2)", y = NULL) +
    theme_minimal() +
    theme(plot.margin = margin(15, 70, 5, 5)))

ggsave("all_figures/fig_regional_split.png", p_spread,
       width = 17, height = 10, units = "cm", dpi = 300, bg = "white")

# Appendix: car and PT share histograms
(p_car_pt_hist <- travel_props %>% st_drop_geometry() %>%
  select(`Car driver` = prop_aggregate_private_car,
         `Public transport` = prop_aggregate_public_transport) %>%
  pivot_longer(everything(), names_to = "Mode", values_to = "Share") %>%
  ggplot(aes(Share)) +
  geom_histogram(bins = 30, fill = "grey60", colour = "white") +
  facet_wrap(~ Mode, scales = "free") +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Share of commuters", y = "Number of SA2s") +
  theme_minimal())
ggsave("all_figures/car_vs_PT_shares.png", p_car_pt_hist,
       width = 16, height = 9, units = "cm", dpi = 300, bg = "white")



# 4. Car share outliers (IQR rule) and what they do instead of driving

y   <- travel_props$prop_aggregate_private_car
q1  <- quantile(y, 0.25); q3 <- quantile(y, 0.75); iqr <- q3 - q1

car_outliers <- travel_props %>%
  st_drop_geometry() %>%
  filter(prop_aggregate_private_car < q1 - 1.5 * iqr |
           prop_aggregate_private_car > q3 + 1.5 * iqr) %>%
  select(name = usual_residence_statistical_area_2_ascii_name_na,
         prop_aggregate_private_car, n_commuters = transportMode_total_stated) %>%
  arrange(prop_aggregate_private_car)
car_outliers
# -> All outliers are LOW car share (the distribution is left-skewed):
#    CBD apartments, city-fringe suburbs, and the Devonport ferry suburbs.

# Stacked bar chart of the full mode mix in those outlier SA2s
cbd_sa2   <- c("Queen Street", "Symonds Street North West", "Hobson Ridge North", "Anzac Avenue",
               "Shortland Street", "Symonds Street West", "Hobson Ridge South", "Symonds Street East",
               "Hobson Ridge Central", "Auckland-University", "Queen Street South West",
               "Quay Street-Customs Street", "Victoria Park", "Wynyard-Viaduct")
ferry_sa2 <- c("Devonport", "Stanley Point")

mode_mix <- travel_props %>% st_drop_geometry() %>%
  filter(usual_residence_statistical_area_2_ascii_name_na %in% car_outliers$name) %>%
  transmute(
    name  = usual_residence_statistical_area_2_ascii_name_na,
    group = case_when(name %in% cbd_sa2   ~ "CBD",
                      name %in% ferry_sa2 ~ "Ferry suburbs",
                      TRUE                ~ "City fringe"),
    `Car driver`       = prop_aggregate_private_car,
    `Car passenger`    = prop_aggregate_car_passenger,
    `Public transport` = prop_aggregate_public_transport,
    `Active modes`     = prop_aggregate_active_modes,
    `Work at home`     = prop_aggregate_work_at_home,
    Other              = prop_aggregate_other) %>%
  mutate(name  = fct_reorder(name, `Car driver`, .desc = TRUE),
         group = factor(group, levels = c("CBD", "City fringe", "Ferry suburbs"))) %>%
  pivot_longer(`Car driver`:Other, names_to = "Mode", values_to = "Share") %>%
  mutate(Mode = factor(Mode, levels = c("Car driver", "Car passenger", "Public transport",
                                        "Active modes", "Work at home", "Other")))

# Okabe-Ito palette: colour-blind safe (avoids the red-green pairing)
mode_cols <- c("Car driver" = "#D55E00", "Car passenger" = "#E69F00",
               "Public transport" = "#0072B2", "Active modes" = "#009E73",
               "Work at home" = "#CC79A7", "Other" = "grey60")

(outlier_car_mode_mix <- ggplot(mode_mix, aes(x = Share, y = name, fill = Mode)) +
    geom_col(width = 0.8, position = position_stack(reverse = TRUE)) +
    geom_text(aes(label = ifelse(Share >= 0.05, scales::percent(Share, accuracy = 1), "")),
              position = position_stack(vjust = 0.5, reverse = TRUE),
              size = 2.4, colour = "white", fontface = "bold") +
    facet_grid(group ~ ., scales = "free_y", space = "free_y") +
    scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
    scale_fill_manual(values = mode_cols) +
    labs(x = "Share of commuters", y = NULL, fill = NULL) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom", panel.grid.major.y = element_blank(),
          strip.text.y = element_text(angle = 0, face = "bold")))

ggsave("all_figures/fig_outlier_car_mode_mix.png", outlier_car_mode_mix,
       width = 16, height = 18, units = "cm", dpi = 300, bg = "white")
# -> CBD: walking dominates. City fringe: PT + active. Ferry suburbs: ferry (PT).



# 5. Maps: car share vs PT share (Results figure)
map_data <- travel_props %>%
  mutate(car_pct = 100 * prop_aggregate_private_car,
         pt_pct  = 100 * prop_aggregate_public_transport)

make_map <- function(var, title, pal, extras = FALSE) {
  m <- tm_shape(map_data) +
    tm_polygons(fill = var,
                fill.scale  = tm_scale_intervals(style = "quantile", n = 5, values = pal),
                fill.legend = tm_legend(title = title,
                                        position = tm_pos_in("left", "bottom"),
                                        title.size = 0.7, text.size = 0.6,
                                        frame = FALSE),
                col = "white", lwd = 0.1) +
    tm_shape(suppressed) +                                   # dropped SA2s in grey
    tm_polygons(fill = "grey80", col = "white", lwd = 0.1) +
    tm_add_legend(type = "polygons", labels = "No data",     # say "suppressed" in the caption
                  fill = "grey80", col = "white",
                  position = tm_pos_in("left", "bottom"),
                  text.size = 0.6, frame = FALSE) +
    tm_layout(frame = FALSE, legend.frame = FALSE,
              inner.margins = c(0.02, 0.02, 0.02, 0.02))
  if (extras) m <- m +
      tm_compass(type = "arrow", position = c("right", "top"), size = 1.5) +
      tm_scalebar(breaks = c(0, 5, 10), text.size = 0.6, position = c("right", "top"))
  m
}

tmap_mode("plot")
(map_car_pt <- tmap_arrange(make_map("car_pct", "Car share (%)", "brewer.reds"),
                            make_map("pt_pct",  "PT share (%)",  "brewer.blues", extras = TRUE),
                            ncol = 2))
dir.create("./all_figures/RQ1/RQ1.1", recursive = TRUE, showWarnings = FALSE)
tmap_save(map_car_pt, "./all_figures/RQ1/RQ1.1/fig_car_pt_maps.png",
          width = 24, height = 14, units = "cm", dpi = 300)





















# Notes only:side-by-side interactive car and PT maps (pan/zoom linked)
# library(leafsync)
# tmap_mode("view")
# notes_map <- travel_props %>%
#   mutate(car_pct = round(100 * prop_aggregate_private_car, 1),
#          pt_pct  = round(100 * prop_aggregate_public_transport, 1))
# popup <- c("Car (%)" = "car_pct", "PT (%)" = "pt_pct",
#            "Commuters" = "transportMode_total_stated")

# m_car <- tm_shape(notes_map) +
#   tm_polygons(fill = "car_pct",
#               fill.scale = tm_scale_intervals(style = "quantile", n = 5, values = "brewer.reds"),
#               fill.legend = tm_legend(title = "Car drivers (%)"),
#               fill_alpha = 0.7, col = "white", lwd = 0.3,
#               id = "usual_residence_statistical_area_2_ascii_name_na", popup.vars = popup)
# m_pt <- tm_shape(notes_map) +
#   tm_polygons(fill = "pt_pct",
#               fill.scale = tm_scale_intervals(style = "quantile", n = 5, values = "brewer.blues"),
#               fill.legend = tm_legend(title = "PT users (%)"),
#               fill_alpha = 0.7, col = "white", lwd = 0.3,
#               id = "usual_residence_statistical_area_2_ascii_name_na", popup.vars = popup)

# (side_by_side <- sync(tmap_leaflet(m_car), tmap_leaflet(m_pt)))
# htmltools::save_html(side_by_side,
#                      "./all_figures/Notes_Hinami_figures/02_Notes_car_pt_map.html")

tmap_mode("plot")   # switch back so later maps save as PNGs




# Do car and PT shares mirror each other? (supports the map description)
cor.test(travel_props$prop_aggregate_private_car,
         travel_props$prop_aggregate_public_transport,
         method = "spearman", exact = FALSE)
# -> Negative rho: SA2s with more driving have less PT use.



# Is car share higher than PT share across SA2s? (paired, non-parametric)
wilcox.test(travel_props$prop_aggregate_private_car,
            travel_props$prop_aggregate_public_transport,
            paired = TRUE, alternative = "greater")
sum(travel_props$prop_aggregate_private_car >
      travel_props$prop_aggregate_public_transport)   # SA2s where car > PT
# Car share exceeded PT share in 491 of 497 SA2s (99%), 
# a highly significant difference (Wilcoxon signed-rank test, 
# V = 123,702, p < 0.001).

travel_props %>% sf::st_drop_geometry() %>%
  filter(prop_aggregate_private_car <= prop_aggregate_public_transport) %>%
  select(usual_residence_statistical_area_2_ascii_name_na,
         car = prop_aggregate_private_car, pt = prop_aggregate_public_transport,
         walk = prop_aggregate_active_modes)



#  Highest / lowest car and PT SA2s (named examples for the text)
top_bottom <- function(var, n = 5) {
  d <- map_data %>% st_drop_geometry() %>%
    select(name = usual_residence_statistical_area_2_ascii_name_na, value = all_of(var),
           n_commuters = transportMode_total_stated)
  list(highest = slice_max(d, value, n = n, with_ties = FALSE),
       lowest  = slice_min(d, value, n = n, with_ties = FALSE))
}
top_bottom("car_pct")
top_bottom("pt_pct")



#Save
st_write(travel_props, "./data/Saved/travel_props.gpkg", delete_dsn = TRUE, quiet = TRUE)
