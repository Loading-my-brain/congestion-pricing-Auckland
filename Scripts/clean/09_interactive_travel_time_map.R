# =============================================================================
# 09_interactive_travel_time_map.R
# -----------------------------------------------------------------------------
# PURPOSE  Optional extra (not a thesis figure): an interactive web map of
#          free-flow car times, 8 am PT times and the PT / car ratio for each
#          SA2, with three layers you can switch on and off.
#
# INPUT    data/Saved/travel_props.gpkg                (from 02/03)
#          data/car_time_cbd_r5r_freeflow_inbound.csv  (from 04c)
#          data/pt_time_cbd_r5r_20260513_0800.csv      (from 04a)
#          data/boundary/sa3/statistical-area-3-2023-clipped-generalised.shp
# OUTPUT   all_figures/interactive_travel_times.html   (open in any browser)
#
# RUN      after 05. Not part of 00_run_all.R.
# =============================================================================
library(sf)
library(tidyverse)
library(tmap)

# 1. SA2 boundaries (497 SA2s)
sa2 <- st_read("./data/Saved/travel_props.gpkg", quiet = TRUE) %>%
  mutate(code = as.character(usual_residence_statistical_area_2_code_na),
         name = usual_residence_statistical_area_2_ascii_name_na)

# 2. Travel times
car_ff <- read.csv("data/car_time_cbd_r5r_freeflow_inbound.csv",
                   colClasses = c("character", "numeric")) %>%
  rename(code = SA22023_V1_00, car_mins = car_time_r5r_inbound)   # free-flow, SA2 -> CBD

pt <- read.csv("data/pt_time_cbd_r5r_20260513_0800.csv",
               colClasses = c("character", "numeric", "numeric")) %>%
  select(code = sa2_code, pt_mins = pt_time_cbd_mins)              # 8 am, 30-min walk

# 3. Join and calculate the ratio
map_data <- sa2 %>%
  left_join(car_ff, by = "code") %>%
  left_join(pt,     by = "code") %>%
  mutate(pt_car_ratio = round(pt_mins / car_mins, 1)) %>%
  select(name, code, car_mins, pt_mins, pt_car_ratio)

summary(st_drop_geometry(map_data)[, c("car_mins", "pt_mins", "pt_car_ratio")])

# CBD destination point (for reference on the map)
cbd <- st_read("data/boundary/sa3/statistical-area-3-2023-clipped-generalised.shp", quiet = TRUE) %>%
  filter(SA32023_V1 == "51150") %>%
  st_transform(st_crs(map_data)) %>%
  st_centroid()

# 4. Interactive map: three switchable layers
tmap_mode("view")

popup <- c("Car, free-flow (min)" = "car_mins",
           "PT, 8 am (min)"       = "pt_mins",
           "PT \u00f7 car"        = "pt_car_ratio")

(m_times <- 
    tm_shape(map_data) +
    tm_polygons(fill = "car_mins",
                fill.scale  = tm_scale_intervals(style = "quantile", n = 5, values = "brewer.reds"),
                fill.legend = tm_legend(title = "Car, free-flow (min)"),
                fill_alpha = 0.75, col = "white", lwd = 0.3,
                id = "name", popup.vars = popup, group = "Car travel time") +
    
    tm_shape(map_data) +
    tm_polygons(fill = "pt_mins",
                fill.scale  = tm_scale_intervals(style = "quantile", n = 5, values = "brewer.blues",
                                                 value.na = "grey60", label.na = "No stop \u2264 30-min walk"),
                fill.legend = tm_legend(title = "PT, 8 am (min)"),
                fill_alpha = 0.75, col = "white", lwd = 0.3,
                id = "name", popup.vars = popup, group = "PT travel time") +
    
    tm_shape(map_data) +
    tm_polygons(fill = "pt_car_ratio",
                fill.scale  = tm_scale_intervals(style = "quantile", n = 5, values = "brewer.purples",
                                                 value.na = "grey60"),
                fill.legend = tm_legend(title = "PT \u00f7 car time"),
                fill_alpha = 0.75, col = "white", lwd = 0.3,
                id = "name", popup.vars = popup, group = "PT / car ratio") )

# 5. Save as a web page you can open in any browser
tmap_save(m_times, "all_figures/interactive_travel_times.html")
