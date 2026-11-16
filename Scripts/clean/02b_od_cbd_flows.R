# =============================================================================
# 02b_od_cbd_flows.R
# -----------------------------------------------------------------------------
# PURPOSE  RQ3: Who commutes INTO the city centre, and how?
#          (identifies who would pay a cordon charge; input to RQ3.1 and RQ3.2)
#          Counts 2023 Census commuters from each urban-Auckland SA2 to the
#          city-centre SA2s, split by mode (car driver / PT / active).
#          -> Car drivers = the people who would actually pay a cordon charge.
#          -> PT share of CBD-bound trips = OBSERVED competitiveness of PT
#             (complements the MODELLED travel times in 04-05).
#
# INPUT    data/OD-data/2023-census-main-means-of-travel-to-work-by-statistical-area.dbf
#          data/Saved/travel_props.gpkg      (497 SA2s, from 02/03)
#          data/boundary/sa3/statistical-area-3-2023-clipped-generalised.shp
# OUTPUT   data/Saved/od_cbd_by_sa2.csv     (one row per origin SA2)
#          all_figures/fig_od_cbd_flows.png (Results: 1x2 map)
#
# CBD      = SA2s whose centroid lies in SA3 51150 (Auckland City Centre).
#            Trips that START inside the CBD are excluded (they never cross
#            the cordon).
#
# NB       Stats NZ suppresses small OD cells (-999) and randomly rounds all
#          counts to base 3 (RR3). Suppressed cells are treated as 0, so
#          totals are a slight UNDER-estimate (Limitations).
#
# REPLACES the August exploration code. Two fixes vs that version:
#   1. SA2s are matched by CODE, not name (names have macron differences).
#   2. Rows are no longer dropped when car drivers = 0, which had removed
#      PT-only routes and under-counted CBD-bound PT riders.
#
# NEXT     03_spatial_clustering_RQ1.R
# =============================================================================

library(sf)
library(tidyverse)
library(foreign)    # read.dbf()
library(tmap)


# Study SA2s and the city-centre (CBD) SA2s
travel_props <- st_read("./data/Saved/travel_props.gpkg", quiet = TRUE) %>%
  mutate(sa2_code = as.character(usual_residence_statistical_area_2_code_na))

cbd_sa3 <- st_read("data/boundary/sa3/statistical-area-3-2023-clipped-generalised.shp",
                   quiet = TRUE) %>%
  filter(SA32023_V1 == "51150") %>%
  st_transform(st_crs(travel_props))

cbd_codes <- travel_props %>%
  st_centroid() %>%
  st_filter(cbd_sa3) %>%
  pull(sa2_code)

length(cbd_codes)   # expect 17 (Aug exploration)
travel_props %>% st_drop_geometry() %>%
  filter(sa2_code %in% cbd_codes) %>%
  pull(usual_residence_statistical_area_2_ascii_name_na)


# -----------------------------------------------------------------------------
# Load and clean the OD table (2023 columns only)
# Stats NZ. (2025). 2023 Census main means of travel to work by statistical area 2 [Data set]. 
# https://datafinder.stats.govt.nz/table/121988-2023-census-main-means-of-travel-to-work-by-statistical-area-2/
# -----------------------------------------------------------------------------
# Origin = SA22023_V1 (code), destination = SA22023__3 (code)
od_raw <-read.dbf("./data/OD-data/2023-census-main-means-of-travel-to-work-by-statistical-area.dbf")

to_num <- function(x) { # factor -> number; -999 (suppressed) -> 0
  v <- as.numeric(as.character(x))
  ifelse(is.na(v) | v < 0, 0, v)
}

od <- od_raw %>%
  transmute(
    origin_code = as.character(SA22023_V1),
    dest_code   = as.character(SA22023__3),
    car_driver  = to_num(X2023_Drive) + to_num(X2023_Dri_1),   # private + company
    passenger   = to_num(X2023_Passe),
    pt          = to_num(X2023_Publi) + to_num(X2023_Train) + to_num(X2023_Ferry),
    active      = to_num(X2023_Walk_) + to_num(X2023_Bicyc),
    other       = to_num(X2023_Other)
  ) %>%
  mutate(travelled = car_driver + passenger + pt + active + other)


#  Inbound CBD flows: study-area origin OUTSIDE the CBD -> destination INSIDE
od_cbd <- od %>%
  filter(origin_code %in% travel_props$sa2_code,
         !origin_code %in% cbd_codes,
         dest_code %in% cbd_codes)

# Region-wide totals (report in Results, RQ3 section)
(cbd_totals <- od_cbd %>%
    summarise(across(c(car_driver, passenger, pt, active, other, travelled), sum)) %>%
    mutate(car_share_pct = round(100 * car_driver / travelled, 1),
           pt_share_pct  = round(100 * pt / travelled, 1)))
# Expect car_driver close to 18,990 (Aug exploration, same CBD definition).


# -----------------------------------------------------------------------------
# Study-area coverage: what share of Auckland's city-centre drivers
#     live in urban Auckland? (justifies the study area, Methods 3.2)

#Stats NZ. (2022). Territorial authority 2023 (clipped, generalised) [Data set]. 
# https://datafinder.stats.govt.nz/layer/111193-territorial-authority-2023-clipped-generalised/
# -----------------------------------------------------------------------------
# the Auckland territorial authority boundary
ta_akl <- st_read("data/territorial-authority-2023-clipped-generalised/territorial-authority-2023-clipped-generalised.shp",
                  quiet = TRUE) %>%
  filter(TA2023_V_1 == "Auckland") %>%
  st_transform(st_crs(travel_props))

#which SA2s are in Auckland TA? (any SA2 that overlaps the boundary)
sa2_area <- st_read("./data/SA2-part2/2023-census-totals-by-topic-for-individuals-by-statistical-a.shp",
                    quiet = TRUE) %>%
  dplyr::select(sa2_code = SA22023_V1) %>%
  mutate(sa2_code = as.character(sa2_code)) %>%
  st_transform(st_crs(travel_props))

akl_codes <- sa2_area$sa2_code[lengths(st_intersects(sa2_area, ta_akl)) > 0]

# all car drivers INTO the city centre, from anywhere (national OD table)
all_in <- od %>% filter(dest_code %in% cbd_codes, !origin_code %in% cbd_codes)

#add them up three ways
(coverage <- c(
  all_nz   = sum(all_in$car_driver), # from anywhere in NZ
  auckland = sum(all_in$car_driver[all_in$origin_code %in% akl_codes]), # from Auckland TA
  study    = sum(all_in$car_driver[all_in$origin_code %in% travel_props$sa2_code]))) # from urban Auckland

round(100 * coverage["study"] / coverage["auckland"], 1)   # % of Auckland's drivers in the study area


# One row per origin SA2
od_cbd_sa2 <- od_cbd %>%
  group_by(sa2_code = origin_code) %>%
  summarise(across(c(car_driver, passenger, pt, active, other, travelled), sum),
            .groups = "drop") %>%
  rename_with(~ paste0("cbd_", .x), -sa2_code) %>%
  mutate(
    # PT share of CBD-bound trips by car OR PT (observed PT competitiveness)
    cbd_pt_share = if_else(cbd_car_driver + cbd_pt > 0,
                           cbd_pt / (cbd_car_driver + cbd_pt), NA_real_))

# All 497 study SA2s (0 where nobody commutes to the CBD; NA for CBD SA2s)
od_cbd_sa2 <- travel_props %>% st_drop_geometry() %>%
  dplyr::select(sa2_code, sa2_name = usual_residence_statistical_area_2_ascii_name_na,
         workers = transportMode_total_stated) %>%
  left_join(od_cbd_sa2, by = "sa2_code") %>%
  mutate(in_cbd = sa2_code %in% cbd_codes,
         across(c(cbd_car_driver, cbd_passenger, cbd_pt, cbd_active,
                  cbd_other, cbd_travelled),
                ~ if_else(in_cbd, NA_real_, coalesce(.x, 0))),
         # share of the SA2's workers who commute to the CBD
         cbd_commute_share = cbd_travelled / workers)
write.csv(od_cbd_sa2, "data/Saved/od_cbd_by_sa2.csv", row.names = FALSE)

# Describe: where do CBD-bound drivers come from?
# Top 15 origins of CBD-bound car drivers
od_cbd_sa2 %>% arrange(desc(cbd_car_driver)) %>%
  dplyr::select(sa2_name, cbd_car_driver, cbd_pt, cbd_active, cbd_pt_share) %>%
  mutate(cbd_pt_share = round(cbd_pt_share, 2)) %>%
  head(15)

# How concentrated? Share of all CBD-bound drivers from the top 10% of SA2s
od_cbd_sa2 %>% filter(!in_cbd) %>%
  arrange(desc(cbd_car_driver)) %>%
  mutate(cum_share = cumsum(cbd_car_driver) / sum(cbd_car_driver)) %>%
  slice(ceiling(0.1 * n())) %>% pull(cum_share)

# Distribution of the PT share of CBD-bound trips
summary(od_cbd_sa2$cbd_pt_share)

# Does the observed PT share match the MODELLED PT disadvantage? (run after 05)
# times <- st_read("./data/Saved/travel_props_Times.gpkg", quiet = TRUE) %>%
#   st_drop_geometry() %>%
#   transmute(sa2_code = as.character(usual_residence_statistical_area_2_code_na),
#             ratio = PT_transit_time_cbd_mins / car_travel_time_cbd_mins)
# chk <- inner_join(od_cbd_sa2, times, by = "sa2_code") %>% filter(cbd_car_driver + cbd_pt >= 30)
# cor.test(chk$ratio, chk$cbd_pt_share, method = "spearman", exact = FALSE)
# -> Negative rho = where PT is relatively slower, fewer CBD commuters take it.
# (>= 30 trips: avoids noisy shares from tiny, RR3-rounded counts)


#  Map: CBD-bound drivers (count) and PT share of CBD-bound trips
map_data <- travel_props %>%
  dplyr::select(sa2_code) %>%
  left_join(od_cbd_sa2, by = "sa2_code") %>%
  mutate(cbd_pt_pct = 100 * if_else(cbd_car_driver + cbd_pt >= 30, cbd_pt_share, NA_real_))

cbd_outline <- map_data %>% filter(in_cbd) %>% st_union()

map_drivers <- tm_shape(map_data) +
  tm_polygons(fill = "cbd_car_driver",
              fill.scale = tm_scale_intervals(style = "quantile", n = 5,
                                              values = "brewer.reds",
                                              value.na = "grey60", label.na = "City centre",
                                              label.format = list(digits = 0)),
              fill.legend = tm_legend(title = "Car drivers\nto city centre",
                                      position = tm_pos_in("left", "bottom")),
              col = "white", lwd = 0.1) +
  tm_shape(cbd_outline) + tm_borders(col = "black", lwd = 1) +
  tm_layout(frame = FALSE)

map_pt <- tm_shape(map_data) +
  tm_polygons(fill = "cbd_pt_pct",
              fill.scale = tm_scale_intervals(style = "quantile", n = 5,
                                              values = "brewer.blues",
                                              value.na = "grey60",
                                              label.na = "City centre / < 30 trips",
                                              label.format = list(digits = 0)),
              fill.legend = tm_legend(title = "PT share of\ncity-centre trips (%)",
                                      position = tm_pos_in("left", "bottom")),
              col = "white", lwd = 0.1) +
  tm_shape(cbd_outline) + tm_borders(col = "black", lwd = 1) +
  tm_compass(type = "arrow", position = c("right", "top"), size = 1.5) +
  tm_scalebar(breaks = c(0, 5, 10), text.size = 0.6, position = c("right", "top")) +
  tm_layout(frame = FALSE)

(map_od <- tmap_arrange(map_drivers, map_pt, ncol = 2))
tmap_save(map_od, "all_figures/fig_od_cbd_flows.png",
          width = 24, height = 14, units = "cm", dpi = 300)




# What share of all CBD-bound car drivers live in the study area?
# This part of the code is important in my study design section of my research thesis
all_in  <- od %>% filter(dest_code %in% cbd_codes, !origin_code %in% cbd_codes)
study   <- all_in %>% filter(origin_code %in% travel_props$sa2_code)
round(sum(study$car_driver) / sum(all_in$car_driver), 2)


# Methods sentence:
# "Commuter flows to the city centre were derived from the 2023 Census
#  origin-destination table of main means of travel to work (Stats NZ). The
#  city centre was defined as the SA2s whose centroids fall within the Auckland
#  City Centre SA3. Flows originating inside the city centre were excluded, as
#  these trips would not cross a cordon. Suppressed cells were treated as zero."





# -----------------------------------------------------------------------------
# NOTES (for me only, not a thesis figure)
# Where do CBD-bound car drivers come from, incl. outside the study area?
# -----------------------------------------------------------------------------
# Commented out: interactive notes maps (map_all / study_outline are not built in this script)
# dir.create("Notes_Hinami_figures", showWarnings = FALSE)   # makes the folder if it doesn't exist

# tmap_mode("view")
# notes_cbd_drivers_map <- tm_shape(map_all) +
#   tm_polygons(fill = "cbd_car_drivers",
#               fill.scale = tm_scale_intervals(style = "quantile", n = 5, values = "brewer.reds"),
#               fill.legend = tm_legend(title = "Car drivers to city centre"),
#               col = "white", lwd = 0.2,
#               popup.vars = c("sa2_name", "cbd_car_drivers", "in_study")) +
#   tm_shape(study_outline) +
#   tm_borders(col = "black", lwd = 2)

# notes_cbd_drivers_map   # show it in RStudio

# tmap_save(notes_cbd_drivers_map,
#           "all_figures/Notes_Hinami_figures/notes_cbd_drivers_all_origins.html")








# Desire lines: SA2 centroid -> city-centre centroid, width = car drivers
# cent <- travel_props %>% st_centroid() %>% dplyr::select(sa2_code)
# cbd_point <- cbd_sa3 %>% st_centroid() %>% st_geometry()

# lines_sf <- od_cbd_sa2 %>%
#   filter(!in_cbd, cbd_car_driver > 0) %>%
#   left_join(cent %>% mutate(x = st_coordinates(.)[,1], y = st_coordinates(.)[,2]) %>%
#               st_drop_geometry(), by = "sa2_code") %>%
#   rowwise() %>%
#   mutate(geometry = st_sfc(st_linestring(rbind(c(x, y), st_coordinates(cbd_point)[1, 1:2])),
#                            crs = st_crs(travel_props))) %>%
#   ungroup() %>% st_as_sf()

# tmap_mode("view")
# (notes_desire_lines <- tm_shape(travel_props) + tm_polygons(fill = "grey95", col = "white", lwd = 0.2) +
#   tm_shape(lines_sf %>% arrange(cbd_car_driver)) +
#   tm_lines(col = "cbd_car_driver", col.scale = tm_scale_intervals(style = "quantile", n = 5, values = "brewer.reds"),
#            lwd = "cbd_car_driver", lwd.scale = tm_scale_continuous(values.scale = 5),
#            col.legend = tm_legend(title = "Car drivers to city centre"),
#            popup.vars = c("sa2_name", "cbd_car_driver", "cbd_pt")))

# tmap_save(notes_desire_lines, "Notes_Hinami_figures/notes_desire_lines_cbd.html")