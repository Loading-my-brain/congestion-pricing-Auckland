# =============================================================================
# 04c_car_freeflow_check.R
# -----------------------------------------------------------------------------
# PURPOSE  MAIN CAR TRAVEL TIMES for the thesis (since Oct 2026): free-flow
#          (no traffic) car times from r5r on the same OSM network as PT,
#          SA2 -> CBD. Replaces the TravelTime API times, whose licence for
#          publication could not be confirmed.
#
#   PART A  r5r routing - ALREADY DONE (skipped if the CSV exists)
#   PART B  Internal check only: how do free-flow times compare with the
#           (unpublishable) TravelTime peak times? Do NOT report TravelTime
#           numbers in the thesis unless the licence is confirmed.
#
# INPUT    data/Saved/travel_props.gpkg, data/r5_clean/ (network)
#          data/car_time_cbd_20260513_0800.csv           (from 04b)
#          data/car_time_cbd_r5r_freeflow.csv            (outbound CBD -> SA2, old run)
# OUTPUT   data/car_time_cbd_r5r_freeflow_inbound.csv   (SA2 -> CBD)
#
# JAVA     Needs ~3 GB: put  _JAVA_OPTIONS=-Xmx3G  in ~/.Renviron, restart R.
#          With the default ~1 GB, CAR routing fails with OutOfMemoryError.
#
# THESIS   Methods (validation sentence) + Appendix (optional scatter plot)
# =============================================================================

library(sf)
library(tidyverse)

freeflow_csv <- "data/car_time_cbd_r5r_freeflow_inbound.csv"


 # PART A - r5r free-flow routing (run once; ~ a few minutes)
 
if (!file.exists(freeflow_csv)) {
  library(r5r)
  rJava::J("java.lang.Runtime")$getRuntime()$maxMemory() / 1e9   # should be ~3.2

  r5r_network <- build_network(data_path = "data/r5_clean")

  origins_all <- st_read("./data/Saved/travel_props.gpkg", quiet = TRUE) %>%
    st_transform(2193) %>% st_centroid() %>% st_transform(4326) %>%
    mutate(id = usual_residence_statistical_area_2_code_na,
           lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) %>%
    st_drop_geometry() %>% select(id, lon, lat)

  destination <- st_read("data/boundary/sa3/statistical-area-3-2023-clipped-generalised.shp", quiet = TRUE) %>%
    filter(SA32023_V1 == "51150") %>%
    st_transform(2193) %>% st_centroid() %>% st_transform(4326) %>%
    { data.frame(id = "CBD", lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) }

  # Batches of 25 and one thread keep memory use low
  batches <- split(origins_all, ceiling(seq_len(nrow(origins_all)) / 25))
  car_in <- map_dfr(seq_along(batches), function(i) {
    message("Batch ", i, " of ", length(batches))
    out <- travel_time_matrix(r5r_network, origins = batches[[i]], destinations = destination,
                              mode = "CAR", max_trip_duration = 90, n_threads = 1)
    gc()
    out
  }) %>%
    select(SA22023_V1_00 = from_id, car_time_r5r_inbound = travel_time_p50)

  write.csv(car_in, freeflow_csv, row.names = FALSE)
  stop_r5(r5r_network)
}


 
# PART B - compare free-flow (r5r) with peak (TravelTime)
car_in  <- read.csv(freeflow_csv, colClasses = c("character", "numeric"))
car_out <- read.csv("data/car_time_cbd_r5r_freeflow.csv", colClasses = c("character", "numeric"))
car_tt  <- read.csv("data/car_time_cbd_20260513_0800.csv", colClasses = c("character", "numeric")) %>%
  rename(SA22023_V1_00 = 1, car_time_traveltime = 2)

nrow(car_in) # 497: every SA2 reached
summary(car_in$car_time_r5r_inbound) # median 20, IQR 16-24, range 2-39 (empty roads)

car_check <- car_in %>%
  left_join(car_out, by = "SA22023_V1_00") %>%
  left_join(car_tt,  by = "SA22023_V1_00") %>%
  mutate(dir_diff         = car_time_r5r_inbound - car_time_r5r_freeflow,
         congestion_ratio = car_time_traveltime / car_time_r5r_inbound)

# Direction: inbound (SA2 -> CBD) minus outbound (CBD -> SA2)
summary(car_check$dir_diff)
# median +1 min (range -2 to +5): inbound slightly longer (one-ways, ramps), small.

#  Peak vs free-flow
summary(car_check$congestion_ratio)
# median 1.45 (IQR 1.27-1.62): a typical 8 am trip takes ~45% longer than on empty roads.

cor(car_check$car_time_traveltime, car_check$car_time_r5r_inbound, method = "spearman")
# rho = 0.94: both sources rank SA2s almost identically -> TravelTime times are credible.

# Ratios below 1 (TravelTime faster than empty roads?)
car_check %>% filter(congestion_ratio < 1) %>% arrange(congestion_ratio)
# 13 SA2s, all within ~11 min of the CBD; absolute gaps <= 4.2 min.
# -> Artefact of rounding (r5r = whole minutes) and different road snapping on very
#    short trips, not real "negative congestion". Values kept unchanged.

# Methods sentence (ONLY if the TravelTime licence is confirmed):
# "Peak-period car travel times were validated against free-flow times computed in
#  r5r on the same OpenStreetMap network (SA2 -> CBD). The two were strongly
#  correlated (Spearman rho = 0.94), and peak times were a median of 1.45 times
#  free-flow times (IQR 1.27-1.62), consistent with morning congestion."



# Original network folder used in August
file.info(list.files("data/r5_network", pattern = "\\.pbf$", full.names = TRUE))[, c("size", "mtime")]

# Any other copies anywhere in your project
pbfs <- list.files(".", pattern = "\\.osm\\.pbf$", recursive = TRUE, full.names = TRUE)
file.info(pbfs)[, c("size", "mtime")]
