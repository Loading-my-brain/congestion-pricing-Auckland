# =============================================================================
# 04a_pt_times_r5r.R
# -----------------------------------------------------------------------------
# PURPOSE  Public transport travel time from each SA2 to the CBD (r5r).
#
#   PART A  RUN ONCE NOW: export the PT times you already calculated to a CSV.
#           This cuts the dependency on the old 503-SA2 .gpkg files.
#   PART B  DOCUMENTATION ONLY (inside if (FALSE) {...}, so it never runs):
#           how the PT times were produced, so the method is on record and
#           can be reproduced.
#
# OUTPUT   data/pt_time_cbd_r5r_20260513_0800.csv
#            sa2_code                  SA2 code (text)
#            pt_time_cbd_mins          MAIN variable: 30-min walk rule (6 NA)
#            pt_time_cbd_mins_walk90   SENSITIVITY: 6 NA filled with 90-min walk
#
# SETTINGS (method summary for the thesis)
#   Tool        r5r (Pereira et al. 2021), Java 21
#   Network     BBBike Auckland OSM extract (downloaded 10 Aug 2026)
#               + Auckland Transport GTFS (valid 1 May - 31 Jul 2026)
#   Origins     SA2 centroids (497 SA2s)
#   Destination centroid of City Centre SA3 (51150)
#   Departure   8:00 am, Wednesday 13 May 2026 (mid-week, school term)
#   Limits      max walk 30 min (whole trip); max trip 240 min
#   Result      travel_time_p50 (median), minutes
#
# NEXT     04b (already done), 04c (optional check), then 05_travel_times.R
# =============================================================================

library(sf)
library(tidyverse)

pt_csv <- "data/pt_time_cbd_r5r_20260513_0800.csv"



# Understanding the data -------------
gtfs_zip <- "data/r5_clean/gtfs.zip"

# Official feed validity (if AT included feed_info.txt)
read.csv(unz(gtfs_zip, "feed_info.txt"))

# Date range actually covered by the timetable
cal <- read.csv(unz(gtfs_zip, "calendar.txt"))
range(as.Date(as.character(cal$start_date), "%Y%m%d"))
range(as.Date(as.character(cal$end_date),   "%Y%m%d"))


# =============================================================================
# PART A - export existing PT times (run once, BEFORE running 05)
# =============================================================================
# travel_props_Times.gpkg (saved by the old 3.1 script) holds the final PT times:
# original run + the 3 SA2s re-routed with a 240-min limit + the 90-min-walk
# sensitivity values. 05_travel_times.R will later OVERWRITE that .gpkg, which
# is why the times are copied to a CSV first.

if (!file.exists(pt_csv)) {
  old <- st_read("./data/Saved/travel_props_Times.gpkg", quiet = TRUE) %>%
    st_drop_geometry()

  stopifnot(nrow(old) == 497) # the 497-SA2 version
  stopifnot(sum(is.na(old$PT_transit_time_cbd_mins)) == 6) # after the retries

  pt_times <- old %>%
    transmute(sa2_code = as.character(usual_residence_statistical_area_2_code_na),
              pt_time_cbd_mins = PT_transit_time_cbd_mins,
              pt_time_cbd_mins_walk90 = PT_time_sensitivity)

  write.csv(pt_times, pt_csv, row.names = FALSE)
  message("Saved ", pt_csv)
} else {
  message(pt_csv, " already exists - nothing to do.")
}

# Check the CSV
pt_times <- read.csv(pt_csv, colClasses = c("character", "numeric", "numeric"))
nrow(pt_times) # 497
colSums(is.na(pt_times)) # 6 / 0
summary(pt_times$pt_time_cbd_mins) # median 66, IQR 50-80, max 151



# Appendix SA2s with no stop within a 30-min walk (sensitivity check)
sa2_names <- st_read("./data/Saved/travel_props.gpkg", quiet = TRUE) %>%
  st_drop_geometry() %>%
  transmute(sa2_code = as.character(usual_residence_statistical_area_2_code_na),
            sa2_name = usual_residence_statistical_area_2_name_na)



table_appendix_no_stop_30min<- pt_times %>%
  filter(is.na(pt_time_cbd_mins)) %>%
  left_join(sa2_names, by = "sa2_code") %>%
  dplyr::select(SA2 = sa2_name, `PT time with up to 90-min walk (min)` = pt_time_cbd_mins_walk90) %>%
  arrange(`PT time with up to 90-min walk (min)`)

# Appendix: SA2s with no stop within a 30-min walk (sensitivity check)
library(flextable)
tab_no_stop <- table_appendix_no_stop_30min |>   
  dplyr::rename(`PT travel time (min)` = `PT time with up to 90-min walk (min)`)
ft <- flextable(tab_no_stop) |>
  theme_apa() |>
  autofit()
save_as_docx(ft, path = "./all_figures/Appendix/table_A1_no_stop_30min.docx")

 
# PART B - how the PT times were produced (documentation, does not run) 
if (FALSE) {

  # --- Java set-up --------------------------------------------------------------
  # Java memory is set in ~/.Renviron with the line:  _JAVA_OPTIONS=-Xmx3G
  # (options(java.parameters = ...) did not work because the project .Rprofile
  #  starts Java before any script runs.) Check after restarting R:
  #  rJava::J("java.lang.Runtime")$getRuntime()$maxMemory() / 1e9   # ~3.2
  library(r5r)

  # --- Network ------------------------------------------------------------------
  # data/r5_clean contains ONLY bbbike_Auckland.osm.pbf + gtfs.zip.
  # (The full NZ .pbf needed ~32 GB RAM, so a city extract is used.)
  # build_network() loads the cached network.dat if it exists.
  r5r_network <- build_network(data_path = "data/r5_clean")
  # Note: 447 of ~6,000 GTFS stops could not be linked to streets (r5r-log.log)

  # --- Origins and destination ---------------------------------------------------
  travel_props <- st_read("./data/Saved/travel_props.gpkg", quiet = TRUE)

  origins <- travel_props %>%
    st_transform(2193) %>% st_centroid() %>% st_transform(4326) %>%
    mutate(id = usual_residence_statistical_area_2_code_na,
           lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) %>%
    st_drop_geometry() %>% select(id, lon, lat)

  destination <- st_read("data/boundary/sa3/statistical-area-3-2023-clipped-generalised.shp",
                         quiet = TRUE) %>%
    filter(SA32023_V1 == "51150") %>%
    st_transform(2193) %>% st_centroid() %>% st_transform(4326) %>%
    { data.frame(id = "CBD", lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) }

  dep <- as.POSIXct("2026-05-13 08:00:00", tz = "Pacific/Auckland")

  # --- Main run: 30-min walk -------------------------------------------------------
  # (Originally run with max_trip_duration = 120; the 3 SA2s over 120 min were
  #  re-routed with 240. Times under 120 min are identical either way.)
  pt_main <- travel_time_matrix(r5r_network, origins = origins, destinations = destination,
                                mode = c("WALK", "TRANSIT"), departure_datetime = dep,
                                max_walk_time = 30, max_trip_duration = 240, verbose = FALSE)
  # -> 491 SA2s reached. 3 needed > 120 min: Hingaia (151), Waima-Woodlands Park (138),
  #    Laingholm (138). 6 have no stop within a 30-min walk.

  # --- Sensitivity: up to 90-min walk for the 6 unreached SA2s ----------------------
  still_na <- origins %>% filter(!id %in% pt_main$from_id)
  pt_walk90 <- travel_time_matrix(r5r_network, origins = still_na, destinations = destination,
                                  mode = c("WALK", "TRANSIT"), departure_datetime = dep,
                                  max_walk_time = 90, max_trip_duration = 240, verbose = FALSE)
  # -> Middlemore 109, Harbour View Heights 167, Bremner Ridge 131, Tuscany Heights 136,
  #    Ormiston South East 135, Massey Red Hills 100 (all residential fringe, except
  #    Middlemore = hospital). Used ONLY as a sensitivity check.

  stop_r5(r5r_network)
}
