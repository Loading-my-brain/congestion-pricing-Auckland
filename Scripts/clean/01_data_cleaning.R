# =============================================================================
# 01_data_cleaning.R

# PURPOSE  Read the 2023 Census (individuals, SA2, part 2), give columns
#          readable names, clip to urban Auckland, replace Stats NZ
#          confidentiality codes with NA, and make the study area map.
#
# INPUT    data/SA2-part2/2023-census-totals-by-topic-for-individuals-by-statistical-a.shp
#          data/SA2-part2/2023_census_totals_by_topic_for_individuals_by_sa2_part_2_lookup_table.csv
#          data/boundary/urban-rural-2023-clipped-generalised/urban-rural-2023-clipped-generalised.shp
#
# OUTPUT   data/Saved/sa2_clean.gpkg      (all urban-Auckland SA2s, all 2023 variables)
#          all_figures/fig_study_area.png
#
# NEXT     02_mode_shares_RQ1.R
# =============================================================================

library(sf)
library(tidyverse)   # dplyr, stringr, tidyr, ggplot2 ...
library(janitor)     # make_clean_names()
library(tmap)
library(maptiles)    # basemap tiles without an API key



# 1. Read the Census data and the lookup table

# Source: Stats NZ, "2023 Census totals by topic for individuals by SA2 - part 2".
# The shapefile has short, cryptic column names (e.g. "C23_Trv_W_1");
# the lookup table says what each one means.
sa2_raw <- st_read("./data/SA2-part2/2023-census-totals-by-topic-for-individuals-by-statistical-a.shp",
                   quiet = TRUE)
look_up <- read.csv("./data/SA2-part2/2023_census_totals_by_topic_for_individuals_by_sa2_part_2_lookup_table.csv")



# 2. Keep 2023 variables only and give them readable names

# Rows with Year = NA are geographic fields (SA2 code, name, area) -> keep them.
look_up_2023 <- look_up %>%
  filter(Year == 2023 | is.na(Year)) %>%
  mutate(
    descriptive_name = case_when(
      !is.na(Variable1) & !is.na(Variable1_category) ~ paste(Variable1, Variable1_category, Year, sep = "_"),
      !is.na(Field_name_alias)                       ~ paste(Field_name_alias, Year, sep = "_"),
      TRUE                                           ~ Shapefile_name),
    clean_name = make_clean_names(descriptive_name))   # e.g. "..._public_bus_2023"

# Named vector: c("old_name" = "new_name")
rename_map <- setNames(look_up_2023$clean_name, look_up_2023$Shapefile_name)

spatial_cols <- c("SA22023_V1", "SA22023__1", "SA22023__2", "AREA_SQ_KM", "LAND_AREA_")

sa2_renamed <- sa2_raw %>%
  select(any_of(c(spatial_cols, names(rename_map)))) %>%            # drops 2013 / 2018 columns
  rename_with(~ unname(rename_map[.]), .cols = any_of(names(rename_map)))

"LAND_AREA_" %in% colnames(sa2_renamed)   # TRUE: land area kept
nrow(sa2_renamed) == nrow(sa2_raw) # TRUE: no rows lost



# 3. Clip to the study area: Auckland "Major urban area"

# Stats NZ Urban Rural 2023 classification. Using the major urban area (rather
# than the whole Auckland Council area) removes rural SA2s and the Gulf islands.
akl_urban <- st_read("./data/boundary/urban-rural-2023-clipped-generalised/urban-rural-2023-clipped-generalised.shp",
                     quiet = TRUE) %>%
  filter(IUR2023__1 == "Major urban area") %>%
  st_transform(st_crs(sa2_renamed))

sa2_clipped <- st_intersection(sa2_renamed, akl_urban) %>%
  st_make_valid() %>%                       # repair invalid shapes after clipping
  st_collection_extract("POLYGON") %>%      # drop stray points / lines on the edges
  st_cast("MULTIPOLYGON") %>%
  filter(!st_is_empty(.))



# 4. Replace confidentiality codes with NA

# Stats NZ uses negative codes (e.g. -999 = confidential) instead of values.
# Left in, they produce impossible results (e.g. negative mode shares).
sa2_clean <- sa2_clipped %>%
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x)))

nrow(sa2_clean)   # number of urban-Auckland SA2s before any are dropped



# 5. Keep travel-to-work by USUAL RESIDENCE only and shorten its names

# The Census has four travel tables: to work / education, by home / destination.
# This study uses travel to WORK by place of USUAL RESIDENCE (where people live).
sa2_clean <- sa2_clean %>%
  select(!contains(c("main_means_of_travel_to_education_by_usual_residence_address",
                     "main_means_of_travel_to_education_by_education_address",
                     "main_means_of_travel_to_work_by_workplace_address"))) %>%
  rename_with(~ .x %>%
                str_remove("^main_means_of_travel_to_work_by_usual_residence_address_") %>%
                str_remove("_2023$") %>%
                paste0("transportMode_", .),
              .cols = starts_with("main_means_of_travel_to_work_by_usual_residence_address_"))

# NOTE: this is built from the CLEANED object (step 4). An earlier version built it
# from the uncleaned object, which caused negative proportions (-111).

st_write(sa2_clean, "./data/Saved/sa2_clean.gpkg", delete_dsn = TRUE, quiet = TRUE)



# 6. Study area map (Methods figure)

aoi_outline <- sa2_clean %>% st_union() %>% st_as_sf()

bm <- get_tiles(sa2_clean, provider = "Esri.WorldGrayCanvas", zoom = 11, crop = TRUE)

tmap_mode("plot")
study_area_map <-
  tm_shape(bm) + tm_rgb() +
  tm_shape(sa2_clean, is.main = TRUE) +
  tm_polygons(fill = "#4C9BE8", fill_alpha = 0.35, col = "white", lwd = 0.3) +
  tm_shape(aoi_outline) +
  tm_borders(col = "#1F4E79", lwd = 2) +
  tm_add_legend(type = "polygons", labels = "Study area (SA2s, urban Auckland)",
                fill = "#4C9BE8", fill_alpha = 0.35, col = "#1F4E79",
                position = c("left", "bottom")) +
  tm_compass(type = "arrow", position = c("right", "top"), size = 2) +
  tm_scalebar(breaks = c(0, 5, 10, 20), text.size = 0.7, position = c("left", "bottom")) +
  tm_layout(frame = FALSE)

study_area_map
tmap_save(study_area_map, "all_figures/fig_study_area.png",
          width = 16, height = 18, units = "cm", dpi = 300)
# Caption credit: "Basemap: Esri, HERE, Garmin, (c) OpenStreetMap contributors.
#                  Boundaries: Stats NZ (2023)."
