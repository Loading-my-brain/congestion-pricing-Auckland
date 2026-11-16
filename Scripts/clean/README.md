# Analysis scripts

Run these in number order. I restart R (Ctrl + Shift + F10) before each one so nothing
carries over. You can also just source `00_run_all.R`, which runs them all and saves a log.
Each script has a header at the top saying what it reads in and what it saves.

| Script | What it does | Saves |
|---|---|---|
| 00_run_all.R | Runs 01 to 08 in order | a log in `logs/` |
| 01_data_cleaning.R | Reads the 2023 Census data, renames columns, clips to urban Auckland | `sa2_clean.gpkg`, study area map |
| 02_mode_shares_RQ1.R | Works out car, PT and other mode shares for each SA2 | `travel_props.gpkg` (497 SA2s) |
| 02b_od_cbd_flows.R | Counts how many people from each SA2 commute into the city centre, and how | `od_cbd_by_sa2.csv` |
| 03_spatial_clustering_RQ1.R | Moran's I and LISA to find clusters of car commuting | adds cluster columns to `travel_props.gpkg` |
| 04a_pt_times_r5r.R | PT travel time from each SA2 to the city centre | `pt_time_cbd_r5r_20260513_0800.csv` |
| 04b_car_times_traveltime_api.R | Peak car times from TravelTime. Not used, kept as a record | – |
| 04c_car_freeflow_check.R | Free-flow car times from r5r (the car times I used) | `car_time_cbd_r5r_freeflow_inbound.csv` |
| 05_travel_times.R | Joins the PT and car times to the SA2s and maps them | `travel_props_Times.gpkg` |
| 06_predictors_RQ2.R | Makes the predictor variables and the regression dataset | `travel_props_vars.gpkg`, `model_data.gpkg` (490 SA2s) |
| 07_ols_gwr_RQ2.R | OLS regression, then GWR, and compares them | `gwr_results.gpkg` |
| 07.1_GWR_CoefMaps.R | Maps of the local GWR coefficients | coefficient maps |
| 08_rq3_charge_burden.R | Charge as a share of income, and whether there's a PT alternative | `rq3_by_sa2.csv`, RQ3 figures |
| 09_interactive_travel_time_map.R | Optional web map of travel times | `interactive_travel_times.html` |
| 99_render_scripts_to_word.R | Turns each script into a Word document with its output | `printouts/` |

09 and 99 aren't in `00_run_all.R`, so run them separately if you want them.

The `.gpkg` and `.csv` outputs go into `data/Saved/`.

## Number of SA2s

The number of SA2s changes between steps:

- 497 SA2s in most of the analysis. SA2s where Stats NZ suppressed the travel-to-work
  counts are removed.
- 491 for anything using PT times, because 6 SA2s have no stop within a 30-minute walk.
- 496 for anything using income, because Wiri West is mostly the prison, so I set its
  income to NA.
- 490 in the regression models, after removing both of those.
