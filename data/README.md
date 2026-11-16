# Data

Most of the input data isn't in this repo because the files are big and you can download
them for free. Put each one in the folder listed below. The scripts create everything in
`data/Saved/` themselves.

## What to download

From Stats NZ Datafinder (all 2023 versions):

- 2023 Census totals by topic for individuals by SA2, part 2 (shapefile and lookup table)
  into `data/SA2-part2/`
- Urban Rural 2023 (clipped, generalised) into `data/boundary/urban-rural-2023-clipped-generalised/`
- Statistical Area 3 2023 (clipped, generalised) into `data/boundary/sa3/`. I used SA3
  51150 (Auckland City Centre) as the city centre.
- Territorial Authority 2023 (clipped, generalised) into
  `data/territorial-authority-2023-clipped-generalised/`
- 2023 Census main means of travel to work by statistical area (the origin–destination
  table, .dbf) into `data/OD-data/`

You only need these if you want to redo the travel time routing (scripts 04a and 04c):

- An Auckland OpenStreetMap extract (.osm.pbf) from BBBike. Mine was downloaded on
  10 August 2026.
- The Auckland Transport GTFS timetable, saved as `gtfs.zip`. Mine was valid from 1 May
  to 31 July 2026.

Both go in `data/r5_clean/`.

## What's included

These are travel times I calculated myself, so you can run the analysis without Java or r5r:

- `pt_time_cbd_r5r_20260513_0800.csv` has the PT time from each SA2 to the city centre,
  leaving at 8 am on Wednesday 13 May 2026, with a 30-minute walking limit (from 04a).
- `car_time_cbd_r5r_freeflow_inbound.csv` has the free-flow driving time from each SA2 to
  the city centre (from 04c). These are the car times used in the dissertation.
- `car_time_cbd_r5r_freeflow.csv` is an earlier run going the other way (city centre to
  SA2). Script 04c compares the two.

The TravelTime API results aren't included because I couldn't confirm I was allowed to
share them.
