# Congestion pricing in Auckland

This is the R code for my GEOG 789 Honours dissertation at the University of Auckland (2026).

Hinami Nishi, School of Environment

## About the project

Auckland Council has been looking at a congestion charge for the city centre. My
dissertation uses the 2023 Census at SA2 level to look at who would be affected. I had
three research questions:

1. Where in urban Auckland do people mostly drive to work, and do those areas cluster
   together? (scripts 02 and 03)
2. How do income, home ownership, occupation and travel time relate to car commuting,
   and does this change across the city? I used an OLS regression first and then a
   geographically weighted regression (GWR). (scripts 04a to 07.1)
3. If there was a cordon charge, who would pay it, how much would it be compared with
   their income, and could they take public transport instead? (scripts 02b and 08)

## Folders

- `Scripts/clean/` has the analysis scripts. They are numbered in the order you run them,
  and there is a separate README in that folder.
- `data/` is where the input data goes. Most of it isn't in this repo because it's large
  and you can download it from Stats NZ. `data/README.md` lists what you need.
- `all_figures/` has the figures the scripts make.

## Running it

Open `Transport-Behaviour.Rproj` in RStudio so the file paths work. Download the data
listed in `data/README.md`, then install the packages:

```r
install.packages(c("sf", "tidyverse", "tmap", "spdep", "GWmodel", "car", "ppcor",
                   "patchwork", "flextable", "janitor", "foreign", "maptiles",
                   "leafsync", "httr", "jsonlite", "r5r", "rmarkdown", "callr"))
```

After that, restart R and source `Scripts/clean/00_run_all.R`. It runs everything in
order and saves a log in `Scripts/clean/logs/`. It took about 5 to 10 minutes on my laptop.

I calculated the travel times with r5r, which needs Java and takes a while, so the
scripts read them from the CSV files in `data/` instead of routing again. The routing
code is still in scripts 04a and 04c if you want to rerun it.

I used R 4.5.2 on Windows 11.

## Data sources

- Census, boundary and travel-to-work data are from Stats NZ (CC BY 4.0).
- The bus, train and ferry timetable (GTFS) is from Auckland Transport.
- The road and walking network is from OpenStreetMap, downloaded through BBBike.
- The background map tiles are Esri World Gray Canvas.

Script 04b gets peak-hour car times from the TravelTime API. I didn't end up using
these in the dissertation because I couldn't confirm I was allowed to publish them, so
the results aren't included here.
