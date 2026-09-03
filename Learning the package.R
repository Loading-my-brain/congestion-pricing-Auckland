#https://ipeagit.github.io/r5r/


# from CRAN
install.packages("r5r")

# dev version with latest features
utils::remove.packages('r5r')
devtools::install_github("ipeaGIT/r5r", subdir = "r-package")





# allocate RAM memory to Java **before** loading the {r5r} library
options(java.parameters = "-Xmx2G")

library(r5r)

# 1) build transport network, pointing to the path where OSM and GTFS data are stored
path <- system.file("extdata/poa", package = "r5r")
r5r_network <- build_network(data_path = path, verbose = FALSE)

# 2) load origin/destination points and set arguments
points <- read.csv(system.file("extdata/poa/poa_hexgrid.csv", package = "r5r"))
mode <- c("WALK", "TRANSIT")
max_walk_time <- 30   # minutes
max_trip_duration <- 60 # minutes
departure_datetime <- as.POSIXct("13-05-2019 14:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

# 3.1) calculate a travel time matrix
ttm <- travel_time_matrix(r5r_network = r5r_network,
                          origins = points,
                          destinations = points,
                          mode = mode,
                          departure_datetime = departure_datetime,
                          max_walk_time = max_walk_time,
                          max_trip_duration = max_trip_duration)

# 3.2) or get detailed info on multiple alternative routes
det <- detailed_itineraries(r5r_network = r5r_network,
                            origins = points[370, ],
                            destinations = points[200, ],
                            mode = mode,
                            departure_datetime = departure_datetime,
                            max_walk_time = max_walk_time,
                            max_trip_duration = max_trip_duration,
                            shortest_path = FALSE,
                            drop_geometry = FALSE)

# 4) Calculate number of schools accessible within 20 minutes 
access <- accessibility(r5r_network = r5r_network,
                        origins = points,
                        destinations = points,
                        opportunities_colname = "schools",
                        decay_function = "step",
                        cutoffs = 21,
                        mode =  c("WALK", "TRANSIT"),
                        departure_datetime = departure_datetime,
                        verbose = FALSE)