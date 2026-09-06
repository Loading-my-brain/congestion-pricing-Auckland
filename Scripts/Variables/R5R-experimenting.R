# From the webapge -- copy and pasted 
# from CRAN
install.packages('r5r')

# dev version with latest features
devtools::install_github("ipeaGIT/r5r", subdir = "r-package")

# install {rJavaEnv} from CRAN
install.packages("rJavaEnv")
install.packages("rJava")
rJavaEnv::rje_consent(provided = TRUE)

# check version of Java currently installed (if any) 
rJavaEnv::java_check_version_rjava() #I need to restart R session before running this

## if this is the first time you use {rJavaEnv}, you might need to run this code
## below to consent the installation of Java.
rJavaEnv::rje_consent(provided = TRUE)

# install Java 21
rJavaEnv::java_quick_install(version = 21)

# check if Java was successfully installed
rJavaEnv::java_check_version_rjava()

options(java.parameters = "-Xmx2G")

# By default, {r5r} uses all CPU cores available. If you want to limit the 
# number of CPUs to 4, for example, you can run:  
options(java.parameters = c("-Xmx2G", "-XX:ActiveProcessorCount=4"))

library(r5r)
library(sf)
library(data.table)
library(ggplot2)
# Copy and paste ends here -----------------------------------------------------


# 3.1 Data requirements (on the webapge)
# 1. A road network data set from OpenStreetMap in .pbf format (mandatory) 
  # - I have downloaded the data from https://download.geofabrik.de/australia-oceania/new-zealand.html
  # 2026-08-06T20:21:21Z
  # Meta data: https://osm-internal.download.geofabrik.de/australia-oceania/new-zealand.html
  # Update, I have dicthed this data set becuase the file was wayyyy to large for it to load in my computer I'd need a 32 GB RAM apaprently. 
  # So, like the example on the webpage, I have decided to use osmextract R package as recommeded by the `r5r` package. 
# 2. A public transport feed in GTFS.zip format (optional)
# 3. A raster file of Digital Elevation Model data in .tif format (optional)








library(osmextract)
library(sf)

#The Major Urban Area in Auckland - boundary we're using to filter the gtf data
boundary = st_read("./data/boundary/urban-rural-2023-clipped-generalised/urban-rural-2023-clipped-generalised.shp")
akl_boundary = boundary %>%# Filter to Auckland
  filter(IUR2023__1  == "Major urban area")
akl_boundary_4326 <- st_transform(akl_boundary, 4326) # Transform CRS to EPSG:4326 
#The Major Urban Area in Auckland - boundary we're using to filter the gtf data
boundary = st_read("./data/boundary/urban-rural-2023-clipped-generalised/urban-rural-2023-clipped-generalised.shp")
akl_boundary = boundary %>%# Filter to Auckland
  filter(IUR2023__1  == "Major urban area")
akl_boundary_4326 <- st_transform(akl_boundary, 4326) # Transform CRS to EPSG:4326 

# Set osmextract's download directory to my network folder
Sys.setenv(OSMEXT_DOWNLOAD_DIRECTORY = "data/r5_network")

#Crop and save the .pbf file
pbf_cropped <- oe_get(
  "New Zealand",
  boundary = akl_boundary_4326,
  download_only = TRUE,
  skip_vectortranslate = TRUE
)
list.files("data/r5_network")

# Point to the folder containing my .pbf and .zip files
# r5r_network <- build_network(data_path = "data/r5_network")
# r5r_network

# THIS DIDN'T WORK; the biggest reason is because my laptop ram size is 8GB. 
# OSM Map (.pbf):
#   - If you only allocate 2GB of RAM (-Xmx2G), my .pbf file must be under 20MB to 30MB to prevent Java from crashing[1].
#   - If you want to use the full New Zealand .pbf (380MB), you must allocate at least 8GB of RAM (-Xmx8G)[4]. (If my computer only has 8GB of physical RAM, you cannot allocate 8GB to R because Windows needs RAM to run).
# So the fix was to use Pre-Cropped Auckland Map (15MB) from the BBBike severe


#THE FIX
# Set osmextract's download directory to my network folder
Sys.setenv(OSMEXT_DOWNLOAD_DIRECTORY = "data/r5_network")

# Download the pre-cropped Auckland-only map from the BBBike provider (approx. 15MB)
# (download_only = TRUE keeps the raw .pbf file in my folder)
oe_get(
  "Auckland",
  provider = "bbbike",
  download_only = TRUE,
  skip_vectortranslate = TRUE
)




options(java.parameters = "-Xmx2G")

# By default, {r5r} uses all CPU cores available. If you want to limit the 
# number of CPUs to 4, for example, you can run:  
options(java.parameters = c("-Xmx2G", "-XX:ActiveProcessorCount=4"))

library(r5r)
library(sf)
library(data.table)
library(ggplot2)

# Point to the folder containing my .pbf and .zip files
r5r_network <- build_network(data_path = "data/r5_network", overwrite = TRUE, verbose = FALSE)
r5r_network
# omg! IT FINALLY WORKED!!!!!!!!! 




# But let's check if this actually worked 
auckland_streets <- oe_read(
  file = "data/r5_network/bbbike_Auckland.osm.pbf",
  layer = "lines"
)

# unique(auckland_streets$highway)
#Filter for major roads to keep the plot clean and fast
major_roads <- auckland_streets %>%
  filter(highway %in% c("motorway", "trunk", "primary", "secondary"))

#Plot the roads statically using base R
plot(
  st_geometry(major_roads), 
  col = "darkgrey", 
  lwd = 0.5, 
  main = "Auckland Major Road Network \n(from local PBF)"
)



--------------------------------------------------------------------------------
# NOW WE CAN ACTUALLY CALCULATE THE TRAVEL TIMES
--------------------------------------------------------------------------------
# Library Package
library(sp)
library(sf)
library(dplyr) 
library(tidyverse)
library(tmap)
library(grid)
library(gridExtra)
library(terra)
library(stringr)
library(r5r)

#Extract SA2 Centroids and project to WGS84 (EPSG:4326) for r5r
origins <- travel_income_HO_emp %>%
  st_centroid() %>%
  st_transform(4326) %>% # r5r requires WGS84 coordinates
  mutate(
    id = usual_residence_statistical_area_2_code_na,
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(id, lon, lat)

# Extract My  CBD Cordon Centroid in WGS84
cbd_centroid <- cbd_poly_nztm %>%
  st_centroid() %>%
  st_transform(4326)

destination <- data.frame(
  id = "CBD",
  lon = st_coordinates(cbd_centroid)[, 1],
  lat = st_coordinates(cbd_centroid)[, 2]
)


#Wed, May 13, 2026 8am peak hr - typical working day (middle of work week), no public holdiays or school holidays 
# This date and time is appropriate to define the typical hybrid working patterns.  Mondays and Fridays are considered more busy. 
departure_time <- as.POSIXct("2026-05-13 08:00:00", tz = "Pacific/Auckland")

#Calculate Public Transport Travel Times (Walk + Transit + Walk)
transit_times <- travel_time_matrix(
  r5r_network = r5r_network, # Matches the modern argument name
  origins = origins,
  destinations = destination,
  mode = c("WALK", "TRANSIT"), #he routing engine automatically scans my entire gtfs.zip file and activates every single public transport mode available in Auckland
  departure_datetime = departure_time,
  max_walk_time = 30,          
  max_trip_duration = 120,     
  verbose = FALSE
) %>%
  rename(transit_time_cbd_mins = travel_time_p50) %>% 
  select(from_id, transit_time_cbd_mins)

# Hinami's Notes ---------------------------------------------------------------
#mode = c("WALK", "TRANSIT"),
# What it does: This tells the Java routing engine to calculate a multimodal trip.
# Since humans cannot teleport directly to a bus stop, R5 must calculate a complete journey; WALK from the 
# neighborhood centroid to the nearest bus stop, train station, or ferry terminal.
# TRANSIT (ride the bus, train, or ferry).
# WALK from the final transit stop in the CBD to the exact center of my CBD box.

# max_walk_time = 30: 
#   This sets a logical threshold of 30 minutes of total walking time allowed for
# the entire trip (including walking to the first stop, transferring between services,
#                  and walking to the final destination). If a commute requires more than 
# 30 minutes of walking, R5 will discard it as unrealistic.

# max_trip_duration = 120: The maximum allowed travel time is 120 minutes (2 hours). 
# Any commute longer than 2 hours is excluded as non-viable.

# How did we decide these numbers 30 min walking time and max trip duration 120min? 

# rename(...): By default, r5r calculates the 50th percentile (the median) 
# travel time and names the column travel_time_p50. You rename this
# to transit_time_cbd_mins so you know exactly what it represents in my master dataset.



# CALCULATE MULTIMODAL PUBLIC TRANSPORT COMMUTE TIMES TO CBD (DOOR-TO-DOOR)
# Concept & Planning Justification:
# 1. Door-to-Door Journey: Rather than simply measuring station-to-station times,
#    r5r calculates a complete, seamless, multi-leg journey along Auckland's 
#    actual physical street and transit network.
#
# 2. Cumulative Travel Time: The final 'transit_time_cbd_mins' is the exact mathematical
#    sum of five distinct travel segments:
#    - Access Walk: Walking from the SA2 origin centroid to the first transit stop .
#    - In-Vehicle Time (Leg 1): Riding the first vehicle (e.g., local bus) .
#    - Transfer Walk & Wait: Walking between stops/terminals (e.g., bus to train) 
#      and waiting for the next scheduled service in the GTFS timetable.
#    - In-Vehicle Time (Leg 2): Riding the second vehicle (e.g., rail or ferry) .
#    - Egress Walk: Walking from the final CBD transit stop to the CBD destination centroid .
#
# 3. Walking Constraints (max_walk_time = 30): Sets a cumulative ceiling. Any route 
#    requiring more than 30 minutes of total walking (approx. 2.4 km at 4.8 km/h) 
#    across the entire trip is rejected as non-viable, reflecting Auckland Transport
#    and international pedestrian commuter tolerance thresholds .

# However, I have found an international paper(reserch paper title: Physical Activity Associated with Public Transport Use—
# A Review and Modelling of Potential Benefits) tht justifies the use of 20 min walking;
# To reflect realistic travel behavior, the maximum walk duration parameter (max_walk_time)
# was set to 20 minutes in the routing engine. Empirical transport planning literature
# indicates that cumulative walking time across multi-leg transit commutes rarely exceeds
# 15 to 20 minutes before commuter acceptability drops sharply and mode shift occurs 
# (e.g., O'Sullivan & Morrall, 1996; Guo & Wilson, 2011; Yang & Diez-Roux, 2012). 
# Capping walk legs at 20 minutes ensures that generated transit routes capture 
# realistic catchment areas without artificially inflating transit accessibility. 
#"Willingness to walk" is a human behavioral constraint (how far a person is willing to walk 
#before they refuse to use transit and choose to drive instead)
#Auckland Policy Alignment: Setting max_walk_time = 20 (which is about 1.6 km
#of walking at a standard 4.8 km/h pace) aligns with Auckland Council’s Plan Change 78 (PC78),
#which legally defines the walkable catchment of the CBD as exactly  1,200 m (a 15 -minute walk

#HINAMI wil change this from 30 to 20 min
# Journal of Transport Geography, Transportation Research Part A):
#   1. 10–15 minutes (800 m – 1,200 m) for standard bus/local transit.
#   2. 15–20 minutes (1,200 m – 1,600 m) for rapid transit/rail or total cumulative walking.
#
# 4. Temporal Validity (departure_datetime): Runs on Wed, 13 May 2026 at 8:00 AM,
#    capturing a standard, non-holiday school-term weekday commute, ensuring 100%
#    of Auckland Transport's active winter schedules work commuter are represented. 
#    Monday are Fridays are generally more busy becuase of the weekends around tht time frame. But 
#   Tuesday, Wed, Thursday should be apporiate to use. The reason why I chose Wednesday is betcasue it's the mid week of work week. 

#Unlike walking, my maximum transit time is a geographic constraint (how vast Auckland's physical boundaries are).
# So instead of using emperical studies, I should calculate the furtherest pt stop to cbd where available right? 



# Wait should I calculate the max travel walking time? 
# And calculate the travel transit time as well?

# ==============================================================================

# Hinami's Notes ends ---------------------------------------------------------------



#Join the calculated times back to my master spatial dataset
travel_income_HO_emp_time <- travel_income_HO_emp %>%
  left_join(transit_times, by = c("usual_residence_statistical_area_2_code_na" = "from_id"))

#Check the summaries of my newly calculated variables!
summary(travel_income_HO_emp_time$transit_time_cbd_mins)


sf::st_write(
  travel_income_HO_emp_time,
  dsn = "./data/Saved/travel_income_HO_emp_time.gpkg",
  layer = "travel_income_HO_emp_time",
  delete_dsn = TRUE # Overwrites the file if it already exists
)


#Close the Java connection once you are finished working to free up RAM
stop_r5(r5r_network)



# Let's Analyse Travel time!-----------------------------------------------------
# ANALYSIS A: Descriptive Summary and Distribution
travel_income_HO_emp_time <- st_read("./data/Saved/travel_income_HO_emp_time.gpkg")

# Check the minimum, maximum, median, and mean commute times
summary(travel_income_HO_emp_time$transit_time_cbd_mins)

# Plot the distribution of travel times across Auckland
ggplot(travel_income_HO_emp_time, aes(x = transit_time_cbd_mins)) +
  geom_histogram(fill = "purple", color = "white", bins = 30, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Auckland Public Transport Travel Times to CBD \n(Wednesday AM Peak)",
    x = "Travel Time (minutes)",
    y = "Count of SA2 Neighborhoods"
  )

# The Equity ANOVA (Transit Travel Time vs. Income Quartiles)
# This test answers: "Do lower-income neighborhoods face statistically 
# longer transit travel times to the CBD than high-income neighborhoods?"

# Define my income quartiles on this spatial dataset
travel_income_HO_emp_time <- travel_income_HO_emp_time %>%
  filter(!is.na(total_personal_income_median_2023)) %>%
  mutate(
    income_tier = cut(
      total_personal_income_median_2023, 
      breaks = quantile(total_personal_income_median_2023, probs = seq(0, 1, 0.25), na.rm = TRUE), 
      include.lowest = TRUE,
      labels = c("Q1: Low Income", "Q2: Lower-Middle", "Q3: Upper-Middle", "Q4: High Income")
    )
  )

#Run the ANOVA
time_anova <- aov(transit_time_cbd_mins ~ income_tier, data = travel_income_HO_emp_time)
summary(time_anova)
#This proves mathematically that public transport travel times to the CBD are significantly unequal across Auckland's economic neighborhoods
# Are more affluent people spend more/ess transit time compared to lower income groups? We use Tukey to identify

# Identify where the significant differences lies
TukeyHSD(time_anova)
# Interpretation for the first signfication: 
# n average, public transport commutes to the CBD from High-Income neighborhoods (Q4) are 16.69 minutes shorter
#(one-way) than commutes originating from Low-Income neighborhoods (Q1) (95% CI [−23.25 mins , −10.13 mins ],p<0.001 )

# Spatial Mapping (Transit Travel Times) - 
# Map of Transit time on Wed 13th May @8am in Auckland high urabn area
tmap_mode("view")
transit_time_wed<- tm_shape(travel_income_HO_emp_time) +
  tm_polygons("transit_time_cbd_mins", 
              style = "quantile", 
              n = 5,
              palette = "-RdYlGn", # Diverging color scheme: Green (fast) to Red (slow)
              title = "PT Travel Time to CBD (mins)",
              id = "usual_residence_statistical_area_2_name_na") +
  tm_view(
    basemap.server = c("Esri.WorldGrayCanvas", "OpenStreetMap")
  )


# Save the interactive map as an HTML webpage
# tmap_save(
#   tm = transit_time_wed,
#   filename = "./Data_Exploration/all_figures/transit_time_wed.html"
# )

# Intial observations: Interesting to see that on the North shore there's reltively fast PT travel time to CBD on Wedneday May 13th 2026 at 8am, which I think reflects
# the motivation Auckland has made to reduce the heavy congestion patters seen in the North shore, and get everyone to use PT more.
# In comparison, there's a lot of reds down south Botany Junction, Acukland air poirt and stuff which reflects not great public trnapsort access.
# We also see some reds/oragnes in the west like Whenuuapai.

# So despite faster travllining times to auckland cbd using public tranposrtin more affluent paces, privte car use
# are morst observed in affleunt groups as well. I wander the time difference in car and public transport in affleunt income neighbourhoods
# But even if PT was faster than PC, and even if the price of fuel or congetion pricing, it would not ecouagre them to use PT as it's not appealing enought right?
#  They will only choose transit if it is exceptionally competitive with driving (for example, taking a ferry from Devonport to completely bypass Harbour Bridge traffic). Alternltveiyl, PT accesability, more so with time are not well served to lower income groups which means when congestion pricing
# is introdcued, will these lower inome group face the  of burden of this pricing on top of pt accessability?

# =_____________________________________________________________________________
#  HINAMI'S DIAGNOSTIC NOTES: SPATIAL TRANSIT PATTERNS & THE EQUITY PARADOX
# =_____________________________________________________________________________
# 1. Spatial Observations:
#    - North Shore: Shows relatively fast PT travel times to the CBD during the
#      Wednesday AM peak (May 13, 2026). This reflects successful mitigation of
#      State Highway 1 congestion corridor patterns via the Northern Busway.
#    - South, East & West: Large clusters of slow travel times (red/orange) appear
#      around Botany Junction, Auckland Airport, and Whenuapai, reflecting
#      severe, historic deficits in rapid transit access [1.1.7, 1.2.7].
#
# 2. The Socio-Spatial Paradox:
#    - Despite having faster PT commute times, wealthy central neighborhoods also
#      generate the highest absolute volumes of private car commutes. 
#    - For these affluent "choice" commuters, standard pricing signals (like fuel
#      increases or congestion pricing) are often ineffective. They will only
#      switch to transit if it is exceptionally competitive and bypasses traffic
#      entirely (such as the Devonport Ferry bypassing Harbour Bridge gridlock) [1.1.2, 1.2.9].
#
# 3. The Regressive Congestion Charge Impact:
#    - Because PT accessibility (specifically travel-time competitiveness) is 
#      poorly served to lower-income outer suburbs, these vulnerable groups face 
#      a severe double-burden under congestion pricing [1.1.2, 1.2.6]. They cannot
#      realistically switch to transit (as it takes too long), forcing them to pay
#      the congestion toll to drive [1.1.2, 1.2.6].
# =_____________________________________________________________________________

# play around with the numberws

# DO GWR first ( understand )
# - Experiememt 
# - spatial lags 
# - explore a different model if it doens't wokr and then compare'
# - using package. 
# 
# - sankey - sa3(comapre) - prioirty 
# - tone is to convince
# 
# 
# 12th sept






