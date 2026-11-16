# =============================================================================
# 03_spatial_clustering_RQ1.R

# PURPOSE  RQ1.2: is car commuting spatially clustered, and where?
#          Global Moran's I (is there clustering overall?) and
#          LISA / local Moran's I (WHERE are the clusters?).
#          The same is done for PT share, to check whether PT clusters simply
#          mirror the car clusters or show something new.
#
# INPUT    data/Saved/travel_props.gpkg    (from 02)
# OUTPUT   data/Saved/travel_props.gpkg    (same file + columns lisa_car, lisa_pt)
#          all_figures/fig_lisa_car_pt.png                      (Results: Figure 4.3)
#          all_figures/fig_lisa_car.png                         (car only, spare)
#          all_figures/fig_appendix_car_distribution.png        (Appendix)
#          all_figures/fig_three_separate_components.png        (Appendix)
#          all_figures/fig_appendix_neighbours_Queen_vs_KNN.png (Appendix)
#
# NEXT     04a_pt_times_r5r.R
# =============================================================================

library(sf)
library(tidyverse)
library(tmap)
library(spdep) # neighbours, Moran's I, LISA
library(patchwork) # p1 + p2 side by side

travel_props <- st_read("./data/Saved/travel_props.gpkg", quiet = TRUE)
y    <- travel_props$prop_aggregate_private_car        # car share
y_pt <- travel_props$prop_aggregate_public_transport   # PT share



# Check the distribution -> decides which LISA method to use

p_hist <- ggplot(travel_props, aes(prop_aggregate_private_car)) +
  geom_histogram(bins = 30, fill = "grey60", colour = "white") +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Car driver share", y = "Number of SA2s", title = "(a)") +
  theme_minimal()

p_qq <- ggplot(travel_props, aes(sample = prop_aggregate_private_car)) +
  stat_qq(size = 0.8, alpha = 0.6) +
  stat_qq_line(colour = "#D55E00") +
  labs(x = "Theoretical quantiles (normal)", y = "Observed car driver share",
       title = "(b)") +
  theme_minimal()

(p_norm <- p_hist + p_qq)
ggsave("all_figures/fig_appendix_car_distribution.png", p_norm,
       width = 18, height = 8, units = "cm", dpi = 300, bg = "white")

mean((y - mean(y))^3) / sd(y)^3 # skewness = -1.80 -> strongly left-skewed
shapiro.test(y) # W = 0.84, p < 0.001 -> not normal
# -> The standard LISA p-values assume normality, so use the PERMUTATION version
#    (localmoran_perm), which builds the null distribution by reshuffling values.


# PT share: same check (permutation LISA is used for both, so this is for reporting)
mean((y_pt - mean(y_pt))^3) / sd(y_pt)^3   # skewness = 0.9525528 (positive = right-skewed)
shapiro.test(y_pt) # W = 0.94063, p = 3.416e-13

#Define neighbours Queen contiguity (SA2s sharing a border or corner)
nb_queen <- poly2nb(travel_props, queen = TRUE)
summary(nb_queen)            # average 5.1 neighbours, but 3 disconnected subgraphs
comp <- n.comp.nb(nb_queen)
table(comp$comp.id)          # 239, 175, 83 SA2s
# -> Water (harbours / estuaries) splits the city into 3 pieces that share no
#    borders, so queen weights would treat them as unrelated. Use kNN instead.
#    (Check the component map to name which water bodies separate them.)

(groups_unlinked_map <- travel_props %>%
    mutate(component = factor(comp$comp.id)) %>%
    tm_shape() +
    tm_polygons(fill = "component", fill_alpha = 0.6) +
    tm_compass(type = "arrow", position = c("left", "bottom"), size = 1.5) +
    tm_scalebar(breaks = c(0, 5, 10), position = c("left", "bottom")) +
    tm_layout(frame = FALSE, legend.position = c("right", "top")))

tmap_save(groups_unlinked_map, "all_figures/fig_three_separate_components.png",
          width = 24, height = 14, units = "cm", dpi = 300)


# k-nearest neighbours: choose k so the average matches queen (5.1)
coords <- st_centroid(st_geometry(travel_props)) %>% st_coordinates()

sapply(4:6, function(k) mean(card(make.sym.nb(knn2nb(knearneigh(coords, k = k)))))) %>%
  setNames(paste0("k=", 4:6))
# k=4 -> 4.8 (closest to 5.1), k=5 -> 5.9, k=6 -> 7.1
# make.sym.nb: if A is B's neighbour, B is also A's neighbour (symmetric)

nb_4 <- make.sym.nb(knn2nb(knearneigh(coords, k = 4)))
lw4  <- nb2listw(nb_4, style = "W")   # row-standardised: neighbours' weights sum to 1
# -> lw4 is used for ALL spatial tests in the thesis (incl. OLS residuals later)

# Appendix figure: queen vs kNN links
# Drawn with tmap (like the other maps) so it carries a north arrow and scale
# bar; both go on panel (b) only, as in the LISA figure.
get_links <- function(data, nb) {
  co <- st_coordinates(st_centroid(st_geometry(data)))
  st_set_crs(suppressWarnings(st_as_sf(nb2lines(nb, coords = co))), st_crs(data))
}
plot_links <- function(nb, title, extras = FALSE) {
  m <- tm_shape(travel_props) +
    tm_polygons(fill = "grey85", col = "white", lwd = 0.1) +
    tm_shape(get_links(travel_props, nb)) +
    tm_lines(col = "black", lwd = 0.5) +
    tm_title(title, size = 0.9) +
    tm_layout(frame = FALSE, inner.margins = c(0.02, 0.02, 0.02, 0.02))
  if (extras) m <- m +
      tm_compass(type = "arrow", position = c("right", "top"), size = 1.5) +
      tm_scalebar(breaks = c(0, 5, 10), text.size = 0.6, position = c("right", "top"))
  m
}
(p_links <- tmap_arrange(plot_links(nb_queen, "(a) Queen contiguity"),
                         plot_links(nb_4, "(b) k-nearest neighbours (k = 4)", extras = TRUE),
                         ncol = 2))

tmap_save(p_links, "all_figures/fig_appendix_neighbours_Queen_vs_KNN.png",
          width = 24, height = 14, units = "cm", dpi = 300)



#  Global Moran's I: is car (and PT) share clustered overall?

moran.test(y, lw4) # car: I = 0.92, z = 31.9, p < 0.001
moran.mc(y, lw4, nsim = 999) # permutation version: p = 0.001 (the smallest possible with 999)
# -> Very strong positive autocorrelation: SA2s look like their neighbours.

# Sensitivity: does the result depend on k?
moran_by_k <- function(v) {
  sapply(4:6, function(k) {
    moran.test(v, nb2listw(make.sym.nb(knn2nb(knearneigh(coords, k = k))), style = "W"))$estimate[1]
  }) %>% setNames(paste0("k=", 4:6)) %>% round(3)
}
moran_by_k(y) # car: 0.923, 0.915, 0.911 -> robust to the choice of k

moran.test(y_pt, lw4) # PT: I = 0.8093487710 (fill in after running)
moran_by_k(y_pt) # PT: 0.809 0.802 0.782 -> robust to the choice of k=4



#  Local Moran's I (LISA), permutation-based, FDR-corrected
# One function, used for car and PT so both are classified the same way.
#   z   : is this SA2 above (+) or below (-) the average?
#   lag : are its neighbours, on average, above or below?
# FDR (Benjamini-Hochberg): 497 tests at once would give ~25 false positives at
# p < 0.05 by chance alone; FDR controls the share of false discoveries.

lisa_levels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not significant")

lisa_classes <- function(v, lw, seed = 2026) {
  z   <- as.numeric(scale(v))
  lag <- lag.listw(lw, z)
  set.seed(seed)                                   # reproducible permutations
  p   <- p.adjust(localmoran_perm(v, lw, nsim = 9999)[, "Pr(folded) Sim"],
                  method = "fdr")
  factor(case_when(p >= 0.05       ~ "Not significant",
                   z > 0 & lag > 0 ~ "High-High", # high, surrounded by high
                   z < 0 & lag < 0 ~ "Low-Low", # low, surrounded by low
                   z > 0 & lag < 0 ~ "High-Low", # outlier: high among low
                   TRUE            ~ "Low-High"),  # outlier: low among high
         levels = lisa_levels)
}

travel_props <- travel_props %>%
  mutate(lisa_car = lisa_classes(y,    lw4),
         lisa_pt  = lisa_classes(y_pt, lw4))

table(travel_props$lisa_car)
# Car: HH 114, LL 45, LH 1 (Wiri West), HL 0, NS 337
table(travel_props$lisa_pt)
# PT:  HH 57,  LL 106, HL 0, LH 0, NS 334

# Car cluster medians (for the text)
travel_props %>% st_drop_geometry() %>%
  group_by(lisa_car) %>%
  summarise(n = n(), median_car_pct = round(100 * median(prop_aggregate_private_car), 1))
# HH 75.4%, LL 36.9%

# The single Low-High outlier
travel_props %>% st_drop_geometry() %>%
  filter(lisa_car == "Low-High") %>%
  select(usual_residence_statistical_area_2_ascii_name_na, prop_aggregate_private_car)
# Wiri West (48.7%): prison population (see 05)


#  Do the PT clusters just mirror the car clusters?
table(Car = travel_props$lisa_car, PT = travel_props$lisa_pt)
# Mirror cells: car HH = PT LL (78), car LL = PT HH (35)
# BUT 50 SA2s have a PT cluster with NO car cluster:
#   22 PT High-High: inner suburbs (Sandringham, Mt Albert, Greenlane, Ellerslie)
#                    + lower North Shore (Birkenhead, Northcote); car ~48-64%
#   28 PT Low-Low:   East (Howick, Botany Junction, Dannemora) and
#                    West (Te Atatu South, Royal Heights, West Harbour); car ~67-79%
# -> PT map adds new information: show both maps side by side (Figure 4.3).

travel_props %>% st_drop_geometry() %>%
  filter(lisa_car == "Not significant", lisa_pt %in% c("High-High", "Low-Low")) %>%
  transmute(name = usual_residence_statistical_area_2_ascii_name_na,
            lisa_pt,
            car_pct = round(100 * prop_aggregate_private_car, 1),
            pt_pct  = round(100 * prop_aggregate_public_transport, 1)) %>%
  arrange(lisa_pt, desc(pt_pct)) %>%
  as_tibble() %>%
  print(n = 50)



#  LISA maps (Results figure)
tmap_mode("plot")

lisa_cols <- c("High-High" = "#d7191c", "Low-Low" = "#2c7bb6",
               "High-Low"  = "#fdae61", "Low-High" = "#abd9e9",
               "Not significant" = "grey85")

make_lisa_map <- function(var, title, extras = FALSE) {
  # Legend shows the number of SA2s in each class, e.g. "High-High (114)"
  # (replaces the separate cluster-count table in the Results)
  cls <- factor(travel_props[[var]], levels = names(lisa_cols))
  cnt <- table(cls)
  keep <- cnt > 0 # drop empty classes (e.g. High-Low)
  m <- tm_shape(travel_props) +
    tm_polygons(fill = var,
                fill.scale = tm_scale_categorical(
                  levels = names(lisa_cols)[keep],
                  values = unname(lisa_cols[keep]),
                  labels = paste0(names(cnt)[keep], " (", cnt[keep], ")")),
                fill.legend = tm_legend(title = title,
                                        position = tm_pos_in("left", "bottom"),
                                        title.size = 0.7, text.size = 0.6,
                                        frame = FALSE),
                col = "white", lwd = 0.1) +
    tm_layout(frame = FALSE, inner.margins = c(0.02, 0.02, 0.02, 0.02))
  if (extras) m <- m +
      tm_compass(type = "arrow", position = c("right", "top"), size = 1.5) +
      tm_scalebar(breaks = c(0, 5, 10), text.size = 0.6, position = c("right", "top"))
  m
}

# Car and PT side by side (Figure 4.3)
(map_lisa_both <- tmap_arrange(make_lisa_map("lisa_car", "Car share LISA"),
                               make_lisa_map("lisa_pt",  "PT share LISA", extras = TRUE),
                               ncol = 2))
tmap_save(map_lisa_both, "all_figures/fig_lisa_car_pt.png",
          width = 24, height = 14, units = "cm", dpi = 300)

#  Car only (spare / presentation)
(map_lisa <- make_lisa_map("lisa_car", "Car share LISA", extras = TRUE))
tmap_save(map_lisa, "all_figures/fig_lisa_car.png", width = 16, height = 18, units = "cm", dpi = 300)
# -> Car High-High: South Auckland belt (+ small group around Massey / Westgate).
#    Car Low-Low: city centre, inner isthmus, Devonport peninsula.
#    PT High-High reaches further out than car Low-Low; PT Low-Low also covers
#    East and West Auckland, not only the south.



# Notes only: interactive side-by-side LISA maps (pan/zoom linked)
# library(leafsync)
# tmap_mode("view")
# 
# notes_lisa <- travel_props %>%
#   mutate(car_pct = round(100 * prop_aggregate_private_car, 1),
#          pt_pct  = round(100 * prop_aggregate_public_transport, 1))
# 
# popup_lisa <- c("Car (%)"  = "car_pct",  "Car LISA" = "lisa_car",
#                 "PT (%)"   = "pt_pct",   "PT LISA"  = "lisa_pt")
# 
# make_lisa_view <- function(var, title) {
#   tm_shape(notes_lisa) +
#     tm_polygons(fill = var,
#                 fill.scale = tm_scale_categorical(values = lisa_cols),
#                 fill.legend = tm_legend(title = title),
#                 fill_alpha = 0.7, col = "white", lwd = 0.3,
#                 id = "usual_residence_statistical_area_2_ascii_name_na",
#                 popup.vars = popup_lisa)
# }
# 
# (lisa_side <- sync(tmap_leaflet(make_lisa_view("lisa_car", "Car share LISA")),
#                   tmap_leaflet(make_lisa_view("lisa_pt",  "PT share LISA"))))
# htmltools::save_html(lisa_side,
#                      "./all_figures/Notes_Hinami_figures/03_Notes_lisa_car_pt.html")


# Save (lisa_car / lisa_pt are stored as text in the .gpkg -> re-factor after reading)
st_write(travel_props, "./data/Saved/travel_props.gpkg", delete_dsn = TRUE, quiet = TRUE)

