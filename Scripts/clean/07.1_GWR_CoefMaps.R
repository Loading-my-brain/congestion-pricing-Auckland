# =============================================================================
# 07.1_GWR_CoefMaps.R
# PURPOSE  Local GWR coefficient maps (Results figure). Runs after 07.
# INPUT    data/Saved/gwr_results.gpkg       (from 07, incl. *_sig columns)
#          data/Saved/travel_props_vars.gpkg (all 497 SA2s, grey background)
# OUTPUT   all_figures/fig_gwr_coefficients.png, all_figures/fig_gwr_pt_time_only.png
# =============================================================================
library(sf)
library(tidyverse)
library(tmap)

gwr_out <- st_read("./data/Saved/gwr_results.gpkg", quiet = TRUE)
all_sa2 <- st_read("./data/Saved/travel_props_vars.gpkg", quiet = TRUE)

# High-High clusters from RQ1 (saved by 03 as columns lisa_car and lisa_pt)
lisa   <- st_read("./data/Saved/travel_props.gpkg", quiet = TRUE)
hh     <- lisa %>% filter(lisa_car == "High-High") %>% st_union()   # car clusters
hh_pt  <- lisa %>% filter(lisa_pt  == "High-High") %>% st_union()   # high-PT clusters
# Outlines: same dark grey and same thickness, told apart by line type only
# (solid vs dashed). No reliance on colour, so colour-blind safe, and quieter
# than coloured lines on top of the red-blue fill.
col_hh    <- "black"    # solid, slightly thicker = High-High car share (main focus; same in Figure 4.6)
col_hh_pt <- "grey55"   # grey, thinner = High-High PT share (secondary)
lwd_hh    <- 0.7   # thin: shows the cluster edge without hiding the fill
lwd_hh_pt <- 0.5

# Predictors in the final GWR = the columns 07 saved with a "_sig" suffix
# Fixed order = Table 4.2 / Equation 3.1, so panels (a)-(d) are always in the same order
preds <- c("pt_time_10", "income_10k", "home_owner_pct", "blue_collar_pct")
stopifnot(all(paste0(preds, "_sig") %in% names(gwr_out)))


#Maps: local coefficients (Results figure)
# Diverging colours centred on 0: red = more car commuting as X increases,
# blue = less. Grey = not significant (after adjustment) or excluded SA2.
tmap_mode("plot")

# Class breaks calculated from the significant local coefficients:
# outer breaks = the actual minimum and maximum (2 dp, to match Table 4.4),
# inner breaks = round numbers ("pretty"), always including 0 where the range crosses it
make_breaks <- function(x, n = 4) {
  x  <- x[!is.na(x)]
  lo <- floor(min(x) * 100) / 100 # actual minimum, rounded down to 2 dp
  hi <- ceiling(max(x) * 100) / 100 # actual maximum, rounded up to 2 dp
  b  <- pretty(range(c(x, 0)), n = n) 
  b  <- b[b > lo & b < hi] # keep only the round numbers inside the range
  unique(sort(c(lo, b, hi)))
}

coef_breaks <- map(set_names(preds), ~ make_breaks(gwr_out[[paste0(.x, "_sig")]]))
coef_breaks   # print them: report these in the figure caption or Methods

panel_titles <- c(pt_time_10  = "(a) PT travel time (per 10 min)",
                  income_10k = "(b) Median income (per $10,000)",
                  home_owner_pct = "(c) Home ownership (%)",
                  blue_collar_pct = "(d) Blue-collar workers (%)")

# Decimals in the legend: 2 dp for every map, the same as Table 4.4
legend_digits <- c(pt_time_10 = 2, income_10k = 2,
                   blue_collar_pct = 2, home_owner_pct = 2)

# slide = TRUE -> bigger title and legend text (for the presentation)
coef_map <- function(v, slide = FALSE) {
  txt <- if (slide) c(title = 1.1, leg_title = 0.9, leg_text = 0.8) else
    c(title = 1.0, leg_title = 0.75, leg_text = 0.65)   # sized for a full A4 page
  tm_shape(all_sa2) + tm_polygons(fill = "grey90", col = "white", lwd = 0.1) +
    tm_shape(gwr_out) +
    tm_polygons(fill = paste0(v, "_sig"),
                fill.scale = tm_scale_intervals(breaks = coef_breaks[[v]], midpoint = 0,
                                                values = "-brewer.rd_bu",
                                                value.na = "grey85",
                                                label.na = "Not significant",
                                                label.format = list(digits = legend_digits[[v]], text.separator = "to")),
                fill.legend = tm_legend(title = "Local coefficient",
                                        title.size = txt[["leg_title"]],
                                        text.size  = txt[["leg_text"]], frame = FALSE,
                                        # below the map, in one row: never covers Auckland
                                        position = tm_pos_out("center", "bottom"),
                                        orientation = "landscape"),
                col = "white", lwd = 0.1) +
    tm_shape(hh_pt) + tm_borders(col = col_hh_pt, lwd = lwd_hh_pt) +  # High-High PT share
    tm_shape(hh)    + tm_borders(col = col_hh,    lwd = lwd_hh) +                   # High-High car share
    tm_title(panel_titles[[v]], size = txt[["title"]]) +
    tm_layout(frame = FALSE,
              # bottom, left, top, right: extra room on the right for the
              # north arrow, scale bar and outline key, so nothing sits on the map
              inner.margins = c(0.02, 0.02, 0.02, 0.20))
}

# Small north arrow + scale bar (added to one panel only)
map_extras <- tm_compass(type = "arrow", position = c("right", "top"),
                         size = 1.5, text.size = 0.7) +
  tm_scalebar(breaks = c(0, 5, 10), position = c("right", "top"),
              text.size = 0.7, lwd = 1.2)

# Legend for the two outlines (added to one panel only)
outline_key <- function(pos) {
  # No title and no box: the two line samples explain themselves, and the
  # figure note says where the clusters come from (Section 4.1.4)
  tm_add_legend(type = "lines", labels = c("High-High car share", "High-High PT share"),
                col = c(col_hh, col_hh_pt), lwd = c(lwd_hh, lwd_hh_pt),
                position = pos, frame = FALSE, text.size = 0.6)
}
outline_legend     <- outline_key(tm_pos_in("right", "top")) # 2x2 figure: empty corner of panel (b)
outline_legend_out <- outline_key(tm_pos_out("right", "center")) # single maps: beside the map, never on it

coef_maps <- map(preds, coef_map)
coef_maps[[1]] <- coef_maps[[1]] + map_extras # north arrow + scale bar on panel (a)
coef_maps[[2]] <- coef_maps[[2]] + outline_legend # outline key in the empty top-right of panel (b)

#  All four panels (thesis figure)
(map_coefs <- do.call(tmap_arrange, c(coef_maps, ncol = 2)))
# Saved at the size it will print (A4 text width ~16 cm), so text and scale bar
# keep their size in Word. Insert at 100% - do not shrink it.
tmap_save(map_coefs, "all_figures/fig_gwr_coefficients.png",
          width = 16, height = 20, units = "cm", dpi = 300)

#  Each panel on its own (if you prefer separate figures or an appendix)
# Built fresh so each has its own arrow, scale bar and outline key (key outside the map)
walk(preds, ~ tmap_save(coef_map(.x) + map_extras + outline_legend_out,
                        paste0("all_figures/fig_gwr_coef_", .x, ".png"),
                        width = 20, height = 14, units = "cm", dpi = 300))

# How many significant SA2s fall inside each type of cluster? (for the text)
in_hh    <- lengths(st_intersects(st_point_on_surface(gwr_out), hh))    > 0
in_hh_pt <- lengths(st_intersects(st_point_on_surface(gwr_out), hh_pt)) > 0
map_dfr(preds, ~ tibble(predictor = .x,
                        n_sig = sum(!is.na(gwr_out[[paste0(.x, "_sig")]])),
                        in_car_HH = sum(!is.na(gwr_out[[paste0(.x, "_sig")]]) & in_hh),
                        in_PT_HH = sum(!is.na(gwr_out[[paste0(.x, "_sig")]]) & in_hh_pt)))
# Expected (from the saved data): PT time 96 sig, 1 in car HH, 39 in PT HH (of 57 PT HH SA2s)

#  PT time panel only, larger text (presentation slide)
(map_pt_only <- coef_map("pt_time_10", slide = TRUE) + map_extras + outline_legend_out)
tmap_save(map_pt_only, "all_figures/fig_gwr_pt_time_only.png",
          width = 22, height = 16, units = "cm", dpi = 300)

#  Where are the significant SA2s? (to fill the [area] gaps in Section 4.2.3)
# One list per predictor, strongest coefficient first. Use the map for the
# broad area (e.g. central isthmus) and the top names here as examples.
sig_lists <- map(set_names(preds), ~ gwr_out %>%
                   st_drop_geometry() %>%
                   as_tibble() %>%   # tibble, so print(n = Inf) shows every row
                   filter(!is.na(.data[[paste0(.x, "_sig")]])) %>%
                   select(sa2_name, coef = all_of(paste0(.x, "_sig"))) %>%
                   arrange(desc(coef)))
walk2(sig_lists, names(sig_lists), ~ { cat("\n====", .y, "(n =", nrow(.x), ")====\n"); print(.x, n = Inf) })


#  Interactive map (for checking only - not a thesis figure)
# Opens in the RStudio Viewer. Hover over an SA2 to see its name, click for
# the coefficient values. Use the layer button (top left) to switch predictors.
tmap_mode("view")
# Basemap: OpenStreetMap is free and needs no API key
map_view <- tm_basemap("OpenStreetMap")
for (v in preds) {
  map_view <- map_view +   # add each predictor as its own layer (don't overwrite)
    tm_shape(gwr_out) +
    tm_polygons(fill = paste0(v, "_sig"),
                fill.scale = tm_scale_intervals(breaks = coef_breaks[[v]], midpoint = 0,
                                                values = "-brewer.rd_bu",
                                                value.na = "grey85",
                                                label.na = "Not significant"),
                fill.legend = tm_legend(title = panel_titles[[v]]),
                fill_alpha = 0.8, col = "white", lwd = 0.3,
                id = "sa2_name",
                popup.vars = paste0(preds, "_sig"),
                group = panel_titles[[v]])
}
map_view <- map_view +
  tm_shape(hh_pt) + tm_borders(col = col_hh_pt, lwd = 2, group = "High-High PT share") +
  tm_shape(hh)    + tm_borders(col = col_hh,    lwd = 2, group = "High-High car share")
map_view   # show it in the Viewer

# Optional: save it to open in a browser later
#tmap_save(map_view, "./all_figures/Notes_Hinami_figures/07.1_gwr_coefficients_interactive.html")
tmap_mode("plot")   # back to static maps for everything else
