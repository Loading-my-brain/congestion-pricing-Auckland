# =============================================================================
# 07_ols_gwr_RQ2.R
# -----------------------------------------------------------------------------
# PURPOSE  RQ2: How does the relationship between car commuting, socio-economic
#          factors and travel time vary across space?
#            Step 1  Global OLS   - one relationship for the whole region
#            Step 2  Checks       - multicollinearity (VIF), residuals, outliers
#            Step 3  Residual Moran's I - does OLS leave spatial clustering?
#            Step 4  GWR          - a separate local regression around each SA2
#            Step 5  Compare OLS vs GWR (AICc, R2, residual clustering)
#            Step 6  Maps of local coefficients (only where significant)
#            Step 7  Robustness checks
#
# INPUT    data/Saved/model_data.gpkg          (490 SA2s, from 06)
#          data/Saved/travel_props_vars.gpkg   (497 SA2s, grey background on maps)
# OUTPUT   data/Saved/gwr_results.gpkg
#          all_figures/fig_gwr_coefficients.png   (Results: local coefficient maps)
#          all_figures/fig_gwr_r2_residuals.png   (Results / Appendix)
#          all_figures/fig_appendix_ols_diagnostics.png
#          all_figures/fig_appendix_pairs.png
#
# NEXT     08_rq3_*.R
# =============================================================================

library(sf)
library(tidyverse)
library(spdep) # weights, Moran's I
library(GWmodel) # bw.gwr(), gwr.basic()
library(car) # vif(), crPlots()
library(tmap)
library(patchwork)


#Data: rescale variables so coefficients are easy to read
# Shares are converted to PERCENT (0-100), so a coefficient reads as
# "percentage points of car share per 1 percentage point of X".
model_data <- st_read("./data/Saved/model_data.gpkg", quiet = TRUE) %>%
  mutate(
    car_pct         = 100 * prop_aggregate_private_car, # OUTCOME
    pt_time_10      = PT_transit_time_cbd_mins / 10, # per 10 minutes
    income_10k      = total_personal_income_median_2023 / 10000, # per $10,000
    blue_collar_pct = 100 * prop_blue_collar,
    home_owner_pct  = 100 * prop_home_owner,
    full_time_pct   = 100 * prop_emp_full_time,
    hrs50_pct       = 100 * prop_hours_50plus,
    low_income_pct  = 100 * prop_income_low # robustness only
  )
nrow(model_data)   # 490

#Descriptive statistics for Table 4.1 (the 490 SA2s used in OLS and GWR)
desc_490 <- model_data %>%
  st_drop_geometry() %>%
  transmute(`Car share (%)`              = car_pct,
            `Blue-collar workers (%)`    = blue_collar_pct,
            `Home ownership (%)`         = home_owner_pct,
            `PT travel time (min)`       = PT_transit_time_cbd_mins,
            `Median personal income ($)` = total_personal_income_median_2023) %>%
  tidyr::pivot_longer(everything(), names_to = "Variable") %>%
  group_by(Variable) %>%
  summarise(n      = n(),
            Mean   = round(mean(value), 1),
            Median = round(median(value), 1),
            Q1     = round(quantile(value, .25), 1),
            Q3     = round(quantile(value, .75), 1),
            Min    = round(min(value), 1),
            Max    = round(max(value), 1))
print(desc_490, width = Inf)
# Expected: car 64.4 / 66.6 / 59.7-73.1 / 13.5-80.9; blue-collar 25.2 / 24.2 / 17.6-31.4 / 6.5-50.2
#           home 40.3 / 40.9 / 31.6-49.1 / 6.7-70.2; PT 64.3 / 65.5 / 50-80 / 4-151
#           income 45,363 / 44,650 / 38,800-51,525 / 13,000-80,500 (all n = 490)

# Background layer for maps: all 497 SA2s (excluded ones appear grey)
all_sa2 <- st_read("./data/Saved/travel_props_vars.gpkg", quiet = TRUE)

# Predictor labels used in tables and maps
pred_labels <- c(pt_time_10      = "PT time to CBD (per 10 min)",
                 income_10k      = "Median income (per $10k)",
                 blue_collar_pct = "Blue-collar (%)",
                 home_owner_pct  = "Home ownership (%)",
                 full_time_pct   = "Full-time employed (%)",
                 hrs50_pct       = "Works 50+ hrs (%)")


# Look at the pairs (Appendix figure)
# Spearman rho shows direction; scatterplots show SHAPE (curves, outliers, clumps).
# Check the top row: car share vs each predictor - roughly straight?
p_pairs <- model_data %>%
  st_drop_geometry() %>%
  transmute(`Car share (%)` = car_pct, `PT time (10 min)` = pt_time_10,
            `Income ($10k)` = income_10k, `Blue-collar` = blue_collar_pct,
            `Home owner` = home_owner_pct, `Full-time` = full_time_pct,
            `50+ hrs` = hrs50_pct) %>%
  GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.3, size = 0.4)),
                  upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 2.8))) +
  theme_minimal(base_size = 7)
p_pairs
ggsave("all_figures/fig_appendix_pairs.png", p_pairs,
       width = 22, height = 22, units = "cm", dpi = 300, bg = "white")


#  Global OLS
#  Start with all six candidates and check multicollinearity.
#     VIF = how much a coefficient's variance is inflated because the predictor
#     overlaps with the others. Rule: < 5 (stricter: < 3).
f_full <- car_pct ~ pt_time_10 + income_10k + blue_collar_pct +
  home_owner_pct + full_time_pct + hrs50_pct
vif(lm(f_full, data = model_data))
# Result: income_10k = 9.68 (> 5) -> income overlaps with blue-collar (rho -0.77),
# full-time (0.68) and home ownership (0.63). Its OLS coefficient was unstable
# (p = 0.74, large SE) and its GWR coefficients swung from -11.7 to +13.7.

#  Fix: drop full-time employment and 50+ hrs.
#     Both were weak and non-significant (p = 0.13, 0.16), and full-time
#     employment was also the variable removed for the same reason in the
#     earlier PT-share model. (50+ hrs tested the "time poverty" idea: not supported.)
f_4 <- car_pct ~ pt_time_10 + income_10k + home_owner_pct + blue_collar_pct
vif_4 <- vif(lm(f_4, data = model_data))
round(vif_4, 2)

# If income is STILL > 5, its remaining overlap is with blue-collar.
#     Income is kept (central to the equity question) and blue-collar dropped.
f_3 <- car_pct ~ pt_time_10 + income_10k + home_owner_pct
round(vif(lm(f_3, data = model_data)), 2)

f_final <- if (max(vif_4) < 5) f_4 else f_3
f_final# <- the model used from here on
message(ifelse(max(vif_4) < 5,
               "Using 4 predictors (all VIF < 5)",
               "Using 3 predictors (blue-collar dropped: VIF still > 5 with income)"))

ols <- lm(f_final, data = model_data)

summary(ols)
# How to read it:
#  - Estimate: change in car share (percentage points) for a 1-unit increase in X,
#    holding the other predictors constant.
#  - Pr(>|t|) < 0.05: the relationship is statistically significant.
#  - Adjusted R-squared: share of variation in car share explained REGION-WIDE.


# HINAMI - CHECK PLS
# Robust (HC3) standard errors - variance is not constant (ncvTest p < 0.001)
lmtest::coeftest(ols, vcov = sandwich::vcovHC(ols, type = "HC3"))
# -> Report these p-values if any change; otherwise state results are robust.


# Standardised coefficients: which predictor matters MOST? (all on the same scale)
std_vars <- all.vars(f_final)
ols_std  <- lm(f_final, data = model_data %>% st_drop_geometry() %>%
                 mutate(across(all_of(std_vars), ~ as.numeric(scale(.x)))) )
round(sort(    abs(coef(ols_std)[-1])    , decreasing = TRUE), 2)
# -> Largest = strongest predictor (change in SDs of car share per 1 SD of X).


#OLS diagnostics
#Standard residual plots (Appendix)
png("all_figures/fig_appendix_ols_diagnostics.png", width = 18, height = 16,
    units = "cm", res = 300)
par(mfrow = c(2, 2)); plot(ols); par(mfrow = c(1, 1))
dev.off()
# Residuals vs Fitted: no curve / funnel?  Q-Q: points near the line?
# Scale-Location: flat?  Residuals vs Leverage: any point beyond Cook's distance 1?

if (requireNamespace("s20x", quietly = TRUE)) s20x::modelcheck(ols)   # UoA STATS package

# Non-linearity: component + residual plots
crPlots(ols)
# -> If a line clearly curves (earlier: income), try log(income) as a check.

# Unusual SA2s: studentized residuals beyond +/-3, and Cook's distance
model_data %>%
  st_drop_geometry() %>%
  mutate(stud_resid = rstudent(ols), cooks = cooks.distance(ols)) %>%
  filter(abs(stud_resid) > 3 | cooks > 4 / nrow(model_data)) %>%
  select(sa2_name, car_pct, stud_resid, cooks) %>%
  arrange(desc(abs(stud_resid)))
# -> Expect CBD / student / retirement SA2s (e.g. Henderson Lincoln East).
#    These are REAL places, so they are kept (state this in Methods).

#  Heteroscedasticity (non-constant variance) test
ncvTest(ols)
# -> If p < 0.05, variance is not constant: one more reason a single global
#    model may not fit everywhere (and supports trying GWR).


# Spatial autocorrelation in the OLS residuals
# Same weights as RQ1 (kNN, k = 4, symmetric, row-standardised), but built on
# the SAME 490 SA2s as the model (row order must match).
coords <- st_centroid(st_geometry(model_data)) %>% st_coordinates()
lw4    <- nb2listw(make.sym.nb(knn2nb(knearneigh(coords, k = 4))), style = "W")

lm.morantest(ols, lw4)
# -> lm.morantest is the correct test for regression residuals (it adjusts the
#    expected value for the fitted model).
#    Significant positive I = OLS errors are clustered in space: neighbouring
#    SA2s are over- or under-predicted together. The relationship is likely
#    NOT the same everywhere -> justifies GWR.

model_data$ols_resid <- residuals(ols)


# GWR: bandwidth and model
# GWR fits a weighted regression around every SA2 using its nearby SA2s.
#  - Adaptive bandwidth: a fixed NUMBER of nearest SA2s (suits SA2s that are
#    small in the centre and large on the fringe).
#  - Bisquare kernel: nearby SA2s count most; beyond the bandwidth, weight = 0.
#  - Bandwidth chosen by minimising AICc (balances fit and complexity).
model_sp <- as(model_data, "Spatial")# GWmodel needs the older 'sp' format

set.seed(2026)
bw <- bw.gwr(f_final, data = model_sp, approach = "AICc",
             kernel = "bisquare", adaptive = TRUE, longlat = FALSE)
bw
paste0("Each local model uses the nearest ", bw, " SA2s (",
       round(100 * bw / nrow(model_data)), "% of the data)")

gwr_mod <- gwr.basic(f_final, data = model_sp, bw = bw,
                     kernel = "bisquare", adaptive = TRUE, longlat = FALSE)
gwr_mod

# LOCAL multicollinearity: global VIFs can be fine while small local
#     models still have overlapping predictors -> unstable local coefficients.
#     Local condition number (CN) > 30 = local coefficients not trustworthy.
lcd <- gwr.collin.diagno(f_final, data = model_sp, bw = bw,
                         kernel = "bisquare", adaptive = TRUE, longlat = FALSE)
summary(lcd$SDF$local_CN)
sum(lcd$SDF$local_CN > 30)# number of SA2s with a problem
# Local VIFs per predictor (> 10 = problem)
lcd_vif <- st_drop_geometry(st_as_sf(lcd$SDF))
lcd_vif <- lcd_vif[, grepl("VIF", names(lcd_vif)), drop = FALSE]
sapply(lcd_vif, function(x) c(median = median(x), max = max(x), n_over_10 = sum(x > 10)))
# -> If only a few SA2s exceed the limits, report it as a limitation.
#    If many do, the GWR needs fewer predictors.

#Same check on STANDARDISED predictors (report THIS version in the thesis).
#     Much of the high CN above is a scaling artefact: the intercept sits next to
#     unstandardised predictors (e.g. home ownership, mean ~41%). Standardising
#     (mean 0, SD 1) removes that. VIFs do not change with scaling; only CN does.
md_z <- model_data %>%
  mutate(across(c(pt_time_10, income_10k, home_owner_pct, blue_collar_pct),
                ~ as.numeric(scale(.x))))
lcd_z <- gwr.collin.diagno(f_final, data = as(md_z, "Spatial"), bw = bw,
                           kernel = "bisquare", adaptive = TRUE, longlat = FALSE)
summary(lcd_z$SDF$local_CN)
sum(lcd_z$SDF$local_CN > 30)                   # SA2s with CN > 30 after standardising
# -> Put these two results in Section 4.2.3 [CN result].
#    If many SA2s are still > 30, say so in the Chapter 5 limitations.


# Compare OLS and GWR
aicc_lm <- function(m) {                          # AICc for an lm model
  k <- length(coef(m)) + 1; n <- nobs(m)
  AIC(m) + 2 * k * (k + 1) / (n - k - 1)
}

gwr_out <- st_as_sf(gwr_mod$SDF)# local results as sf

moran_raw <- moran.test(model_data$car_pct, lw4)
moran_ols <- lm.morantest(ols, lw4)
moran_gwr <- moran.test(gwr_out$residual, lw4)

(model_compare <- tibble(
  Model      = c("OLS (global)", "GWR (local)"),
  AICc       = c(aicc_lm(ols), gwr_mod$GW.diagnostic$AICc),
  R2         = c(summary(ols)$r.squared, gwr_mod$GW.diagnostic$gw.R2),
  Adj_R2     = c(summary(ols)$adj.r.squared, gwr_mod$GW.diagnostic$gwR2.adj),
  Resid_I    = c(moran_ols$estimate[1], moran_gwr$estimate[1]),
  Resid_I_p  = c(moran_ols$p.value, moran_gwr$p.value)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))))
# How to read it:
#  - AICc lower by > 3 = better model (accounting for GWR's extra complexity).
#  - Higher adjusted R2 = more variation explained.
#  - Residual Moran's I closer to 0 = less unexplained spatial clustering.

# Moran's I table for the thesis: raw car share -> OLS residuals -> GWR residuals
tibble(Surface  = c("Car share", "OLS residuals", "GWR residuals"),
       Morans_I = c(moran_raw$estimate[1], moran_ols$estimate[1], moran_gwr$estimate[1]),
       p_value  = c(moran_raw$p.value, moran_ols$p.value, moran_gwr$p.value)) %>%
  mutate(across(where(is.numeric), ~ signif(.x, 3)))
# NB: moran.test on GWR residuals is a descriptive check (no exact test exists).


# Local coefficients: summary table (Results table)
preds <- names(pred_labels)[names(pred_labels) %in% all.vars(f_final)]

(coef_table <- map_dfr(c("Intercept", preds), function(v) {
  x <- gwr_out[[v]]
  tibble(Variable = v, Min = min(x), Q1 = quantile(x, 0.25), Median = median(x),
         Q3 = quantile(x, 0.75), Max = max(x),
         Global_OLS = coef(ols)[ifelse(v == "Intercept", "(Intercept)", v)],
         Pct_positive = 100 * mean(x > 0))
}) %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
# -> If Min and Max have OPPOSITE signs, the relationship changes direction
#    across Auckland: the clearest evidence of spatial variation (RQ2).


#  Which local coefficients are significant? (multiple-testing adjusted)
# Each predictor is tested 490 times (once per SA2), so the usual |t| > 1.96
# gives many false positives. da Silva & Fotheringham (2016) adjust alpha by the
# effective number of parameters (ENP) relative to the number of predictors:
#     alpha_adj = 0.05 / (ENP / p)
enp     <- gwr_mod$GW.diagnostic$enp
p       <- length(preds) + 1 # incl. intercept
alpha_a <- 0.05 / (enp / p)
t_crit  <- qt(1 - alpha_a / 2, df = gwr_mod$GW.diagnostic$edf)   # edf = effective residual df
c(ENP = round(enp, 1), adjusted_alpha = signif(alpha_a, 3), t_critical = round(t_crit, 2))

for (v in preds) {
  gwr_out[[paste0(v, "_sig")]] <- ifelse(abs(gwr_out[[paste0(v, "_TV")]]) > t_crit,
                                         gwr_out[[v]], NA)   # NA = not significant
}

# How many SA2s have a significant (positive / negative) coefficient?
map_dfr(preds, function(v) {
  s <- gwr_out[[paste0(v, "_sig")]]
  tibble(Variable = pred_labels[v],
         Sig_positive = sum(s > 0, na.rm = TRUE),
         Sig_negative = sum(s < 0, na.rm = TRUE),
         Not_sig      = sum(is.na(s)))
})

# CHECK ONLY (not reported): standard |t| > 1.96 vs adjusted |t| > t_crit
# What is a t-value?
#   t = local coefficient / its standard error. It measures how far the
#   coefficient is from 0 relative to its uncertainty. |t| > 1.96 means the
#   coefficient is significant at p < 0.05 for ONE test (two-tailed).
#
# Why adjust using ENP instead of using 1.96?
#   GWR tests each predictor 490 times (once per SA2). At p < 0.05, about
#   1 in 20 tests look significant by chance -> ~25 false positives per
#   predictor. The local models also overlap (neighbouring SA2s share data),
#   so the tests are not independent. ENP (effective number of parameters,
#   184.3 here) measures how many independent estimates the GWR really makes.
#   da Silva & Fotheringham (2016): alpha_adj = 0.05 / (ENP / p)
#   -> alpha 0.00136, critical |t| 3.23.
#
# Pros of the adjustment:
#   + Removes most chance (false-positive) results
#   + Every SA2 shown as significant on the maps is defensible
#   + Recommended practice for GWR
# Cons:
#   - Stricter, so some real but weaker local effects are missed (shown grey)
#   - Fewer significant SA2s to describe
#
# Sig_unadjusted = count at 1.96 (standard, single test)
# Sig_adjusted   = count at t_crit (what the dissertation reports)
# Dropped        = SA2s removed by the correction
map_dfr(preds, function(v) {
  tv <- gwr_out[[paste0(v, "_TV")]]
  tibble(Variable          = pred_labels[v],
         Sig_unadjusted    = sum(abs(tv) > 1.96,   na.rm = TRUE),
         Sig_adjusted      = sum(abs(tv) > t_crit, na.rm = TRUE),
         Dropped_by_adjust = Sig_unadjusted - Sig_adjusted)
})

# Expected false positives per predictor with no correction:
round(nrow(gwr_out) * 0.05)   # about 25 of 490 SA2s


#  Maps: local coefficients (Results figure)
# Diverging colours centred on 0: red = more car commuting as X increases,
# blue = less. Grey = not significant (after adjustment) or excluded SA2.
tmap_mode("plot")

coef_map <- function(v) {
  tm_shape(all_sa2) + tm_polygons(fill = "grey90", col = "white", lwd = 0.1) +
    tm_shape(gwr_out) +
    tm_polygons(fill = paste0(v, "_sig"),
                fill.scale = tm_scale_intervals(style = "pretty", n = 6, midpoint = 0,
                                                values = "-brewer.rd_bu",
                                                value.na = "grey85",
                                                label.na = "Not significant"),
                fill.legend = tm_legend(title = pred_labels[v], title.size = 0.65,
                                        text.size = 0.5, position = tm_pos_in("left", "bottom")),
                col = "white", lwd = 0.1) +
    tm_layout(frame = FALSE)
}

coef_maps <- map(preds, coef_map)
coef_maps[[length(coef_maps)]] <- coef_maps[[length(coef_maps)]] +
  tm_compass(type = "arrow", position = c("right", "top"), size = 1.2) +
  tm_scalebar(breaks = c(0, 5, 10), text.size = 0.5, position = c("right", "top"))

(map_coefs <- do.call(tmap_arrange, c(coef_maps, ncol = 2)))
# Quick check only. The thesis figure (fig_gwr_coefficients.png, with panel letters
# and cluster outlines) is made by 07.1 - not saved here, so 07 cannot overwrite it.


#CHECK ONLY (not reported): maps at |t| > 1.96 vs adjusted |t| > t_crit
# Left column = standard 1.96 cut-off; right column = adjusted cut-off (3.23)
# used in the dissertation. Same colours as Section 9; grey = not significant.
# The extra coloured SA2s on the left are the ones the correction removes.
for (v in preds) {
  gwr_out[[paste0(v, "_sig196")]] <- ifelse(abs(gwr_out[[paste0(v, "_TV")]]) > 1.96,
                                            gwr_out[[v]], NA)
}

compare_map <- function(v, suffix, label) {
  tm_shape(all_sa2) + tm_polygons(fill = "grey90", col = "white", lwd = 0.1) +
    tm_shape(gwr_out) +
    tm_polygons(fill = paste0(v, suffix),
                fill.scale = tm_scale_intervals(style = "pretty", n = 6, midpoint = 0,
                                                values = "-brewer.rd_bu",
                                                value.na = "grey85",
                                                label.na = "Not significant"),
                fill.legend = tm_legend(title = paste0(pred_labels[v], "\n", label),
                                        title.size = 0.6, text.size = 0.45,
                                        position = tm_pos_in("left", "bottom")),
                col = "white", lwd = 0.1) +
    tm_layout(frame = FALSE)
}

compare_maps <- unlist(lapply(preds, function(v) list(
  compare_map(v, "_sig196", "|t| > 1.96 (standard)"),
  compare_map(v, "_sig",    paste0("|t| > ", round(t_crit, 2), " (adjusted)"))
)), recursive = FALSE)

(map_compare <- do.call(tmap_arrange, c(compare_maps, ncol = 2)))
# save for my own reference (not in all_figures, not reported)
tmap_save(map_compare, "check_gwr_sig_196_vs_adjusted.png",
          width = 22, height = 22, units = "cm", dpi = 300)


#10. Map: local R2 (Appendix). Residuals are mapped in 10b.
# Colour-blind safe: ColorBrewer YlGnBu (sequential) and RdBu (diverging) are
# both marked colour-blind safe. Excluded SA2s are white with a grey outline,
# so they are not confused with any data class.
map_r2 <- tm_shape(all_sa2) + tm_polygons(fill = "white", col = "grey70", lwd = 0.1) +
  tm_shape(gwr_out) +
  tm_polygons(fill = "Local_R2",
              fill.scale = tm_scale_intervals(style = "quantile", n = 5, values = "brewer.yl_gn_bu",
                                              label.format = list(digits = 2)),
              fill.legend = tm_legend(title = "Local R\u00b2", position = tm_pos_in("left", "bottom")),
              col = "white", lwd = 0.1) +
  tm_compass(type = "arrow", position = c("right", "top"), size = 1.2) +
  tm_scalebar(breaks = c(0, 5, 10), text.size = 0.5, position = c("right", "top")) +
  tm_layout(frame = FALSE)

map_r2
tmap_save(map_r2, "all_figures/fig_gwr_local_r2.png",
          width = 14, height = 14, units = "cm", dpi = 300)
# Local R2: where the local model explains car share well / poorly.


#  OLS vs GWR residuals side by side (Results, Sections 4.2.2-4.2.3)
# Same breaks and colours on both maps, so the drop in clustering
# (Moran's I = 0.61 -> 0.02) can be compared directly.
# Breaks: +/-1.96 and +/-2.58 = unusual at the 5% and 1% levels; +/-1 adds
# the moderate errors so the clustered patches are visible.
# Colours: ColorBrewer RdBu (colour-blind safe); light grey = small error;
# white with grey outline = SA2 not in the model.
model_data$ols_stud <- rstudent(ols)
summary(model_data$ols_stud)        # OLS: -5.31 to 2.32
summary(gwr_out$Stud_residual)      # GWR: -4.72 to 3.60

res_breaks <- c(-Inf, -2.58, -1.96, -1, 1, 1.96, 2.58, Inf)
res_cols   <- c("#2166ac", "#67a9cf", "#d1e5f0", "grey85", "#fddbc7", "#ef8a62", "#b2182b")
res_labels <- c("< \u22122.58", "\u22122.58 to \u22121.96", "\u22121.96 to \u22121", "\u22121 to 1",
                "1 to 1.96", "1.96 to 2.58", "> 2.58")

res_map <- function(data, var, title, legend = TRUE) {
  tm_shape(all_sa2) + tm_polygons(fill = "white", col = "grey70", lwd = 0.1) +
    tm_shape(data) +
    tm_polygons(fill = var,
                fill.scale = tm_scale_intervals(breaks = res_breaks, values = res_cols,
                                                labels = res_labels, midpoint = NA),
                fill.legend = if (legend) tm_legend(title = "Studentised\nresidual",
                                                    position = tm_pos_in("left", "bottom"),
                                                    title.size = 0.7, text.size = 0.55)
                              else tm_legend(show = FALSE),
                col = "white", lwd = 0.1) +
    tm_title(title, size = 0.9) +
    tm_layout(frame = FALSE)
}

map_res_ols <- res_map(model_data, "ols_stud", "(a) Whole-city model (OLS)")
map_res_gwr <- res_map(gwr_out, "Stud_residual", "(b) Local model (GWR)", legend = FALSE) +
  tm_compass(type = "arrow", position = c("right", "top"), size = 1.2) +
  tm_scalebar(breaks = c(0, 5, 10), text.size = 0.5, position = c("right", "top"))

(map_res_both <- tmap_arrange(map_res_ols, map_res_gwr, ncol = 2))
tmap_save(map_res_both, "all_figures/fig_residuals_ols_gwr.png",
          width = 26, height = 14, units = "cm", dpi = 300)
# Read: (a) clustered light-blue/peach patches (I = 0.61); (b) scattered single SA2s (I = 0.02).
# Blue = less driving than predicted; red = more.


# Who lives where an effect is strong? (describes the significant areas)
describe_by_sig <- function(v) {
  gwr_out %>% st_drop_geometry() %>%
    mutate(class = case_when(gwr_out[[paste0(v, "_sig")]] > 0 ~ "Significant +",
                             gwr_out[[paste0(v, "_sig")]] < 0 ~ "Significant -",
                             TRUE ~ "Not significant")) %>%
    bind_cols(model_data %>% st_drop_geometry() %>%
                select(car_pct_raw = car_pct, PT_transit_time_cbd_mins,
                       total_personal_income_median_2023)) %>%
    group_by(class) %>%
    summarise(n = n(), median_car_pct = median(car_pct_raw),
              median_pt_time = median(PT_transit_time_cbd_mins),
              median_income = median(total_personal_income_median_2023), .groups = "drop")
}
describe_by_sig("income_10k")
describe_by_sig("pt_time_10")


#  Robustness checks (report briefly, e.g. one sentence or an Appendix table)
# Low-income share instead of median income
(f_lowinc   <- update(f_final, . ~ . - income_10k + low_income_pct))
(ols_lowinc <- lm(f_lowinc, data = model_data))
common <- setdiff(names(coef(ols)), "income_10k")
round(cbind(main = coef(ols)[common], low_income_version = coef(ols_lowinc)[common]), 3)
coef(summary(ols_lowinc))["low_income_pct", ]   # the income effect itself
# -> Do the other coefficients keep the same sign and similar size?

#Logit-transformed car share (car share is left-skewed, -1.80)
ols_logit <- lm(update(f_final, qlogis(pmin(pmax(car_pct / 100, 0.001), 0.999)) ~ .),
                data = model_data)   # pmin/pmax guard against 0% or 100%
summary(ols_logit)$coefficients[, c("Estimate", "Pr(>|t|)")]
# -> Same signs and significance as the main model = results are not an
#    artefact of the skewed outcome.




# Save GWR results (for RQ3 and later maps)
gwr_save <- gwr_out %>%
  mutate(sa2_code = model_data$sa2_code, sa2_name = model_data$sa2_name,
         ols_resid = model_data$ols_resid) %>%
  relocate(sa2_code, sa2_name)
st_write(gwr_save, "./data/Saved/gwr_results.gpkg", delete_dsn = TRUE, quiet = TRUE)


#Supplementary: does the PT-time effect depend on income? (OLS only)
# Question: is the link between slow PT and car commuting stronger in lower- or
# higher-income SA2s? (equity angle -> supports RQ3)
# Kept OUT of the GWR: GWR already lets effects vary by place, and interaction
# terms make the small local models unstable.
#
# Centre income and PT time (subtract the mean) so that:
#  - the main effects are read "at average income / average PT time", and
#  - the VIF is not inflated by the product term (structural multicollinearity).
model_data <- model_data %>%
  mutate(income_c  = income_10k - mean(income_10k),
         pt_time_c = pt_time_10 - mean(pt_time_10))

f_int   <- update(f_final, . ~ . - income_10k - pt_time_10 + income_c * pt_time_c)
ols_int <- lm(f_int, data = model_data)
summary(ols_int)
# How to read it:
#  - pt_time_c: effect of +10 min PT time at AVERAGE income
#  - income_c: effect of +$10k income at AVERAGE PT time
#  - income_c:pt_time_c:   how the PT-time effect changes for each +$10k income
#       negative -> slow PT pushes car use up MORE in lower-income SA2s
#       positive -> slow PT pushes car use up MORE in higher-income SA2s
#       p > 0.05 -> no evidence the PT-time effect differs by income

vif(ols_int, type = "predictor")   # correct VIF for models with interactions

# Does the interaction improve the model? (F-test: nested models)
anova(lm(update(f_int, . ~ . - income_c:pt_time_c), data = model_data), ols_int)
# p < 0.05 -> the interaction adds explanatory power

# Plot: PT-time effect at low / average / high income (Appendix or Discussion)
if (requireNamespace("interactions", quietly = TRUE)) {
  p_int <- interactions::interact_plot(ols_int, pred = pt_time_c, modx = income_c,
                                       modx.values = "plus-minus",   # mean +/- 1 SD
                                       x.label = "PT time to CBD (10-min units, centred)",
                                       y.label = "Predicted car share (%)",
                                       legend.main = "Median income\n($10k, centred)")
  print(p_int)
  ggsave("all_figures/fig_appendix_interaction.png", p_int,
         width = 16, height = 10, units = "cm", dpi = 300, bg = "white")
}










