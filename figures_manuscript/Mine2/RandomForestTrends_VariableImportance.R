# ========================================================
# UPDATED: RandomForestTrends_VariableImportance.R
# ========================================================

source(file.path("code", "paths+packages.R"))
library(tidyverse)
library(patchwork)

# 1. DEFINE COLORS
col.cat.red <- "#e41a1c"
col.cat.blu <- "#377eb8"
col.cat.grn <- "#4daf4a"

# 2. RECREATE YOUR PREDICTOR LISTS TO CATEGORIZE THEM
# Note: Because trends use a 40-year average, there are no ".previous" variables here!
predictors_climate <- make.names(c(
  'tmax', 'tmin', 'tavg', 'srad_lw(w/m2)', 'srad_sw(w/m2)', 'wind_u(m/s)', 'wind_v(m/s)',
  'wind(m/s)', 'rel_hum(%)', 'sm_lvl1(kg/m2)', 'sm_lvl2(kg/m2)', 'sm_lvl3(kg/m2)', 'sm_lvl4(kg/m2)', 
  'p', 'pet', 'aet', 'evap_canopy(mm/day)', 'evap_surface(mm/day)', 'tmax_djf', 'tmax_jjas',
  'tmax_mam', 'tmax_on', 'tmin_djf', 'tmin_jjas', 'tmin_mam', 'tmin_on', 'tavg_djf', 'tavg_jjas', 
  'tavg_mam', 'tavg_on', 'p_djf', 'p_jjas', 'p_mam', 'p_on', 'pet_djf', 'pet_jjas', 'pet_mam', 
  'pet_on', 'aet_djf', 'aet_jjas', 'aet_mam', 'aet_on', 'evap_canopy(mm/day)_djf',
  'evap_canopy(mm/day)_jjas', 'evap_canopy(mm/day)_mam', 'evap_canopy(mm/day)_on', 
  'evap_surface(mm/day)_djf', 'evap_surface(mm/day)_jjas', 'evap_surface(mm/day)_mam',
  'evap_surface(mm/day)_on', 'spei3_drought_count', 'spei6_drought_count',
  'spei12_drought_count', 'spei24_drought_count', 'spei3_drought_valid_n',
  'spei6_drought_valid_n', 'spei12_drought_valid_n', 'spei24_drought_valid_n', 
  'spei3_drought_max_spell', 'spei6_drought_max_spell', 'spei12_drought_max_spell',
  'spei24_drought_max_spell', 'spei3_drought_count_frac', 'spei6_drought_count_frac', 
  'spei12_drought_count_frac', 'spei24_drought_count_frac'
))

predictors_human <- make.names(c('urban_pct', 'Total_Dams_So_Far', 'Total_Volume_So_Far'))

predictors_static <- make.names(c(
  'num_dams', 'res_store_sum', 'total_storage', 'geol_porosity', 'geol_permeability', 
  'carb_rocks_frac', 'lai_mean', 'lai_min', 'lai_max', 'lai_diff', 'soil_depth', 
  'soil_conductivity_top', 'soil_conductivity_sub', 'soil_awc_top', 'soil_awc_sub', 
  'soil_awsc_min', 'soil_awsc_max', 'soil_awsc_major', 'sand_frac_top', 'sand_frac_sub', 
  'silt_frac_top', 'silt_frac_sub', 'clay_frac_top', 'clay_frac_sub', 'gravel_frac_top', 
  'gravel_frac_sub', 'bulkdens_top_major', 'bulkdense_top_mean', 'bulkdens_sub_mean', 
  'org_carb_top_major', 'org_carb_top_mean', 'org_carb_sub_major', 'org_carb_sub_mean', 
  'organic_frac_top', 'organic_frac_sub', 'cwc_lat', 'cwc_lon', 'elev_mean', 'elev_median', 
  'elev_min', 'elev_max', 'slope_mean', 'slope_median', 'slope_min', 'slope_max', 
  'cwc_area', 'dpsbar', 'sinuosity' 
))

# 3. CREATE DYNAMIC df_pred TABLE
df_pred <- data.frame(
  predictor = c(predictors_climate, predictors_human, predictors_static),
  Category = c(rep("Climate", length(predictors_climate)),
               rep("Land Use", length(predictors_human)),
               rep("Physiography", length(predictors_static)))
) %>%
  mutate(long_name = predictor) 

# ========================================================
# 4. LOAD DATA
# ========================================================
data_dir <- "results/Mine"

rf_imp <- readr::read_csv(file.path(data_dir, "RandomForestTrends_03_RunModels_VariableImportance.csv"), show_col_types = FALSE) %>% 
  dplyr::left_join(df_pred, by = "predictor")

if(!dir.exists("figures_manuscript")) dir.create("figures_manuscript")

# ========================================================
# 5. PREP TOP PREDICTORS
# ========================================================
n_pred <- 12 # Changed from 9 to 12 to give a slightly richer plot!

# Ranked predictors for each metric
imp_anf <- rf_imp %>% 
  subset(metric == "Zero_Flow_Days_sum") %>% 
  dplyr::slice_max(order_by = IncMSE, n = n_pred) %>% 
  dplyr::arrange(-IncMSE) %>% 
  dplyr::mutate(Predictor = factor(long_name, levels = rev(long_name)))

imp_p2z <- rf_imp %>% 
  subset(metric == "mean_dry_down_days") %>% 
  dplyr::slice_max(order_by = IncMSE, n = n_pred) %>% 
  dplyr::arrange(-IncMSE) %>% 
  dplyr::mutate(Predictor = factor(long_name, levels = rev(long_name)))

imp_zff <- rf_imp %>% 
  subset(metric == "hydro_doy") %>% 
  dplyr::slice_max(order_by = IncMSE, n = n_pred) %>% 
  dplyr::arrange(-IncMSE) %>% 
  dplyr::mutate(Predictor = factor(long_name, levels = rev(long_name)))

# ========================================================
# 6. PLOT BAR CHARTS
# ========================================================
# Helper function to create the plots cleanly
create_imp_bar <- function(data, title) {
  ggplot(data, aes(x = Predictor, y = IncMSE/oobMSE, fill = Category)) +
    geom_col(color = "black", size = 0.2) +
    scale_fill_manual(drop = F,
                      values = c("Climate" = col.cat.red, 
                                 "Physiography" = col.cat.blu,
                                 "Land Use" = col.cat.grn)) +
    scale_y_continuous(name = "MSE Increase [%]", labels = scales::percent) +
    coord_flip() +
    labs(title = title) +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          plot.title = element_text(size = 10, face = "bold"))
}

p_nat_anf <- create_imp_bar(imp_anf, "(a) Kendall \u03c4,\n     No-Flow Days")
p_nat_p2z <- create_imp_bar(imp_p2z, "(b) Kendall \u03c4,\n     Peak to No-Flow")
p_nat_zff <- create_imp_bar(imp_zff, "(c) Kendall \u03c4,\n     First No-Flow Day")

# Combine using patchwork
final_plot <- (p_nat_anf + p_nat_p2z + p_nat_zff) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom")

# Save to figures_manuscript
ggsave(file.path("figures_manuscript", "RandomForestTrends_VariableImportance.png"),plot = 
       plot = final_plot, width = 220, height = 110, units = "mm", bg = "white")

print("Trend Variable Importance plots generated successfully! Check figures_manuscript/RandomForestTrends_VariableImportance.png")