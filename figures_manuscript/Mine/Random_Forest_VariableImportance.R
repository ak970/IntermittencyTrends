# ========================================================
# UPDATED: RandomForest_VariableImportance.R
# ========================================================

source(file.path("code", "paths+packages.R"))
library(tidyverse)
library(patchwork) # Required to stitch the plots together!

# 1. DEFINE COLORS (authors hid these in their helper script)
col.cat.red <- "#e41a1c"
col.cat.blu <- "#377eb8"
col.cat.grn <- "#4daf4a"

# 2. RECREATE YOUR PREDICTOR LISTS TO CATEGORIZE THEM
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
predictors_climate_with_previous <- c(predictors_climate, paste0(predictors_climate, ".previous"))

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
  predictor = c(predictors_climate_with_previous, predictors_human, predictors_static),
  Category = c(rep("Climate", length(predictors_climate_with_previous)),
               rep("Land Use", length(predictors_human)),
               rep("Physiography", length(predictors_static)))
) %>%
  mutate(long_name = predictor) # Using sanitized names as labels

# ========================================================
# LOAD DATA
# ========================================================

# Check folder path (change to "results/Mine" if you saved it there in script 04)
data_dir <- "results/Mine" 

rf_imp <- 
  readr::read_csv(file.path(data_dir, "04_RandomForest_RunModels_VariableImportance.csv"), show_col_types = FALSE) %>% 
  dplyr::left_join(df_pred, by = "predictor") 

fit_pdp_out <- 
  readr::read_csv(file.path(data_dir, "04_RandomForest_RunModels_PartialDependence.csv"), show_col_types = FALSE)

## grab selected predictors to show
n_pred <- 50

# ranked predictors for each
imp_anf <- rf_imp %>% 
  subset(region_rf == "National" & metric == "Zero_Flow_Days_sum") %>% 
  dplyr::slice_max(order_by = IncMSE, n = n_pred) %>% 
  dplyr::arrange(-IncMSE) %>% 
  dplyr::mutate(Predictor = factor(long_name, levels = rev(long_name))) # Fixed factoring

imp_p2z <- rf_imp %>% 
  subset(region_rf == "National" & metric == "mean_dry_down_days") %>% 
  dplyr::slice_max(order_by = IncMSE, n = n_pred) %>% 
  dplyr::arrange(-IncMSE) %>% 
  dplyr::mutate(Predictor = factor(long_name, levels = rev(long_name)))

imp_zff <- rf_imp %>% 
  subset(region_rf == "National" & metric == "hydro_doy") %>% 
  dplyr::slice_max(order_by = IncMSE, n = n_pred) %>% 
  dplyr::arrange(-IncMSE) %>% 
  dplyr::mutate(Predictor = factor(long_name, levels = rev(long_name)))

# extract pdp
pdp_anf <- fit_pdp_out %>% 
  subset(region == "National" & metric == "Zero_Flow_Days_sum") %>% 
  subset(predictor %in% imp_anf$predictor) %>% 
  dplyr::select(-region, -metric) %>% 
  dplyr::left_join(imp_anf, by = "predictor")

pdp_p2z <- fit_pdp_out %>% 
  subset(region == "National" & metric == "mean_dry_down_days") %>% 
  subset(predictor %in% imp_p2z$predictor) %>% 
  dplyr::select(-region, -metric) %>% 
  dplyr::left_join(imp_p2z, by = "predictor")

pdp_zff <- fit_pdp_out %>% 
  subset(region == "National" & metric == "hydro_doy") %>% 
  subset(predictor %in% imp_zff$predictor) %>% 
  dplyr::select(-region, -metric) %>% 
  dplyr::left_join(imp_zff, by = "predictor")

# ========================================================
# BAR PLOTS
# ========================================================

p_nat_anf <- ggplot(imp_anf, aes(x = Predictor, y = IncMSE/oobMSE, fill = Category)) +
  geom_col() +
  scale_fill_manual(drop = F, values = c("Climate" = col.cat.red, "Physiography" = col.cat.blu, "Land Use" = col.cat.grn)) +
  scale_y_continuous(name = "MSE Increase [%]", labels = scales::percent) +
  coord_flip() +
  labs(title = "(a) No-Flow Days") +
  theme_bw() + theme(axis.title.y = element_blank())

p_nat_p2z <- ggplot(imp_p2z, aes(x = Predictor, y = IncMSE/oobMSE, fill = Category)) +
  geom_col() +
  scale_fill_manual(drop = F, values = c("Climate" = col.cat.red, "Physiography" = col.cat.blu, "Land Use" = col.cat.grn)) +
  scale_y_continuous(name = "MSE Increase [%]", labels = scales::percent) +
  coord_flip() +
  labs(title = "(b) Peak to No-Flow") +
  theme_bw() + theme(axis.title.y = element_blank())

p_nat_zff <- ggplot(imp_zff, aes(x = Predictor, y = IncMSE/oobMSE, fill = Category)) +
  geom_col() +
  scale_fill_manual(drop = F, values = c("Climate" = col.cat.red, "Physiography" = col.cat.blu, "Land Use" = col.cat.grn)) +
  scale_y_continuous(name = "MSE Increase [%]", labels = scales::percent) +
  coord_flip() +
  labs(title = "(c) First No-Flow Day") +
  theme_bw() + theme(axis.title.y = element_blank())

# if(!dir.exists("figures_manuscript")) dir.create("figures_manuscript")

# Stitch them together using Patchwork!
final_bar_plot <- (p_nat_anf + p_nat_p2z + p_nat_zff) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom", plot.title = element_text(face = "plain"))

ggsave(file.path("figures_manuscript/Mine", "RandomForest_VariableImportance.png"),
       plot = final_bar_plot, width = 250, height = 100, units = "mm")

# ========================================================
# PARTIAL DEPENDENCE PLOTS (PDPs)
# ========================================================

p_pdp_anf <- ggplot(pdp_anf, aes(x = value, y = yhat, color = Category)) +
  geom_line(size = 1) +
  facet_wrap(~ Predictor, scales = "free_x") +
  scale_color_manual(drop = F, values = c("Climate" = col.cat.red, "Physiography" = col.cat.blu, "Land Use" = col.cat.grn)) +
  labs(title = "No-Flow Days PDP", x = "Scaled Value of Variable [z-score]", y = "Predicted No-Flow Days") +
  theme_bw() + theme(legend.position = "bottom")

ggsave(file.path("figures_manuscript/Mine", "RandomForest_VariableImportance-PartialDependencePlots_NoFlowDays.png"),
       plot = p_pdp_anf, width = 190, height = 190, units = "mm")

p_pdp_p2z <- ggplot(pdp_p2z, aes(x = value, y = yhat, color = Category)) +
  geom_line(size = 1) +
  facet_wrap(~ Predictor, scales = "free_x") +
  scale_color_manual(drop = F, values = c("Climate" = col.cat.red, "Physiography" = col.cat.blu, "Land Use" = col.cat.grn)) +
  labs(title = "Peak to No-Flow PDP", x = "Scaled Value of Variable [z-score]", y = "Predicted Days") +
  theme_bw() + theme(legend.position = "bottom")

ggsave(file.path("figures_manuscript/Mine", "RandomForest_VariableImportance-PartialDependencePlots_PeakToZero.png"),
       plot = p_pdp_p2z, width = 190, height = 190, units = "mm")

p_pdp_zff <- ggplot(pdp_zff, aes(x = value, y = yhat, color = Category)) +
  geom_line(size = 1) +
  facet_wrap(~ Predictor, scales = "free_x") +
  scale_color_manual(drop = F, values = c("Climate" = col.cat.red, "Physiography" = col.cat.blu, "Land Use" = col.cat.grn)) +
  labs(title = "First No-Flow Day PDP", x = "Scaled Value of Variable [z-score]", y = "Predicted Day of Year") +
  theme_bw() + theme(legend.position = "bottom")

ggsave(file.path("figures_manuscript/Mine", "RandomForest_VariableImportance-PartialDependencePlots_ZeroFlowFirst.png"),
       plot = p_pdp_zff, width = 190, height = 190, units = "mm")

print("Plotting Complete! Check the figures_manuscript folder.")