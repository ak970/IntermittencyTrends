# ========================================================
# UPDATED: RandomForestTrends_01_VariableImportance+NumPredictors.R
# ========================================================

source(file.path("code", "paths+packages.R"))
library(tidymodels)
library(partykit)
library(dplyr)
library(reshape2)

# ========================================================
# 1. DEFINE METRICS AND PREDICTORS
# ========================================================
metrics <- c("Zero_Flow_Days_sum", "mean_dry_down_days", "hydro_doy")

predictors_climate <- c(
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
)

predictors_human <- c('urban_pct', 'Total_Dams_So_Far', 'Total_Volume_So_Far')

predictors_static <- c(
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
)

# Sanitize names!
predictors_climate <- make.names(predictors_climate)
predictors_human <- make.names(predictors_human)
predictors_static <- make.names(predictors_static)
metrics <- make.names(metrics)

predictors_all <- c(predictors_human, predictors_static, predictors_climate)

# ========================================================
# 2. LOAD DATA
# ========================================================
data_dir <- "results/Mine"

gage_regions <- readr::read_csv(file.path(data_dir, "station_ai_regions.csv"), show_col_types = FALSE) %>% mutate(gauge_id = as.character(gauge_id))
gage_mean <- readr::read_csv(file.path(data_dir, "Physiographic_data.csv"), show_col_types = FALSE) %>% mutate(gauge_id = as.character(gauge_id))
names(gage_mean) <- make.names(names(gage_mean))

gage_trends <- readr::read_csv(file.path(data_dir, "gauge_trends.csv"), show_col_types = FALSE) %>% mutate(gauge_id = as.character(gauge_id))
gage_sample_annual <- readr::read_csv(file.path(data_dir, "gauges_annual_summary_out.csv"), show_col_types = FALSE) %>% mutate(gauge_id = as.character(gauge_id))
names(gage_sample_annual) <- make.names(names(gage_sample_annual))

# Grab trend data - one row per gauge
df_trends_tau <- gage_trends %>% 
  subset(metric %in% metrics) %>% 
  tidyr::pivot_wider(id_cols = c("gauge_id"), names_from = "metric", values_from = "mk_tau", names_prefix = "tau_")

# ========================================================
# 3. COMPRESS ANNUAL DATA TO MEAN VALUES (1 ROW PER GAUGE)
# ========================================================
data_all <- gage_sample_annual %>% 
  dplyr::select(gauge_id, any_of(predictors_climate), any_of(predictors_human)) %>% 
  dplyr::group_by(gauge_id) %>% 
  # Collapse 40 years of data into the Long-Term Mean for each gauge!
  dplyr::summarize(across(everything(), ~mean(.x, na.rm = TRUE))) %>% 
  # Add static variables
  dplyr::left_join(gage_mean %>% dplyr::select(gauge_id, any_of(predictors_static)), by = "gauge_id") %>% 
  # Add in trends (the target variables)
  dplyr::left_join(df_trends_tau, by = "gauge_id") %>% 
  # Add regions
  dplyr::left_join(gage_regions, by = "gauge_id") %>%
  filter(!is.na(region))

# ========================================================
# 4. FILTER HIGHLY CORRELATED PREDICTORS (To prevent overfitting small N)
# ========================================================
check_cor <- cor(data_all %>% dplyr::select(all_of(predictors_all)), use = "pairwise.complete.obs", method = "pearson")
check_cor[is.na(check_cor)] <- 0 
check_cor[lower.tri(check_cor, diag = TRUE)] <- 0 

cor_high <- check_cor %>% reshape2::melt() %>% subset(abs(value) > 0.85 & Var1 != Var2) 
predictors_drop <- as.character(unique(cor_high$Var2))
predictors_trimmed <- predictors_all[!(predictors_all %in% predictors_drop)]

cat(paste("\nDropped", length(predictors_drop), "highly correlated variables. Remaining:", length(predictors_trimmed), "\n"))

# ========================================================
# 5. TRAIN / TEST SPLIT
# ========================================================
set.seed(1)
test <- data_all %>% 
  dplyr::group_by(region) %>% 
  dplyr::sample_frac(0.2) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Sample = "Test")

train <- data_all %>% 
  dplyr::mutate(Sample = "Train") %>% 
  subset(!(gauge_id %in% test$gauge_id))

fit_data_in <- dplyr::bind_rows(test, train)

# Final Predictor List
predictors_final <- predictors_trimmed

# ========================================================
# 6. RUN RANDOM FORESTS & OPTIMIZE PREDICTORS
# ========================================================
ncores <- 1 # Forced to 1 for mathematical stability on small datasets!
fit_predtest <- data.frame()
fit_varimp_all <- data.frame()

for (m in metrics){
  tau_m <- paste0("tau_", m)
  
  fit_data_r <- fit_data_in %>% 
    dplyr::select(gauge_id, Sample, all_of(tau_m), all_of(predictors_final)) %>% 
    subset(complete.cases(.))
  
  names(fit_data_r)[names(fit_data_r) == tau_m] <- "observed"
  
  fit_data_train <- subset(fit_data_r, Sample == "Train")
  
  ## STEP 1: VARIABLE IMPORTANCE (Using Standard Permutation for stability)
  cat(paste("\nCalculating Variable Importance for:", tau_m, "...\n"))
  fit_varimp <- partykit::cforest(
    observed ~ .,
    data = dplyr::select(fit_data_train, -gauge_id, -Sample),
    control = ctree_control(mincriterion = 0.95),
    ntree = 500, cores = ncores
  )
  
  vi <- partykit::varimp(fit_varimp, conditional = FALSE, cores = ncores) # <--- Safe standard math!
  
  fit_varimp_i <- tibble::tibble(predictor = names(vi), ImpCondPerm = vi) %>% 
    dplyr::arrange(-ImpCondPerm) %>% 
    dplyr::mutate(metric = tau_m)
  
  ## STEP 2: TEST NUMBER OF PREDICTORS (Find the Elbow)
  pred_test_range <- 5:min(30, length(fit_varimp_i$predictor)) # Test 5 to 30 variables
  
  for (n_pred in pred_test_range){
    
    rf_var_predtest <- fit_varimp_i %>% dplyr::top_n(n = n_pred, wt = ImpCondPerm)
    
    fit_data_predtest <- fit_data_train %>% 
      dplyr::select(observed, all_of(rf_var_predtest$predictor)) %>%  
      subset(complete.cases(.))
    
    set.seed(1)
    rf_recipe <- recipe(observed ~ ., data = fit_data_predtest) %>% step_normalize(all_predictors(), -all_outcomes())
    rf_engine <- rand_forest() %>% set_engine("ranger", num.threads = ncores) %>% set_mode("regression")
    
    rf_wflow <- workflow() %>% add_model(rf_engine) %>% add_recipe(rf_recipe)
    rf_fit <- rf_wflow %>% parsnip::fit(data = fit_data_predtest)
    
    fit_predtest <- dplyr::bind_rows(fit_predtest, tibble::tibble(
      metric = tau_m, n = n_pred,
      OOBmse = extract_fit_engine(rf_fit)$prediction.error,
      OOBr2 = extract_fit_engine(rf_fit)$r.squared
    ))
  }
  
  fit_varimp_all <- dplyr::bind_rows(fit_varimp_all, fit_varimp_i)
  print(paste0(tau_m, " optimization complete, ", Sys.time()))
}

# ========================================================
# 7. SAVE OUTPUTS AND PLOT
# ========================================================
if(!dir.exists("figures_manuscript")) dir.create("figures_manuscript")

# Save Data Frame (Very important for later scripts!)
readr::write_csv(fit_data_in, file = file.path("results/Mine2", "RandomForestTrends_01_RFinputData.csv"))
readr::write_csv(fit_varimp_all, file = file.path("results/Mine2", "RandomForestTrends_02_PreliminaryVariableImportance.csv"))
readr::write_csv(fit_predtest, file = file.path("results/Mine2", "RandomForestTrends_02_NumPredictors.csv"))

# Automatically find optimal (Minimum MSE)
npred_final <- fit_predtest %>% 
  dplyr::group_by(metric) %>% 
  dplyr::filter(OOBmse == min(OOBmse)) %>%
  dplyr::select(metric, npred = n) %>%
  dplyr::distinct()

print("Optimal Number of Predictors for Trends:")
print(npred_final)

# Plot and look for elbow
p_mse <- ggplot(fit_predtest, aes(x = n, y = OOBmse)) + 
  geom_vline(data = npred_final, aes(xintercept = npred), color = "red", linetype = "dashed") +
  geom_point() + geom_line() +
  scale_x_continuous(name = "Number of Predictors") +
  scale_y_continuous(name = "Random Forest OOB MSE [training gages only]") +
  facet_grid(metric~., scales = "free_y",
             labeller = as_labeller(c("tau_Zero_Flow_Days_sum" = "\u03c4, Annual\nNo-Flow Days", 
                                      "tau_mean_dry_down_days" = "\u03c4, Days from Peak\nto No Flow",
                                      "tau_hydro_doy" = "\u03c4, First\nNo-Flow Day"))) +
  labs(title = "Random Forest Trend MSE vs. Number of Predictors",
       subtitle = "National model predicting Kendall Tau (\u03c4)") +
  theme_bw()

ggsave(file.path("figures_manuscript/Mine2", "RandomForestTrends_MSEvNumberPredictors.png"),
       plot = p_mse, width = 150, height = 150, units = "mm")