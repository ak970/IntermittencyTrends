## 03_RandomForest_TuneHyperparameters.R
#' This script is intended to tune random forest hyperparameters:
#'  - mtry
#'  - ntree
#'  - min_n
#' 
#' Input variables will be selected using the output from 01_RandomForest_PreliminaryVariableImportance.R
#' 

source(file.path("code", "paths+packages.R"))
library(tidymodels)

# 1. Load Data
gage_sample <- 
  readr::read_csv(file = file.path("results/Mine", "Physiographic_data.csv"))

gage_regions <- 
  readr::read_csv(file.path("results/Mine", "station_ai_regions.csv"))

gage_sample_annual <-
  readr::read_csv(file = file.path("results/Mine", "gauges_annual_summary_out.csv"))

metrics <- c("Zero_Flow_Days_sum", "hydro_doy", "mean_dry_down_days")
regions <- c("National", na.omit(unique(gage_sample_annual$region)))

# 2. DEFINING PREDICTORS (Exact same as Script 01)
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

# previous year predictors will be calculated further down
predictors_climate_with_previous <- 
  c(predictors_climate, c("p_mm_cy.previous", "p_mm_jas.previous", "p_mm_ond.previous", 
                          "p_mm_jfm.previous", "p_mm_amj.previous", "pet_mm_cy.previous", 
                          "pet_mm_jas.previous", "pet_mm_ond.previous", "pet_mm_jfm.previous", 
                          "pet_mm_amj.previous", "T_max_c_cy.previous", "T_max_c_jas.previous", 
                          "T_max_c_ond.previous", "T_max_c_jfm.previous", "T_max_c_amj.previous",
                          "swe_mm_cy.previous", "swe_mm_jas.previous", "swe_mm_ond.previous", 
                          "swe_mm_jfm.previous", "swe_mm_amj.previous", "p.pet_cy.previous", 
                          "swe.p_cy.previous", "p.pet_jfm.previous", "swe.p_jfm.previous", 
                          "p.pet_amj.previous", "swe.p_amj.previous", "p.pet_jas.previous", 
                          "swe.p_jas.previous", "p.pet_ond.previous", "swe.p_ond.previous"))

# 3. Sanitize names so formulas don't crash
names(gage_sample_annual) <- make.names(names(gage_sample_annual))
names(gage_sample) <- make.names(names(gage_sample))
predictors_climate <- make.names(predictors_climate)
predictors_human <- make.names(predictors_human)
predictors_static <- make.names(predictors_static)
metrics <- make.names(metrics)

## 4. Calculate previous year climate metrics
gage_sample_prevyear <- 
  gage_sample_annual %>% 
  dplyr::select(gauge_id, hydro_year, all_of(predictors_climate)) %>% 
  dplyr::mutate(wyearjoin = hydro_year + 1) %>% 
  dplyr::select(-hydro_year)

# adds .previous suffix to the climate predictors
names(gage_sample_prevyear)[names(gage_sample_prevyear) %in% predictors_climate] <- 
  paste0(names(gage_sample_prevyear)[names(gage_sample_prevyear) %in% predictors_climate], ".previous")

## 5. Combine into one master fit dataset
fit_data_in <- 
  gage_sample_annual %>% 
  dplyr::select(gauge_id, hydro_year, Sample, region, all_of(metrics), 
                all_of(predictors_climate), any_of(predictors_human)) %>% 
  dplyr::left_join(gage_sample_prevyear, by = c("gauge_id", "hydro_year"="wyearjoin")) %>% 
  dplyr::left_join(gage_sample %>% dplyr::select(gauge_id, any_of(predictors_static)), by = "gauge_id")



## set up tuning parameter space
# set up model engine
rf_tune <- 
  rand_forest(trees = tune(), 
              mtry = tune(), 
              min_n = tune()) %>% 
  set_engine("ranger", num.threads = (parallel::detectCores() - 1)) %>% 
  set_mode("regression")

# create grid of parameters for tuning
rf_tune_grid <- grid_regular(trees(range = c(250, 1650)),
                             mtry(range = c(1, 10)),
                             min_n(range = c(3, 25)),
                             levels = 8)

## loop through metrics and regions
# choose number of predictors - based on script 02_RandomForest_FigureOutNumPredictors.R
npred_final <- tibble::tibble(metric = c("Zero_Flow_Days_sum", "hydro_doy", "mean_dry_down_days"),
                              npred = c(22, 27, 27)) 
n_folds <- 5 # choose number of folds for cross-val

for (m in metrics){
  # subset to training data, predictors
  fit_data_m <- 
    fit_data_in %>% 
    subset(Sample == "Train")
  
  # rename metric column
  names(fit_data_m)[names(fit_data_m) == m] <- "observed"
  
  # determine number of predictors
  n_pred <- npred_final$npred[npred_final$metric == m]
  
  for (r in regions){
    # get predictor variables
    rf_var_m_r <-
      file.path("results/Mine/Run2", paste0("01_RandomForest_PreliminaryVariableImportance_", m, "_", gsub(" ", "", r, fixed = TRUE), ".csv")) %>% 
      readr::read_csv() %>% 
      dplyr::slice_max(order_by = ImpCondPerm, n = n_pred)
    
    if (r == "National") {
      fit_data_r <- 
        fit_data_m %>% 
        dplyr::select(gage_ID, CLASS, currentclimyear, observed, region, all_of(rf_var_m_r$predictor)) %>% 
        subset(complete.cases(.))
    } else {
      fit_data_r <- 
        fit_data_m %>% 
        subset(region == r) %>% 
        dplyr::select(gage_ID, CLASS, currentclimyear, observed, region, all_of(rf_var_m_r$predictor)) %>% 
        subset(complete.cases(.))
    }
    
    # set up folds
    tune_folds <- vfold_cv(fit_data_r, v = 5, strata = region)
    
    # set up recipe
    tune_recipe <-
      fit_data_r %>% 
      recipe(observed ~ .) %>%
      update_role(gage_ID, currentclimyear, region, CLASS, new_role = "ID") %>% 
      step_normalize(all_predictors(), -all_outcomes())
    
    # build tuning workflow
    tune_wf <-
      workflow() %>% 
      add_model(rf_tune) %>% 
      add_recipe(tune_recipe)
    
    # run tuning
    tune_res <-
      tune_wf %>% 
      tune_grid(
        resamples = tune_folds,
        grid = rf_tune_grid,
        metrics = metric_set(mae, rmse, rsq)
      )
    
    # collect results
    tune_res_m_r <-
      tune_res %>% 
      collect_metrics() %>% 
      dplyr::mutate(metric = m, 
                    region_rf = r)
    
    if (m == metrics[1] & r == regions[1]){
      tune_res_all <- tune_res_m_r
    } else {
      tune_res_all <- dplyr::bind_rows(tune_res_all, tune_res_m_r)
    }
    
    # status update
    print(paste0(m, " ", r, " complete, ", Sys.time()))
    
  }
}

# save data
tune_res_all %>% 
  readr::write_csv(file.path("results/Mine", "03_RandomForest_TuneHyperparameters.csv"))