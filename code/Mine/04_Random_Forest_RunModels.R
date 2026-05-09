## 04_RandomForest_RunModels.R
#' This script is intended to train and run random forest models.
#' 
#' Input variables will be selected using the output from 01_RandomForest_PreliminaryVariableImportance.R
#' and 02_RandomForest_FigureOutNumPredictors.R. Hyperparameters will be based on the output of 
#' 03_RandomForest_TuneHyperparameters.R
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
  c(predictors_climate, c('tmax.previous', 'tmin.previous', 'tavg.previous', 'srad_lw(w/m2).previous', 
                          'srad_sw(w/m2).previous', 'wind_u(m/s).previous', 'wind_v(m/s).previous',
                          'wind(m/s).previous', 'rel_hum(%).previous', 'sm_lvl1(kg/m2).previous', 
                          'sm_lvl2(kg/m2).previous', 'sm_lvl3(kg/m2).previous', 'sm_lvl4(kg/m2).previous', 
                          'p.previous', 'pet.previous', 'aet.previous', 'evap_canopy(mm/day).previous', 
                          'evap_surface(mm/day).previous', 'tmax_djf.previous', 'tmax_jjas.previous',
                          'tmax_mam.previous', 'tmax_on.previous', 'tmin_djf.previous', 'tmin_jjas.previous', 
                          'tmin_mam.previous', 'tmin_on.previous', 'tavg_djf.previous', 'tavg_jjas.previous', 
                          'tavg_mam.previous', 'tavg_on.previous', 'p_djf.previous', 'p_jjas.previous', 
                          'p_mam.previous', 'p_on.previous', 'pet_djf.previous', 'pet_jjas.previous', 
                          'pet_mam.previous', 'pet_on.previous', 'aet_djf.previous', 'aet_jjas.previous', 
                          'aet_mam.previous', 'aet_on.previous', 'evap_canopy(mm/day)_djf.previous',
                          'evap_canopy(mm/day)_jjas.previous', 'evap_canopy(mm/day)_mam.previous', 
                          'evap_canopy(mm/day)_on.previous', 'evap_surface(mm/day)_djf.previous', 
                          'evap_surface(mm/day)_jjas.previous', 'evap_surface(mm/day)_mam.previous',
                          'evap_surface(mm/day)_on.previous', 'spei3_drought_count.previous',
                          'spei6_drought_count.previous', 'spei12_drought_count.previous', 
                          'spei24_drought_count.previous', 'spei3_drought_valid_n.previous',
                          'spei6_drought_valid_n.previous', 'spei12_drought_valid_n.previous',
                          'spei24_drought_valid_n.previous', 'spei3_drought_max_spell.previous', 
                          'spei6_drought_max_spell.previous', 'spei12_drought_max_spell.previous',
                          'spei24_drought_max_spell.previous', 'spei3_drought_count_frac.previous', 
                          'spei6_drought_count_frac.previous', 'spei12_drought_count_frac.previous', 
                          'spei24_drought_count_frac.previous'))

predictors_all <- c(predictors_human, predictors_static, predictors_climate_with_previous)

# 3. Sanitize names so formulas don't crash
names(gage_sample_annual) <- make.names(names(gage_sample_annual))
names(gage_sample) <- make.names(names(gage_sample))
predictors_climate <- make.names(predictors_climate)
predictors_human <- make.names(predictors_human)
predictors_climate_with_previous <- make.names(predictors_climate_with_previous)
predictors_all <- make.names(predictors_all)
predictors_static <- make.names(predictors_static)
metrics <- make.names(metrics)

## 4. Calculate previous year climate metrics
gage_sample_prevyear <- 
  gage_sample_annual %>% 
  dplyr::select(gauge_id, hydro_year, all_of(predictors_climate)) %>% 
  dplyr::mutate(wyearjoin = hydro_year + 1) %>% 
  dplyr::select(-hydro_year)

## combine into one data frame
fit_data_in <- 
  gage_sample_annual %>% 
  # subset to fewer columns - metrics and predictors
  dplyr::select(c("gauge_id", "hydro_year", "Sample", all_of(metrics), 
                  all_of(predictors_climate), all_of(predictors_human))) %>% 
  # join with previous water year
  dplyr::left_join(gage_sample_prevyear, 
                   by = c("gauge_id", "hydro_year"="wyearjoin"), 
                   suffix = c("", ".previous")) %>% 
  # join with static predictors
  dplyr::left_join(gage_sample[ , c("gauge_id", "region", predictors_static)], by = "gauge_id")

## load hyperparameter tuning results
tune_res_all <- readr::read_csv(file.path("results/Mine2", "03_RandomForest_TuneHyperparameters.csv"))

## loop through metrics and regions
# # choose number of predictors - based on script 02_RandomForest_FigureOutNumPredictors.R
# npred_final <- tibble::tibble(metric = c("Zero_Flow_Days_sum", "hydro_doy", "mean_dry_down_days"),
#                               npred = c(22, 27, 27)) 

# Automatically load the optimal predictors for EVERY region and metric
npred_final <- readr::read_csv(file.path("results/Mine2", "02_RandomForest_FigureOutNumPredictors.csv"), show_col_types = FALSE) %>% 
  dplyr::group_by(metric, region) %>% 
  dplyr::filter(OOBmse == min(OOBmse)) %>%
  dplyr::select(metric, region, npred = n) %>%
  dplyr::distinct()


for (m in metrics){
  
  # # determine number of predictors
  # n_pred <- npred_final$npred[npred_final$metric == m]
  
  for (r in regions){
    
    # Now it dynamically finds the perfect number for this specific metric AND region
    n_pred <- npred_final$npred[npred_final$metric == m & npred_final$region == r]
    
    # get predictor variables
    rf_var_m_r <-
      file.path("results/Mine", paste0("01_RandomForest_PreliminaryVariableImportance_", m, "_", gsub(" ", "", r, fixed = TRUE), ".csv")) %>% 
      readr::read_csv() %>% 
      dplyr::slice_max(order_by = ImpCondPerm, n = n_pred)
    
    if (r == "National") {
      fit_data_r <- 
        fit_data_in %>% 
        dplyr::select(gauge_id, hydro_year, region, Sample, all_of(m), all_of(rf_var_m_r$predictor)) %>% 
        subset(complete.cases(.))
    } else {
      fit_data_r <- 
        fit_data_in %>% 
        subset(region == r) %>% 
        dplyr::select(gauge_id, hydro_year, region, Sample, all_of(m), all_of(rf_var_m_r$predictor)) %>% 
        subset(complete.cases(.))
    }
    
    names(fit_data_r)[names(fit_data_r)==m] <- "observed"
    
    # split into training/testing
    fit_data_train <- 
      fit_data_r %>% 
      subset(Sample == "Train")
    fit_data_test <- 
      fit_data_r %>% 
      subset(Sample == "Test")
    
    # set up model engine
    tune_res <-
      tune_res_all %>% 
      subset(metric == m & region_rf == r & .metric == "mae") %>% 
      dplyr::filter(mean == min(mean))
    
    rf_engine <- 
      rand_forest(trees = tune_res$trees[1], 
                  mtry = tune_res$mtry[1], 
                  min_n = tune_res$min_n[1]) %>% 
      set_engine("ranger", 
                 num.threads = (parallel::detectCores() - 1),
                 importance = "permutation") %>% 
      set_mode("regression")
    
    # set up recipe
    rf_recipe <-
      fit_data_train %>% 
      recipe(observed ~ .) %>%
      update_role(gauge_id, hydro_year, region, Sample, new_role = "ID") %>% 
      step_normalize(all_predictors(), -all_outcomes())
    
    # set up workflow
    rf_workflow <-
      workflow() %>% 
      add_model(rf_engine) %>% 
      add_recipe(rf_recipe)
    
    # fit model
    rf_fit <- 
      rf_workflow %>% 
      fit(data = fit_data_train)
    
    # predict
    fit_data_train$predicted <- predict(rf_fit, fit_data_train)$.pred
    fit_data_test$predicted <-  predict(rf_fit, fit_data_test)$.pred
    
    # combine training and test output
    fit_data_i <- 
      dplyr::bind_rows(fit_data_train, fit_data_test) %>% 
      dplyr::select(gauge_id, hydro_year, observed, predicted) %>% 
      dplyr::mutate(region_rf = r,
                    metric = m) %>% 
      dplyr::left_join(dplyr::select(fit_data_in,
                                     gauge_id, hydro_year, region, Sample, all_of(predictors_all)), 
                       by = c("gauge_id", "hydro_year"))
    
    # extract variable importance
    fit_rf_imp_i <- tibble::tibble(predictor = names(pull_workflow_fit(rf_fit)$fit$variable.importance),
                                   IncMSE = pull_workflow_fit(rf_fit)$fit$variable.importance,
                                   oobMSE = pull_workflow_fit(rf_fit)$fit$prediction.error,
                                   metric = m,
                                   region_rf = r)
    
    # partial dependence plots for all variables
    ranger_fit <- ranger::ranger(observed ~ ., 
                                 data = dplyr::bind_cols(rf_fit$pre$mold$outcomes, 
                                                         rf_fit$pre$mold$predictors),
                                 num.trees = tune_res$trees[1],
                                 mtry = tune_res$mtry[1], 
                                 min.node.size = tune_res$min_n[1],
                                 num.threads = (parallel::detectCores() - 1))
    
    
    for (v in 1:n_pred){
      var <- rf_var_m_r$predictor[v]
      df_pdp_var <- 
        pdp::partial(ranger_fit, 
                     pred.var = var,
                     mtry = tune_res$mtry[1], 
                     min.node.size = tune_res$min_n[1],
                     num.threads = (parallel::detectCores() - 1),
                     parallel = T) %>% 
        magrittr::set_colnames(c("value", "yhat"))
      df_pdp_var$predictor <- var
      df_pdp_var$metric <- m
      df_pdp_var$region <- r
      class(df_pdp_var) <- "data.frame"
      
      if (v == 1){
        df_pdp <- df_pdp_var
      } else {
        df_pdp <- dplyr::bind_rows(df_pdp, df_pdp_var)
      }
    }
    
    # combine
    if (m == metrics[1] & r == regions[1]){
      fit_data_out <- fit_data_i
      fit_rf_imp <- fit_rf_imp_i
      fit_pdp_out <- df_pdp
    } else {
      fit_data_out <- dplyr::bind_rows(fit_data_out, fit_data_i)
      fit_rf_imp <- dplyr::bind_rows(fit_rf_imp, fit_rf_imp_i)
      fit_pdp_out <- dplyr::bind_rows(fit_pdp_out, df_pdp)
    }
    
    # status update
    print(paste0(m, " ", r, " complete, ", Sys.time()))
    
  }
}

# save data
fit_data_out %>% 
  dplyr::select(gauge_id, hydro_year, observed, predicted, region_rf, metric) %>% 
  readr::write_csv(file.path("results/Mine2", "04_RandomForest_RunModels_Predictions.csv"))

fit_rf_imp %>% 
  readr::write_csv(file.path("results/Mine2", "04_RandomForest_RunModels_VariableImportance.csv"))

fit_pdp_out %>% 
  readr::write_csv(file.path("results/Mine2", "04_RandomForest_RunModels_PartialDependence.csv"))


## color palettes
# categorical color palette from https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/
col.cat.grn <- "#3cb44b"   # green
col.cat.yel <- "#ffe119"   # yellow
col.cat.org <- "#f58231"   # orange
col.cat.red <- "#e6194b"   # red
col.cat.blu <- "#0082c8"   # blue
col.gray <- "gray65"       # gray for annotation lines, etc

pal_regions <- 
  c("Humid" = "#009E73",
    "Dry_sub_humid" = "#F0E442",
    "Semi_arid" = "#0072B2"
  )


# plots
min(subset(fit_data_out, metric == "Zero_Flow_Days_sum")$predicted)

ggplot(subset(fit_data_out, metric == "Zero_Flow_Days_sum" & Sample == "Test"), 
       aes(x = predicted, y = observed, color = region)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  scale_color_manual(values = pal_regions) +
  facet_wrap(~region_rf)

ggplot(subset(fit_data_out, metric == "Zero_Flow_Days_sum"), 
       aes(x = predicted, y = observed, color = region)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  scale_color_manual(values = pal_regions) +
  facet_wrap(~region_rf)

ggplot(subset(fit_data_out, metric == "Zero_Flow_Days_sum" & Sample == "Test"), 
       aes(x = hydro_year, y = (predicted - observed), color = region)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_point() +
  scale_color_manual(values = pal_regions) +
  facet_wrap(~region_rf)
