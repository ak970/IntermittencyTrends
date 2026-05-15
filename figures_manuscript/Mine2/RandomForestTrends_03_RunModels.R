# ========================================================
# UPDATED: RandomForestTrends_03_RunModels.R
# ========================================================

source(file.path("code", "paths+packages.R"))
library(tidymodels)

# 1. DEFINE METRICS AND CORES
metrics <- c("Zero_Flow_Days_sum", "mean_dry_down_days", "hydro_doy")
ncores <- 1 # Forced to 1 for mathematical stability

# ========================================================
# 2. LOAD DATA
# ========================================================
data_dir <- "results/Mine2"

# RF input data
fit_data_in <- readr::read_csv(file.path(data_dir, "RandomForestTrends_01_RFinputData.csv"), show_col_types = FALSE) %>%
  mutate(gauge_id = as.character(gauge_id))

# Variable importance
rf_var <- readr::read_csv(file.path(data_dir, "RandomForestTrends_02_PreliminaryVariableImportance.csv"), show_col_types = FALSE)

# Hyperparameter tuning results
tune_res_all <- readr::read_csv(file.path(data_dir, "RandomForestTrends_02_TuneHyperparameters.csv"), show_col_types = FALSE)

# Dynamically load optimal number of predictors
npred_final <- readr::read_csv(file.path(data_dir, "RandomForestTrends_02_NumPredictors.csv"), show_col_types = FALSE) %>% 
  dplyr::group_by(metric) %>% 
  dplyr::filter(OOBmse == min(OOBmse)) %>%
  dplyr::select(metric, npred = n) %>%
  dplyr::distinct()

# ========================================================
# 3. START FINAL MODEL LOOP
# ========================================================
for (m in metrics){
  
  # Name for trend
  tau_m <- paste0("tau_", m)
  cat(paste("\nBuilding Final Model for:", tau_m, "...\n"))
  
  # Determine optimal number of predictors
  n_pred <- npred_final$npred[npred_final$metric == tau_m][1]
  
  # Get top predictor variables
  rf_var_m_r <- rf_var %>% 
    subset(metric == tau_m) %>% 
    dplyr::slice_max(order_by = ImpCondPerm, n = n_pred)
  
  # Get data for this metric
  fit_data_r <- fit_data_in %>% 
    dplyr::select(gauge_id, region, Sample, all_of(tau_m), all_of(rf_var_m_r$predictor)) %>% 
    subset(complete.cases(.))
  
  # Rename metric column to 'observed'
  names(fit_data_r)[names(fit_data_r) == tau_m] <- "observed"
  
  # Split into training/testing
  fit_data_train <- subset(fit_data_r, Sample == "Train")
  fit_data_test <- subset(fit_data_r, Sample == "Test")
  
  # Fetch best hyperparameters for this specific metric
  tune_res <- tune_res_all %>% 
    subset(metric == tau_m & .metric == "mae") %>%  # FIXED: Matches tau_m perfectly now!
    dplyr::filter(mean == min(mean))
  
  cat(paste("Using Hyperparameters -> Trees:", tune_res$trees[1], "| MTRY:", tune_res$mtry[1], "| Min_N:", tune_res$min_n[1], "\n"))
  
  # Set up model engine
  set.seed(1)
  rf_engine <- rand_forest(
    trees = tune_res$trees[1], 
    mtry = tune_res$mtry[1], 
    min_n = tune_res$min_n[1]
  ) %>% 
    set_engine("ranger", num.threads = ncores, importance = "permutation") %>% 
    set_mode("regression")
  
  # Set up recipe
  rf_recipe <- fit_data_train %>% 
    recipe(observed ~ .) %>%
    update_role(gauge_id, region, Sample, new_role = "ID") %>% 
    step_normalize(all_predictors(), -all_outcomes())
  
  # Set up workflow and FIT
  rf_workflow <- workflow() %>% add_model(rf_engine) %>% add_recipe(rf_recipe)
  rf_fit <- rf_workflow %>% parsnip::fit(data = fit_data_train)
  
  # Predict on both sets
  fit_data_train$predicted <- predict(rf_fit, fit_data_train)$.pred
  fit_data_test$predicted <-  predict(rf_fit, fit_data_test)$.pred
  
  # Combine training and test output
  fit_data_i <- dplyr::bind_rows(fit_data_train, fit_data_test) %>% 
    dplyr::select(gauge_id, observed, predicted) %>% 
    dplyr::mutate(metric = m) %>% 
    dplyr::left_join(dplyr::select(fit_data_in, gauge_id, region, Sample, all_of(rf_var_m_r$predictor)), 
                     by = "gauge_id")
  
  # Extract true Variable Importance
  fit_rf_imp_i <- tibble::tibble(
    predictor = names(extract_fit_engine(rf_fit)$variable.importance),
    IncMSE = extract_fit_engine(rf_fit)$variable.importance,
    oobMSE = extract_fit_engine(rf_fit)$prediction.error,
    metric = m
  )
  
  # ========================================================
  # 4. PARTIAL DEPENDENCE PLOTS (PDPs)
  # ========================================================
  cat("Calculating Partial Dependence (PDPs)...\n")
  
  # Re-fit raw ranger model for PDP extraction
  ranger_fit <- ranger::ranger(
    observed ~ ., 
    data = dplyr::bind_cols(rf_fit$pre$mold$outcomes, rf_fit$pre$mold$predictors),
    num.trees = tune_res$trees[1],
    mtry = tune_res$mtry[1], 
    min.node.size = tune_res$min_n[1],
    num.threads = ncores
  )
  
  for (v in 1:n_pred){
    var <- rf_var_m_r$predictor[v]
    
    df_pdp_var <- pdp::partial(
      ranger_fit, pred.var = var,
      mtry = tune_res$mtry[1], min.node.size = tune_res$min_n[1],
      num.threads = ncores, parallel = FALSE # Forced to False for stability
    ) %>% magrittr::set_colnames(c("value", "yhat"))
    
    df_pdp_var$predictor <- var
    df_pdp_var$metric <- m
    class(df_pdp_var) <- "data.frame"
    
    if (v == 1){ df_pdp <- df_pdp_var } else { df_pdp <- dplyr::bind_rows(df_pdp, df_pdp_var) }
  }
  
  # Combine everything
  if (m == metrics[1]){
    fit_data_out <- fit_data_i
    fit_rf_imp <- fit_rf_imp_i
    fit_pdp_out <- df_pdp
  } else {
    fit_data_out <- dplyr::bind_rows(fit_data_out, fit_data_i)
    fit_rf_imp <- dplyr::bind_rows(fit_rf_imp, fit_rf_imp_i)
    fit_pdp_out <- dplyr::bind_rows(fit_pdp_out, df_pdp)
  }
  
  cat(paste(">>", m, "Final Model Complete! [", Sys.time(), "]\n"))
}

# ========================================================
# 5. SAVE DATA
# ========================================================
readr::write_csv(fit_data_out %>% dplyr::select(gauge_id, observed, predicted, metric), 
                 file.path(data_dir, "RandomForestTrends_03_RunModels_Predictions.csv"))

readr::write_csv(fit_rf_imp, file.path(data_dir, "RandomForestTrends_03_RunModels_VariableImportance.csv"))

readr::write_csv(fit_pdp_out, file.path(data_dir, "RandomForestTrends_03_RunModels_PartialDependence.csv"))

cat("\nAll Trend Models Completed and Saved Successfully!\n")