# ========================================================
# UPDATED: RandomForestTrends_02_TuneHyperparameters.R
# ========================================================

source(file.path("code", "paths+packages.R"))
library(tidymodels)

# 1. DEFINE METRICS AND CORES
metrics <- c("Zero_Flow_Days_sum", "mean_dry_down_days", "hydro_doy")
ncores <- 1 # Forced to 1 for stability

# ========================================================
# 2. LOAD DATA
# ========================================================
data_dir <- "results/Mine2"

# RF input data
fit_data_in <- readr::read_csv(file.path(data_dir, "RandomForestTrends_01_RFinputData.csv"), show_col_types = FALSE) %>%
  mutate(gauge_id = as.character(gauge_id))

# Variable importance
rf_var <- readr::read_csv(file.path(data_dir, "RandomForestTrends_02_PreliminaryVariableImportance.csv"), show_col_types = FALSE)

# Dynamically load the optimal number of predictors!
npred_final <- readr::read_csv(file.path(data_dir, "RandomForestTrends_02_NumPredictors.csv"), show_col_types = FALSE) %>% 
  dplyr::group_by(metric) %>% 
  dplyr::filter(OOBmse == min(OOBmse)) %>%
  dplyr::select(metric, npred = n) %>%
  dplyr::distinct()

# ========================================================
# 3. SET UP TUNING ENGINE
# ========================================================
rf_tune <- rand_forest(
  trees = tune(), 
  mtry = tune(), 
  min_n = tune()
) %>% 
  set_engine("ranger", num.threads = ncores) %>% 
  set_mode("regression")

n_folds <- 5 # Number of folds for cross-validation

tune_res_all <- data.frame()

# ========================================================
# 4. START TUNING LOOP
# ========================================================
for (m in metrics){
  
  tau_m <- paste0("tau_", m)
  cat(paste("\nStarting Hyperparameter Tuning for:", tau_m, "\n"))
  
  # Subset to training data
  fit_data_m <- fit_data_in %>% subset(Sample == "Train")
  
  # Rename target column to 'observed'
  names(fit_data_m)[names(fit_data_m) == tau_m] <- "observed"
  
  # Get dynamic optimal number of predictors
  n_pred <- npred_final$npred[npred_final$metric == tau_m][1]
  
  # Dynamically limit max `mtry` so it doesn't crash if n_pred is small!
  max_mtry <- min(10, n_pred)
  
  # Create grid of parameters for tuning
  rf_tune_grid <- grid_regular(
    trees(range = c(250, 1650)),
    mtry(range = c(1, max_mtry)),
    min_n(range = c(3, 25)),
    levels = 5 # <--- Reduced from 8 to 5 to save massive processing time!
  )
  
  # Extract top predictor variables
  rf_var_m_r <- rf_var %>% 
    subset(metric == tau_m) %>% 
    dplyr::slice_max(order_by = ImpCondPerm, n = n_pred)
  
  fit_data_r <- fit_data_m %>% 
    dplyr::select(gauge_id, observed, region, all_of(rf_var_m_r$predictor)) %>% 
    subset(complete.cases(.))
  
  # Set up folds
  set.seed(1)
  tune_folds <- vfold_cv(fit_data_r, v = n_folds, strata = region)
  
  # Set up recipe
  tune_recipe <- fit_data_r %>% 
    recipe(observed ~ .) %>%
    update_role(gauge_id, region, new_role = "ID") %>% 
    step_normalize(all_predictors(), -all_outcomes())
  
  # Build tuning workflow
  tune_wf <- workflow() %>% 
    add_model(rf_tune) %>% 
    add_recipe(tune_recipe)
  
  # Run tuning (This is the part that takes time!)
  cat(paste("Testing", nrow(rf_tune_grid), "model configurations...\n"))
  
  tune_res <- tune_wf %>% 
    tune_grid(
      resamples = tune_folds,
      grid = rf_tune_grid,
      metrics = metric_set(mae, rmse, rsq)
    )
  
  # Collect results
  tune_res_m <- tune_res %>% 
    collect_metrics() %>% 
    dplyr::mutate(metric = tau_m)
  
  tune_res_all <- dplyr::bind_rows(tune_res_all, tune_res_m)
  
  cat(paste(">>", tau_m, "tuning complete! [", Sys.time(), "]\n"))
}

# ========================================================
# 5. SAVE DATA & GENERATE EXPLORATORY PLOTS
# ========================================================
readr::write_csv(tune_res_all, file.path(data_dir, "RandomForestTrends_02_TuneHyperparameters.csv"))
cat("\nResults saved to 'RandomForestTrends_02_TuneHyperparameters.csv'!\n")

if(!dir.exists("figures_manuscript/Mine2")) dir.create("figures_manuscript/Mine2")

# Plot 1: MTRY
p_mtry <- ggplot(subset(tune_res_all, .metric == "mae"), aes(x = mtry, y = mean, color = as.factor(min_n))) +
  geom_point() + geom_line(aes(group=interaction(min_n, trees)), alpha=0.3) +
  facet_wrap(~metric, scales="free_y") +
  labs(title="Tuning: Mean Absolute Error vs MTRY", color="Min_N") + theme_bw()
ggsave(file.path("figures_manuscript/Mine2", "RandomForestTrends_Tune_MTRY.png"), plot = p_mtry, width = 200, height = 100, units = "mm")

# Plot 2: TREES
p_trees <- ggplot(subset(tune_res_all, .metric == "mae"), aes(x = trees, y = mean, color = as.factor(min_n))) +
  geom_point() + geom_line(aes(group=interaction(min_n, mtry)), alpha=0.3) +
  facet_wrap(~metric, scales="free_y") +
  labs(title="Tuning: Mean Absolute Error vs Number of Trees", color="Min_N") + theme_bw()
ggsave(file.path("figures_manuscript/Mine2", "RandomForestTrends_Tune_TREES.png"), plot = p_trees, width = 200, height = 100, units = "mm")

# Plot 3: MIN_N
p_min_n <- ggplot(subset(tune_res_all, .metric == "mae"), aes(x = min_n, y = mean, color = as.factor(trees))) +
  geom_point() + geom_line(aes(group=interaction(trees, mtry)), alpha=0.3) +
  facet_wrap(~metric, scales="free_y") +
  labs(title="Tuning: Mean Absolute Error vs Min_N", color="Trees") + theme_bw()
ggsave(file.path("figures_manuscript/Mine2", "RandomForestTrends_Tune_MIN_N.png"), plot = p_min_n, width = 200, height = 100, units = "mm")