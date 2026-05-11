# ========================================================
# UPDATED: RandomForest_Validation.R
# ========================================================

source(file.path("code", "paths+packages.R"))
library(tidyverse)
library(patchwork)
library(hydroGOF) # Required for hydrological error metrics!

# 1. ADD MISSING R2 FUNCTION & COLORS
R2 <- function(sim, obs) {
  if (length(sim) != length(obs)) stop("vectors not the same size")
  return((sum((obs-mean(obs, na.rm=T))*(sim-mean(sim, na.rm=T)), na.rm=T)/
            ((sum((obs-mean(obs, na.rm=T))^2, na.rm=T)^0.5)*(sum((sim-mean(sim, na.rm=T))^2, na.rm=T)^0.5)))^2)
}

col.gray <- "gray50"

# ========================================================
# LOAD DATA
# ========================================================
data_dir <- "results/Mine"

gage_sample_annual <- readr::read_csv(file = file.path(data_dir, "gauges_annual_summary_out.csv"), show_col_types = FALSE) %>% 
  dplyr::select(gauge_id, hydro_year, Sample) %>%
  mutate(gauge_id = as.character(gauge_id))

gage_regions <- readr::read_csv(file.path(data_dir, "station_ai_regions.csv"), show_col_types = FALSE) %>% 
  dplyr::select(gauge_id, region) %>%
  mutate(gauge_id = as.character(gauge_id))

rf_all <- readr::read_csv(file.path("results/Mine2", "04_RandomForest_RunModels_Predictions.csv"), show_col_types = FALSE) %>% 
  mutate(gauge_id = as.character(gauge_id)) %>%
  dplyr::mutate(residual = predicted - observed) %>% 
  dplyr::left_join(gage_sample_annual, by = c("gauge_id", "hydro_year")) %>% 
  dplyr::left_join(gage_regions, by = "gauge_id") %>%
  # Drop NAs safely
  filter(!is.na(predicted) & !is.na(observed))

# Group everything that isn't "National" into a "Regional" bucket for comparison
rf_all$region_rf[rf_all$region_rf != "National"] <- "Regional"

# 2. CREATE DYNAMIC COLOR PALETTE FOR REGIONS
unique_regions <- na.omit(unique(rf_all$region))
pal_regions <- scales::hue_pal()(length(unique_regions))
names(pal_regions) <- unique_regions

if(!dir.exists("figures_manuscript")) dir.create("figures_manuscript")

# ========================================================
# CALCULATE FIT STATISTICS (TEST & TRAIN)
# ========================================================

# -- TEST DATA --
rf_fit_regional <- rf_all %>% subset(Sample == "Test") %>% 
  dplyr::group_by(metric, region_rf, region) %>% 
  dplyr::summarize(MAE = round(hydroGOF::mae(predicted, observed), 3),
                   Rsq = round(R2(predicted, observed), 2),
                   RMSE = round(hydroGOF::rmse(predicted, observed), 3),
                   NRMSE = hydroGOF::nrmse(predicted, observed, norm = "maxmin"),
                   KGE = round(hydroGOF::KGE(predicted, observed, method = "2012"), 3), .groups="drop")

rf_fit_national <- rf_all %>% subset(Sample == "Test") %>% 
  dplyr::group_by(metric, region_rf) %>% 
  dplyr::summarize(MAE = round(hydroGOF::mae(predicted, observed), 3),
                   Rsq = round(R2(predicted, observed), 2),
                   RMSE = round(hydroGOF::rmse(predicted, observed), 3),
                   NRMSE = hydroGOF::nrmse(predicted, observed, norm = "maxmin"),
                   KGE = round(hydroGOF::KGE(predicted, observed, method = "2012"), 3), .groups="drop")

rf_fit <- dplyr::bind_rows(rf_fit_regional, rf_fit_national) %>% dplyr::arrange(metric, region_rf, region)

# Clean up metric names for the final CSV
rf_fit$metric[rf_fit$metric == "Zero_Flow_Days_sum"] <- "No-Flow Days"
rf_fit$metric[rf_fit$metric == "mean_dry_down_days"] <- "Days from Peak to No-Flow"
rf_fit$metric[rf_fit$metric == "hydro_doy"] <- "First No-Flow Day"

rf_fit %>% readr::write_csv(file.path("figures_manuscript/Mine2", "RandomForest_Validation-FitTable-Test.csv"))

# -- TRAINING DATA --
rf_fit_regional_train <- rf_all %>% subset(Sample == "Train") %>% 
  dplyr::group_by(metric, region_rf, region) %>% 
  dplyr::summarize(MAE = round(hydroGOF::mae(predicted, observed), 3),
                   Rsq = round(R2(predicted, observed), 2),
                   RMSE = round(hydroGOF::rmse(predicted, observed), 3),
                   NRMSE = hydroGOF::nrmse(predicted, observed, norm = "maxmin"),
                   KGE = round(hydroGOF::KGE(predicted, observed, method = "2012"), 3), .groups="drop")

rf_fit_national_train <- rf_all %>% subset(Sample == "Train") %>% 
  dplyr::group_by(metric, region_rf) %>% 
  dplyr::summarize(MAE = round(hydroGOF::mae(predicted, observed), 3),
                   Rsq = round(R2(predicted, observed), 2),
                   RMSE = round(hydroGOF::rmse(predicted, observed), 3),
                   NRMSE = hydroGOF::nrmse(predicted, observed, norm = "maxmin"),
                   KGE = round(hydroGOF::KGE(predicted, observed, method = "2012"), 3), .groups="drop")

rf_fit_train <- dplyr::bind_rows(rf_fit_regional_train, rf_fit_national_train) %>% dplyr::arrange(metric, region_rf, region)

rf_fit_train$metric[rf_fit_train$metric == "Zero_Flow_Days_sum"] <- "No-Flow Days"
rf_fit_train$metric[rf_fit_train$metric == "mean_dry_down_days"] <- "Days from Peak to No-Flow"
rf_fit_train$metric[rf_fit_train$metric == "hydro_doy"] <- "First No-Flow Day"

rf_fit_train %>% readr::write_csv(file.path("figures_manuscript/Mine2", "RandomForest_Validation-FitTable-Train.csv"))

# ========================================================
# PLOT 1: MODEL ERROR (NATIONAL VS REGIONAL)
# ========================================================
rf_fit_wide <- rf_fit %>% 
  dplyr::select(metric, region_rf, region, Rsq) %>% 
  tidyr::pivot_wider(id_cols = c("metric", "region"), names_from = "region_rf", values_from = "Rsq")

p_compare <- ggplot(subset(rf_fit_wide, !is.na(region)), aes(x = National, y = Regional)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray, linetype="dashed") +
  geom_point(aes(color = region), size = 2) +
  geom_point(data = subset(rf_fit_wide, is.na(region)), aes(x = National, y = Regional), color = "black", size = 3, shape = 18) +
  facet_wrap(~metric, scales = "free") +
  scale_x_continuous(name = "National Model R²", limits = c(0,1)) +
  scale_y_continuous(name = "Regional Model R²", limits = c(0,1)) +
  scale_color_manual(name = "Region", values = pal_regions) +
  labs(title = "Validation R²: Regional vs National Models", subtitle = "Points above the line mean the Regional model performed better!") +
  theme_bw() + theme(legend.position = "bottom")

# Run ggsave on its own line without the '+'
ggsave(file.path("figures_manuscript/Mine2", "RandomForest_Validation-CompareNationalRegionFit.png"), 
       plot = p_compare, width = 190, height = 100, units = "mm")


# ========================================================
# HELPER PLOTTING FUNCTION FOR SCATTERPLOTS
# ========================================================
create_scatter <- function(data, metric_name, sample_type, model_type, title, max_val) {
  data %>% 
    subset(region_rf == model_type & metric == metric_name & Sample == sample_type) %>% 
    ggplot(aes(x = predicted, y = observed)) + 
    geom_point(shape = 1, aes(color = region), alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = col.gray) +
    scale_color_manual(name = "Region", values = pal_regions) +
    scale_x_continuous(name = "Predicted", limits = c(0, max_val)) +
    scale_y_continuous(name = "Observed", limits = c(0, max_val)) +
    labs(title = title) +
    theme_bw()
}

# ========================================================
# PLOT 2: SCATTERPLOTS (NATIONAL MODELS)
# ========================================================
p_nat_afnf_train <- create_scatter(rf_all, "Zero_Flow_Days_sum", "Train", "National", "(a) No-Flow Days (Train)", 366)
p_nat_p2z_train  <- create_scatter(rf_all, "mean_dry_down_days", "Train", "National", "(b) Peak to No-Flow (Train)", 366)
p_nat_zff_train  <- create_scatter(rf_all, "hydro_doy", "Train", "National", "(c) First No-Flow (Train)", 366)

p_nat_afnf_test  <- create_scatter(rf_all, "Zero_Flow_Days_sum", "Test", "National", "(d) No-Flow Days (Test)", 366)
p_nat_p2z_test   <- create_scatter(rf_all, "mean_dry_down_days", "Test", "National", "(e) Peak to No-Flow (Test)", 366)
p_nat_zff_test   <- create_scatter(rf_all, "hydro_doy", "Test", "National", "(f) First No-Flow (Test)", 366)

((p_nat_afnf_train + p_nat_p2z_train + p_nat_zff_train) / 
    (p_nat_afnf_test + p_nat_p2z_test + p_nat_zff_test)) + 
  plot_layout(guides = 'collect') & theme(legend.position = "bottom")

ggsave(file.path("figures_manuscript/Mine2", "RandomForest_Validation-National-Train+Test.png"), width = 250, height = 200, units = "mm")

# ========================================================
# PLOT 3: SCATTERPLOTS (REGIONAL MODELS)
# ========================================================
p_reg_afnf_train <- create_scatter(rf_all, "Zero_Flow_Days_sum", "Train", "Regional", "(a) No-Flow Days (Train)", 366)
p_reg_p2z_train  <- create_scatter(rf_all, "mean_dry_down_days", "Train", "Regional", "(b) Peak to No-Flow (Train)", 366)
p_reg_zff_train  <- create_scatter(rf_all, "hydro_doy", "Train", "Regional", "(c) First No-Flow (Train)", 366)

p_reg_afnf_test  <- create_scatter(rf_all, "Zero_Flow_Days_sum", "Test", "Regional", "(d) No-Flow Days (Test)", 366)
p_reg_p2z_test   <- create_scatter(rf_all, "mean_dry_down_days", "Test", "Regional", "(e) Peak to No-Flow (Test)", 366)
p_reg_zff_test   <- create_scatter(rf_all, "hydro_doy", "Test", "Regional", "(f) First No-Flow (Test)", 366)

((p_reg_afnf_train + p_reg_p2z_train + p_reg_zff_train) / 
    (p_reg_afnf_test + p_reg_p2z_test + p_reg_zff_test)) + 
  plot_layout(guides = 'collect') & theme(legend.position = "bottom")

ggsave(file.path("figures_manuscript/Mine2", "RandomForest_Validation-Regional-Train+Test.png"), width = 250, height = 200, units = "mm")

print("Validation tables and plots generated successfully!")