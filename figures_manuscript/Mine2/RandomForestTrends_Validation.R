# ========================================================
# UPDATED: RandomForestTrends_Validation.R
# ========================================================

source(file.path("code", "paths+packages.R"))
library(tidyverse)
library(hydroGOF) # Required for KGE, NRMSE, etc.

# 1. ADD MISSING R2 FUNCTION & COLORS
R2 <- function(sim, obs) {
  if (length(sim) != length(obs)) stop("vectors not the same size")
  return((sum((obs-mean(obs, na.rm=T))*(sim-mean(sim, na.rm=T)), na.rm=T)/
            ((sum((obs-mean(obs, na.rm=T))^2, na.rm=T)^0.5)*(sum((sim-mean(sim, na.rm=T))^2, na.rm=T)^0.5)))^2)
}

col.gray <- "gray50"

# ========================================================
# 2. LOAD DATA
# ========================================================
data_dir <- "results/Mine"

# RF input data
fit_data_in <- readr::read_csv(file.path(data_dir, "RandomForestTrends_01_RFinputData.csv"), show_col_types = FALSE) %>%
  mutate(gauge_id = as.character(gauge_id))

# Random forest predictions
rf_all <- readr::read_csv(file.path(data_dir, "RandomForestTrends_03_RunModels_Predictions.csv"), show_col_types = FALSE) %>% 
  mutate(gauge_id = as.character(gauge_id)) %>%
  dplyr::mutate(residual = predicted - observed) %>% 
  dplyr::left_join(fit_data_in, by = "gauge_id") %>%
  filter(!is.na(predicted) & !is.na(observed)) # Safely drop missing data

# Generate Dynamic Color Palette
unique_regions <- na.omit(unique(rf_all$region))
pal_regions <- scales::hue_pal()(length(unique_regions))
names(pal_regions) <- unique_regions

# ========================================================
# 3. CALCULATE FIT STATISTICS
# ========================================================
rf_fit <- rf_all %>% 
  subset(Sample == "Test") %>% 
  dplyr::group_by(metric) %>% 
  dplyr::summarize(MAE = round(hydroGOF::mae(predicted, observed), 3),
                   Rsq = round(R2(predicted, observed), 2),
                   RMSE = round(hydroGOF::rmse(predicted, observed), 3),
                   NRMSE = hydroGOF::nrmse(predicted, observed, norm = "maxmin"),
                   KGE = round(hydroGOF::KGE(predicted, observed, method = "2012"), 3),
                   .groups = "drop")

# Rename metric labels for a cleaner CSV output
rf_fit$metric[rf_fit$metric == "tau_Zero_Flow_Days_sum"] <- "Tau, No-Flow Days"
rf_fit$metric[rf_fit$metric == "tau_mean_dry_down_days"] <- "Tau, Days from Peak to No-Flow"
rf_fit$metric[rf_fit$metric == "tau_hydro_doy"] <- "Tau, First No-Flow Day"

if(!dir.exists("figures_manuscript")) dir.create("figures_manuscript")

rf_fit %>% readr::write_csv(file.path("figures_manuscript", "RandomForestTrends_Validation-FitTable.csv"))
cat("Validation statistics saved to 'RandomForestTrends_Validation-FitTable.csv'!\n")


# ========================================================
# 4. SCATTERPLOT (Predicted vs Observed Trends)
# ========================================================

# Set factor order so Train is on the left and Test is on the right
rf_all$Sample <- factor(rf_all$Sample, levels = c("Train", "Test"))

# Create accurate labels for the facets (Fixed author bug here!)
labs_metrics <- c("tau_Zero_Flow_Days_sum" = "Kendall \u03c4,\nAnnual No-Flow Days",
                  "tau_mean_dry_down_days" = "Kendall \u03c4,\nDays from Peak to No-Flow",
                  "tau_hydro_doy" = "Kendall \u03c4,\nFirst No-Flow Day",
                  "Train" = "Train",
                  "Test" = "Test")

p_scatter <- ggplot(rf_all, aes(x = predicted, y = observed)) + 
  geom_hline(yintercept = 0, color = col.gray, linetype="dashed") +
  geom_vline(xintercept = 0, color = col.gray, linetype="dashed") +
  geom_point(shape = 16, aes(color = region), size=2, alpha=0.7) +
  geom_abline(intercept = 0, slope = 1, color = "black") + # 1:1 Perfect Prediction line
  facet_grid(metric ~ Sample, labeller = as_labeller(labs_metrics)) +
  scale_color_manual(name = "Region", values = pal_regions) +
  scale_x_continuous(name = "Predicted [Kendall \u03c4]") +
  scale_y_continuous(name = "Observed [Kendall \u03c4]") +
  labs(title = "Trend Prediction Accuracy: Train vs Test Set") +
  theme_bw() +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

# Save the plot
ggsave(file.path("figures_manuscript", "RandomForestTrends_Validation.png"),
       plot = p_scatter, width = 190, height = 180, units = "mm")

cat("Validation scatterplots saved to 'RandomForestTrends_Validation.png'!\n")