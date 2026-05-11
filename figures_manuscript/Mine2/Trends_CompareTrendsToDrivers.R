# ========================================================
# UPDATED: Trends_CompareTrendsToDrivers.R
# ========================================================
# NOTE: CHANGE the driver in the beginning. Variable
# Importance results could be used
# ========================================================
# NOTE: Run for different metrics as well
# ========================================================


source(file.path("code", "paths+packages.R"))
library(tidyverse)
library(patchwork)

# 1. DEFINE VARIABLES & COLORS
metrics <- c("Zero_Flow_Days_sum", "mean_dry_down_days", "hydro_doy")
main_driver <- "p" # Change this to "pet", "tmax", etc. if you prefer!

col.gray <- "gray50"
data_dir <- "results/Mine"

# 2. LOAD DATA & BUILD THE MASTER TREND DATASET
# Load static/physiographic data (and safely drop any duplicate region columns)
gage_mean <- readr::read_csv(file.path(data_dir, "Physiographic_data.csv"), show_col_types = FALSE) %>%
  mutate(gauge_id = as.character(gauge_id)) %>%
  dplyr::select(-any_of("region")) # Prevents region.x / region.y conflicts!

# Load regions explicitly
gage_regions <- readr::read_csv(file.path(data_dir, "station_ai_regions.csv"), show_col_types = FALSE) %>% 
  dplyr::select(gauge_id, region) %>%
  mutate(gauge_id = as.character(gauge_id))

# Load trends, pivot, and safely drop any duplicate region columns
gage_trends <- readr::read_csv(file.path(data_dir, "gauge_trends.csv"), show_col_types = FALSE) %>% 
  mutate(gauge_id = as.character(gauge_id)) %>%
  dplyr::select(-any_of("region")) %>% # Prevents conflicts!
  dplyr::select(gauge_id, metric, mk_tau) %>%
  pivot_wider(names_from = metric, values_from = mk_tau, names_prefix = "tau_")

# Join everything together to create the missing 'fit_data_in'
fit_data_in <- gage_trends %>%
  dplyr::left_join(gage_mean, by = "gauge_id") %>%
  dplyr::left_join(gage_regions, by = "gauge_id") %>%
  filter(!is.na(region))

# Generate Dynamic Color Palettes
unique_regions <- na.omit(unique(fit_data_in$region))
pal_regions <- scales::hue_pal()(length(unique_regions))
names(pal_regions) <- unique_regions

# Define exactly which driver we are looking at dynamically
driver_col <- paste0("tau_", main_driver)

if(!dir.exists("figures_manuscript")) dir.create("figures_manuscript")

# ========================================================
# PLOT 1: NO-FLOW METRICS VS CLIMATE DRIVER
# ========================================================

# Figure out tau limits for the Y axis
tau_min <- min(fit_data_in %>% dplyr::select(paste0("tau_", metrics)), na.rm=T)
tau_max <- max(fit_data_in %>% dplyr::select(paste0("tau_", metrics)), na.rm=T)
tau_abs <- max(abs(c(tau_min, tau_max)))

# Helper function to create the scatter plots
create_trend_scatter <- function(y_var, y_label) {
  ggplot(fit_data_in, aes_string(x = driver_col, y = y_var)) +
    geom_hline(yintercept = 0, color = col.gray) + 
    geom_vline(xintercept = 0, color = col.gray) +
    geom_point(aes(color = region), shape = 16, size=2, alpha=0.8) +
    scale_x_continuous(name = paste(toupper(main_driver), "Trend (Kendall \u03c4)"), expand = c(0, 0.01)) +
    scale_y_continuous(name = y_label, limits = c(-tau_abs, tau_abs), expand = c(0, 0.01)) +
    scale_color_manual(name = "Region", values = pal_regions) +
    stat_smooth(method = "lm", color = "black", se = FALSE) +
    theme_bw() +
    theme(legend.position = "bottom")
}

p_anfd <- create_trend_scatter("tau_Zero_Flow_Days_sum", "Annual No-Flow Days\n(Kendall \u03c4)")
p_p2z  <- create_trend_scatter("tau_mean_dry_down_days", "Peak to No-Flow\n(Kendall \u03c4)")
p_zff  <- create_trend_scatter("tau_hydro_doy", "First No-Flow Day\n(Kendall \u03c4)")

# combine using patchwork
final_driver_plot <- (
  (p_anfd + theme(axis.title.x = element_blank())) / 
    (p_p2z + theme(axis.title.x = element_blank())) / 
    (p_zff)
) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave(file.path("figures_manuscript/Mine2", "Trends_CompareTrendsToDrivers-MetricsVsClimate.png"),
       plot = final_driver_plot, width = 120, height = 210, units = "mm")

# ========================================================
# STATS: CORRELATION BETWEEN TRENDS
# ========================================================

cat("\n--- CORRELATIONS: INTERMITTENCY TRENDS vs", toupper(main_driver), "TRENDS ---\n")
cat("No-Flow Days ~", main_driver, ": ", round(cor(x = fit_data_in[[driver_col]], y = fit_data_in$tau_Zero_Flow_Days_sum, use = "complete.obs"), 3), "\n")
cat("Peak to Zero ~", main_driver, ": ", round(cor(x = fit_data_in[[driver_col]], y = fit_data_in$tau_mean_dry_down_days, use = "complete.obs"), 3), "\n")
cat("First No-Flow ~", main_driver, ": ", round(cor(x = fit_data_in[[driver_col]], y = fit_data_in$tau_hydro_doy, use = "complete.obs"), 3), "\n\n")

# ========================================================
# RESIDUAL ANALYSIS
# ========================================================
# Question: If we remove the effect of Precipitation, what explains the remaining trend?

cat("--- RESIDUAL ANALYSIS ---\n")

# Calculate the residual of (No-Flow Trend ~ Precip Trend)
fit_anf.driver <- lm(as.formula(paste("tau_Zero_Flow_Days_sum ~", driver_col)), data = fit_data_in)
fit_data_in$tau_anf_resid <- NA
fit_data_in$tau_anf_resid[complete.cases(fit_data_in[, c("tau_Zero_Flow_Days_sum", driver_col)])] <- fit_anf.driver$residual

# Grab all numeric static variables to test against the residual
static_vars <- gage_mean %>% dplyr::select(where(is.numeric), -gauge_id) %>% names()

# Rank static variables by how strongly they correlate with the leftover residual
df_cor_resid <- fit_data_in %>% 
  dplyr::select(tau_anf_resid, all_of(static_vars)) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  as.data.frame() %>% 
  dplyr::mutate(metric = rownames(.)) %>% 
  dplyr::select(metric, tau_anf_resid) %>% 
  subset(metric != "tau_anf_resid") %>% 
  dplyr::rename(pearson_r = tau_anf_resid) %>% 
  dplyr::arrange(-abs(pearson_r))

cat("Top 5 Static Variables that explain the remaining No-Flow trend (after removing", main_driver, "):\n")
print(head(df_cor_resid, 5))

# Plot the No-Flow vs Driver with Residuals
p_resid <- ggplot(fit_data_in, aes_string(x = "tau_Zero_Flow_Days_sum", y = "tau_anf_resid")) +
  geom_hline(yintercept = 0, color = col.gray) + 
  geom_vline(xintercept = 0, color = col.gray) +
  geom_point(aes(color = region), size=2, alpha=0.8) +
  scale_y_continuous(name = paste("Residual of\n(\u03c4 No-Flow Days ~ \u03c4", main_driver, ")"), expand = c(0, 0.01)) +
  scale_x_continuous(name = "Annual No-Flow Days (Kendall \u03c4)", expand = c(0, 0.01)) +
  scale_color_manual(name = "Region", values = pal_regions) +
  stat_smooth(method = "lm", color = "black", se=FALSE) +
  theme_bw() + theme(legend.position = "bottom")

ggsave(file.path("figures_manuscript/Mine2", "Trends_CompareTrendsToDrivers-NoFlowResidVsNoFlowTrend.png"),
       plot = p_resid, width = 120, height = 110, units = "mm")

# ========================================================
# MULTIPLE LINEAR REGRESSION (MLR)
# ========================================================
# Let's see how much variance is explained by Precip + Top 2 Static Variables

top_static_1 <- df_cor_resid$metric[1]
top_static_2 <- df_cor_resid$metric[2]

cat("\n--- MULTIPLE LINEAR REGRESSION ---\n")
cat("Predicting No-Flow Trends using:", main_driver, "+", top_static_1, "+", top_static_2, "\n")

formula_mlr <- as.formula(paste("tau_Zero_Flow_Days_sum ~", driver_col, "+", top_static_1, "+", top_static_2))
fit_mlr <- lm(formula_mlr, data = fit_data_in)
print(summary(fit_mlr))



# ========================================================
# AUTOMATED REGION-WISE "BEST DRIVER" FINDER
# ========================================================

cat("\n=======================================================\n")
cat(" FINDING THE #1 TREND DRIVER FOR EACH REGION\n")
cat("=======================================================\n")

# 1. Identify all target metrics and candidate drivers
target_metrics <- paste0("tau_", metrics) # e.g., "tau_Zero_Flow_Days_sum"

# Find all columns that start with "tau_" (these are all your trends!)
all_tau_cols <- grep("^tau_", names(fit_data_in), value = TRUE)

# The candidates are all the trend columns EXCEPT the intermittency metrics themselves
candidate_drivers <- setdiff(all_tau_cols, target_metrics)

# 2. Create an empty list to store the results
best_drivers_list <- list()

# 3. Loop through every Region
for (r in unique_regions) {
  
  # Filter data for just this region
  df_region <- subset(fit_data_in, region == r)
  
  # Loop through each of our 3 target metrics (No-Flow Days, Dry-Down, etc.)
  for (m in target_metrics) {
    
    best_driver_name <- NA
    best_cor_value <- 0 
    
    # Loop through every single candidate driver (p, tmax, pet, etc.)
    for (d in candidate_drivers) {
      
      # Make sure we have enough valid points to do math (at least 5 stations)
      if (sum(complete.cases(df_region[[m]], df_region[[d]])) > 5) {
        
        # Calculate Pearson correlation
        current_cor <- cor(x = df_region[[d]], y = df_region[[m]], use = "pairwise.complete.obs")
        
        # If this correlation is the STRONGEST we've seen so far (absolute value), save it!
        if (!is.na(current_cor) && abs(current_cor) > abs(best_cor_value)) {
          best_cor_value <- current_cor
          best_driver_name <- d
        }
      }
    }
    
    # Clean up the names for the final table (remove the "tau_" prefix)
    clean_metric <- gsub("^tau_", "", m)
    clean_driver <- gsub("^tau_", "", best_driver_name)
    
    # Save the winner for this region/metric combination!
    best_drivers_list[[length(best_drivers_list) + 1]] <- data.frame(
      Region = r,
      Metric = clean_metric,
      Number_1_Driver = clean_driver,
      Correlation_R = round(best_cor_value, 3)
    )
  }
}

# 4. Combine everything into a beautiful summary table
best_drivers_df <- do.call(rbind, best_drivers_list)

# Print the final table to the console!
print(best_drivers_df)

# Optionally, save this table so you can put it straight into your manuscript!
write_csv(best_drivers_df, file.path("results/Mine2", "Best_Trend_Drivers_By_Region.csv"))
cat("\nResults saved to 'results/Mine2/Best_Trend_Drivers_By_Region.csv'!\n")