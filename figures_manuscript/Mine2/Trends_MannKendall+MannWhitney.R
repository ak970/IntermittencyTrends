# ========================================================
# UPDATED: Trends_MannKendall+MannWhitney.R
# ========================================================
# install.packages("maps") # For the background India map

source(file.path("code", "paths+packages.R"))
library(tidyverse)
library(patchwork)
library(maps) # Needed for the background India map

# 1. DEFINE COLORS & NAMES
col.cat.red <- "#e41a1c"
col.cat.blu <- "#377eb8"
col.gray <- "gray50"

# YOUR SPECIFIC COLUMN NAMES FOR LATITUDE AND LONGITUDE
col_lat <- "cwc_lat" # Change to "latitude" if needed!
col_lon <- "cwc_lon" # Change to "longitude" if needed!

metrics <- c("Zero_Flow_Days_sum", "hydro_doy", "mean_dry_down_days")

# 2. LOAD DATA
gage_mean <- readr::read_csv(file.path("results/Mine", "Physiographic_data.csv"), show_col_types = FALSE) %>%
  mutate(gauge_id = as.character(gauge_id)) %>%
  # Rename them so the script can easily find them
  rename(dec_lat_va = all_of(col_lat), dec_long_va = all_of(col_lon))

gage_regions <- readr::read_csv(file.path("results/Mine", "station_ai_regions.csv"), show_col_types = FALSE) %>% 
  dplyr::select(gauge_id, region) %>%
  mutate(gauge_id = as.character(gauge_id))

gage_trends <- readr::read_csv(file.path("results/Mine", "gauge_trends.csv"), show_col_types = FALSE) %>% 
  mutate(gauge_id = as.character(gauge_id)) %>%
  subset(metric %in% metrics) %>% 
  dplyr::left_join(gage_mean[,c("gauge_id", "dec_lat_va", "dec_long_va")], by = "gauge_id") %>%
  dplyr::left_join(gage_regions, by = "gauge_id")

# Load India Map!
india_map <- map_data("world", region = "India")

# p-value threshold for significance
p_thres <- 0.05

if(!dir.exists("figures_manuscript")) dir.create("figures_manuscript")

# ========================================================
# MANN-KENDALL TREND MAPS
# ========================================================

df_mk <- gage_trends %>% 
  dplyr::select(metric, gauge_id, region, dec_lat_va, dec_long_va, mk_tau, mk_p) %>% 
  subset(complete.cases(.))

tau_min <- min(df_mk$mk_tau, na.rm = TRUE)
tau_max <- max(df_mk$mk_tau, na.rm = TRUE)

# Helper function to draw the maps
draw_map <- function(data, metric_name, title, invert_colors = FALSE) {
  
  if(invert_colors) {
    grad_colors <- scale_color_gradient2(name = "Kendall \u03c4", limits = c(tau_min, tau_max),
                                         high = "#4575b4", mid = "#ffffbf", low = "#d73027")
  } else {
    grad_colors <- scale_color_gradient2(name = "Kendall \u03c4", limits = c(tau_min, tau_max),
                                         low = "#4575b4", mid = "#ffffbf", high = "#d73027")
  }
  
  ggplot() +
    geom_polygon(data = india_map, aes(x = long, y = lat, group = group), fill = "gray85", color = "white") +
    geom_point(data = subset(data, metric == metric_name),
               aes(x = dec_long_va, y = dec_lat_va, color = mk_tau, shape = mk_p < p_thres), size = 2) +
    # Coordinates adjusted roughly for Peninsular India
    scale_x_continuous(name = NULL, breaks = seq(70, 90, 10), labels = c("70\u00b0E", "80\u00b0E", "90\u00b0E")) +
    scale_y_continuous(name = NULL, breaks = seq(10, 30, 10), labels = c("10\u00b0N", "20\u00b0N", "30\u00b0N")) +
    coord_quickmap(xlim = c(68, 90), ylim = c(8, 28)) + # Zooms in on Peninsular India
    grad_colors +
    scale_shape_manual(name = NULL, values = c("TRUE" = 16, "FALSE" = 1), labels = c("TRUE" = "p < 0.05", "FALSE" = "p > 0.05"))  +
    labs(title = title) +
    theme_minimal() +
    theme(panel.border = element_blank(), strip.text = element_text(face = "bold"),
          axis.text.y = element_text(angle = 90, hjust = 0.5), legend.position = "right") +
    guides(color = guide_colorbar(order = 1, title.position = "top", title.hjust = 0.5), shape = "none")
}

p_afd <- draw_map(df_mk, "Zero_Flow_Days_sum", "(a) Annual No-Flow Days", FALSE)
p_p2z <- draw_map(df_mk, "mean_dry_down_days", "(b) Days from Peak to No-Flow", TRUE)
p_zff <- draw_map(df_mk, "hydro_doy", "(c) First No-Flow Day", TRUE)

p_combo <- (p_afd + p_p2z + p_zff) + plot_layout(ncol = 1) & theme(plot.title = element_text(face = "plain"))

ggsave(file.path("figures_manuscript/Mine2", "Trends_MannKendall-Maps.png"), plot = p_combo, width = 190, height = 250, units = "mm")

# ========================================================
# COMPARISON AMONG TRENDS (SCATTER PLOTS)
# ========================================================

df_mk_wide <- df_mk %>% 
  dplyr::select(metric, gauge_id, mk_tau, mk_p) %>% 
  pivot_wider(names_from = metric, values_from = c(mk_tau, mk_p)) %>% 
  dplyr::left_join(gage_regions, by = "gauge_id")

p_mk_anf.p2z <- ggplot(df_mk_wide, aes(x = mk_tau_Zero_Flow_Days_sum, y = mk_tau_mean_dry_down_days)) +
  geom_hline(yintercept = 0, color = col.gray) + geom_vline(xintercept = 0, color = col.gray) +
  geom_point(shape = 1) +
  scale_x_continuous(name = "Kendall \u03c4, Annual No-Flow Days", limits = c(tau_min, tau_max)) +
  scale_y_continuous(name = "Kendall \u03c4, Peak to No-Flow", limits = c(tau_min, tau_max)) +
  stat_smooth(method = "lm", color = col.cat.blu) + theme_bw()

p_mk_anf.zff <- ggplot(df_mk_wide, aes(x = mk_tau_Zero_Flow_Days_sum, y = mk_tau_hydro_doy)) +
  geom_hline(yintercept = 0, color = col.gray) + geom_vline(xintercept = 0, color = col.gray) +
  geom_point(shape = 1) +
  scale_x_continuous(name = "Kendall \u03c4, Annual No-Flow Days", limits = c(tau_min, tau_max)) +
  scale_y_continuous(name = "Kendall \u03c4, First No-Flow Day", limits = c(tau_min, tau_max)) +
  stat_smooth(method = "lm", color = col.cat.blu) + theme_bw()

p_mk_p2z.zff <- ggplot(df_mk_wide, aes(y = mk_tau_mean_dry_down_days, x = mk_tau_hydro_doy)) +
  geom_hline(yintercept = 0, color = col.gray) + geom_vline(xintercept = 0, color = col.gray) +
  geom_point(shape = 1) +
  scale_y_continuous(name = "Kendall \u03c4, Peak to No-Flow", limits = c(tau_min, tau_max)) +
  scale_x_continuous(name = "Kendall \u03c4, First No-Flow Day", limits = c(tau_min, tau_max)) +
  stat_smooth(method = "lm", color = col.cat.blu) + theme_bw()

p_mk_metric_combo <- (p_mk_anf.p2z + p_mk_anf.zff + p_mk_p2z.zff) + plot_layout(ncol = 1)

ggsave(file.path("figures_manuscript/Mine2", "Trends_MannKendall-MetricComparison.png"), plot = p_mk_metric_combo, width = 95, height = 210, units = "mm")


# ========================================================
# MANN-WHITNEY MAPS & HISTOGRAMS
# ========================================================

df_mw <- gage_trends %>% 
  dplyr::mutate(mw_diff_mean = mw_meanGroup2 - mw_meanGroup1,
                mw_diff_median = mw_medianGroup2 - mw_medianGroup1) %>% 
  dplyr::select(metric, gauge_id, region, dec_lat_va, dec_long_va, mw_p, mw_diff_mean, mw_diff_median) %>% 
  subset(complete.cases(.))

df_mw$mw_sig <- "NotSig"
df_mw$mw_sig[df_mw$mw_p < p_thres & df_mw$mw_diff_mean < 0 & df_mw$metric %in% c("hydro_doy", "mean_dry_down_days")] <- "SigDry"
df_mw$mw_sig[df_mw$mw_p < p_thres & df_mw$mw_diff_mean < 0 & df_mw$metric %in% c("Zero_Flow_Days_sum")] <- "SigWet"
df_mw$mw_sig[df_mw$mw_p < p_thres & df_mw$mw_diff_mean > 0 & df_mw$metric %in% c("hydro_doy", "mean_dry_down_days")] <- "SigWet"
df_mw$mw_sig[df_mw$mw_p < p_thres & df_mw$mw_diff_mean > 0 & df_mw$metric %in% c("Zero_Flow_Days_sum")] <- "SigDry"

p_mw_hist <- ggplot(df_mw, aes(x = mw_diff_mean, fill = mw_sig)) +
  geom_histogram(binwidth = 10, color="black", alpha=0.8) +
  geom_vline(xintercept = 0, color = "black", linetype="dashed") +
  facet_wrap(~metric, ncol = 3, scales = "free_x", labeller = 
               as_labeller(c("Zero_Flow_Days_sum" = "(a) Annual No-Flow Days",
                             "mean_dry_down_days" = "(b) Days from Peak to No-Flow",
                             "hydro_doy" = "(c) First No-Flow Day"))) +
  scale_x_continuous(name = "Change in Annual Mean [days]") +
  scale_y_continuous(name = "Number of Gages") +
  scale_fill_manual(name = "Mann-Whitney Significance", 
                    values = c("SigDry" = col.cat.red, "SigWet" = col.cat.blu, "NotSig" = col.gray),
                    labels = c("SigDry" = "Drier", "SigWet" = "Wetter", "NotSig" = "No Change"))  +
  theme_bw() + theme(legend.position = "bottom")

p_mw_map_combo <- (p_afd + p_p2z + p_zff) + plot_layout(ncol = 3) # Reusing MK maps for layout

final_mw_plot <- (p_mw_hist / p_mw_map_combo) + plot_layout(heights = c(0.4, 0.6))

ggsave(file.path("figures_manuscript/Mine2", "Trends_MannWhitney-Hist+Maps.png"), plot = final_mw_plot, width = 190, height = 150, units = "mm") 

# ========================================================
# STATISTICAL OUTPUTS (Prints to Console!)
# ========================================================
cat("\n--- MANN-KENDALL STATS ---\n")
n_anf_mk <- dim(subset(df_mk, metric == "Zero_Flow_Days_sum"))[1] 
cat("Total gauges with sufficient data (No-Flow Days):", n_anf_mk, "\n")
cat("% Significant Trend:", dim(subset(df_mk, metric == "Zero_Flow_Days_sum" & mk_p < p_thres))[1]/n_anf_mk * 100, "%\n")
cat("% Drying (More Zero-Flow Days):", dim(subset(df_mk, metric == "Zero_Flow_Days_sum" & mk_tau > 0 & mk_p < p_thres))[1]/n_anf_mk * 100, "%\n")
cat("% Wetting (Fewer Zero-Flow Days):", dim(subset(df_mk, metric == "Zero_Flow_Days_sum" & mk_tau < 0 & mk_p < p_thres))[1]/n_anf_mk * 100, "%\n\n")

# correlation between trends
cat("--- TREND CORRELATIONS ---\n")
cat("Correlation: No-Flow Days vs Peak-to-Zero:", cor(x = df_mk_wide$mk_tau_Zero_Flow_Days_sum, y = df_mk_wide$mk_tau_mean_dry_down_days, use = "complete.obs"), "\n")
cat("Correlation: No-Flow Days vs First-Zero-Day:", cor(x = df_mk_wide$mk_tau_Zero_Flow_Days_sum, y = df_mk_wide$mk_tau_hydro_doy, use = "complete.obs"), "\n")

print("Mapping and Stat Generation Complete!")