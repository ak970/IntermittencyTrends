# ========================================================
# UPDATED: Trends-Climate_Violins.R
# ========================================================

source(file.path("code", "paths+packages.R"))
library(tidyverse)
library(patchwork)
library(colorspace)

# 1. DEFINE YOUR CLIMATE METRICS AND LABELS
# Change these if you want to plot different climate/driver variables!
metrics <- c("p", "pet", "tmax", "tmin")

labs_metrics <- c("p" = "Precipitation", 
                  "pet" = "PET", 
                  "tmax" = "Max Temperature", 
                  "tmin" = "Min Temperature")

col.gray <- "gray50"

# 2. LOAD DATA
data_dir <- "results/Mine"

gage_regions <- readr::read_csv(file.path(data_dir, "station_ai_regions.csv"), show_col_types = FALSE) %>%
  mutate(gauge_id = as.character(gauge_id)) %>%
  dplyr::select(gauge_id, region)

gage_mean <- readr::read_csv(file.path(data_dir, "Physiographic_data.csv"), show_col_types = FALSE) %>%
  mutate(gauge_id = as.character(gauge_id)) %>%
  rename(dec_lat_va = cwc_lat, dec_long_va = cwc_lon) %>%
  dplyr::select(gauge_id, dec_lat_va, dec_long_va)

# Load trends, safely ignoring any duplicate region columns
gage_trends <- readr::read_csv(file.path(data_dir, "gauge_trends.csv"), show_col_types = FALSE) %>% 
  mutate(gauge_id = as.character(gauge_id)) %>%
  dplyr::select(gauge_id, metric, mk_tau) %>%
  dplyr::left_join(gage_regions, by = "gauge_id") %>% 
  dplyr::left_join(gage_mean, by = "gauge_id")

# 3. PREP DATA AND DYNAMIC COLORS
gage_hydro_trends <- subset(gage_trends, metric %in% metrics)
gage_hydro_trends$metric <- factor(gage_hydro_trends$metric, levels = metrics)

# Figure out tau axis limits dynamically based on your trends
tau_min <- min(gage_hydro_trends$mk_tau, na.rm = TRUE)
tau_max <- max(gage_hydro_trends$mk_tau, na.rm = TRUE)
tau_abs <- max(abs(c(tau_min, tau_max)))

# Generate Dynamic Color Palettes for the Regions
unique_regions <- na.omit(unique(gage_hydro_trends$region))
pal_regions <- scales::hue_pal()(length(unique_regions))
names(pal_regions) <- unique_regions

# Create a slightly darker version for the borders of the violins
pal_regions_dk <- colorspace::darken(pal_regions, amount = 0.2)
names(pal_regions_dk) <- unique_regions

if(!dir.exists("figures_manuscript")) dir.create("figures_manuscript")

# ========================================================
# PLOT 1: REGIONAL CLIMATE VIOLINS
# ========================================================

p_region <- gage_hydro_trends %>% 
  filter(!is.na(region) & !is.na(mk_tau)) %>%
  ggplot(aes(x = region, y = mk_tau, fill = region, color = region)) +
  geom_hline(yintercept = 0, color = col.gray, linetype="dashed") +
  geom_violin(draw_quantiles = 0.5, alpha = 0.7) +
  facet_wrap(~metric, ncol = 1, dir = "v",
             scales = "free_y", labeller = as_labeller(labs_metrics)) +
  scale_x_discrete(name = "Region", labels = function(x) stringr::str_wrap(x, width = 10)) +
  scale_y_continuous(name = "Trend (Kendall \u03c4)", limits = c(-tau_abs, tau_abs),
                     breaks = seq(-0.6, 0.6, 0.3), expand = c(0, 0.01)) +
  scale_fill_manual(values = pal_regions, guide = "none") +
  scale_color_manual(values = pal_regions_dk, guide = "none") +
  theme_bw() + theme(strip.text = element_text(face = "bold"))

# Save the standalone region plot
ggsave(file.path("figures_manuscript/Mine2", "Trends-Climate_Violins-Region.png"),
       plot = p_region, width = 140, height = 240, units = "mm")

# ========================================================
# PLOT 2: LATITUDE CLIMATE VIOLINS (ADJUSTED FOR INDIA)
# ========================================================

# Cut the latitudes into 4-degree bins suitable for Peninsular India (8N to 28N)
gage_hydro_trends$lat_bin <- cut(gage_hydro_trends$dec_lat_va, 
                                 breaks = seq(8, 28, 4),
                                 labels = seq(10, 26, 4)) 

p_lat <- gage_hydro_trends %>%
  filter(!is.na(lat_bin) & !is.na(mk_tau)) %>%
  ggplot(aes(x = lat_bin, y = mk_tau)) +
  geom_hline(yintercept = 0, color = col.gray, linetype="dashed") +
  geom_violin(draw_quantiles = 0.5, fill = "gray80", color = "gray30") +
  facet_wrap(~metric, ncol = 1, dir = "v",
             scales = "free_y", labeller = as_labeller(labs_metrics)) +
  coord_flip() + # Puts latitude on the Y axis
  scale_y_continuous(name = "Trend (Kendall \u03c4)", limits = c(-tau_abs, tau_abs),
                     breaks = seq(-0.6, 0.6, 0.3), expand = c(0, 0.01)) +
  scale_x_discrete(name = "Latitude[\u00b0N]") +
  theme_bw() + theme(strip.text = element_blank()) # Hides duplicated facet labels

# ========================================================
# COMBINE AND SAVE
# ========================================================

final_plot <- (p_region + p_lat + plot_layout(widths = c(0.6, 0.4)))

ggsave(file.path("figures_manuscript/Mine2", "Trends-Climate_Violins-Region+Lat.png"),
       plot = final_plot, width = 210, height = 200, units = "mm")

# ========================================================
# STATISTICAL OUTPUT
# ========================================================

cat("\n--- NUMBER OF GAUGES BY REGION (CLIMATE TRENDS) ---\n")
print(
  gage_hydro_trends %>% 
    subset(is.finite(mk_tau)) %>% 
    dplyr::group_by(metric, region) %>% 
    dplyr::summarize(n_gages = n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = metric, values_from = n_gages)
)