# ========================================================
# UPDATED: Redundancy_CompareTrends.R
# ========================================================

source(file.path("code", "paths+packages.R"))
library(tidyverse)
library(sf)

# 1. DEFINE COLORS & METRICS
metrics <- c("Zero_Flow_Days_sum", "hydro_doy", "mean_dry_down_days")

col.gray <- "gray50"
col.cat.red <- "#e41a1c"
col.cat.yel <- "#ff7f00" # Orange/Yellow

data_dir <- "results/Mine"

# 2. LOAD DATA
# Check if the redundancy file actually exists!
redundancy_file <- file.path("results/Mine2", "RedundancyAnalysis_RedundantGages.csv")
if(!file.exists(redundancy_file)) {
  stop("ERROR: Could not find 'RedundancyAnalysis_RedundantGages.csv'. Please run RedundancyAnalysis.R first!")
}
redundant_gages <- readr::read_csv(redundancy_file, show_col_types = FALSE) %>%
  mutate(gauge_id = as.character(gauge_id))

gage_mean <- readr::read_csv(file.path(data_dir, "Physiographic_data.csv"), show_col_types = FALSE) %>%
  mutate(gauge_id = as.character(gauge_id)) %>%
  rename(dec_lat_va = cwc_lat, dec_long_va = cwc_lon)

gage_regions <- readr::read_csv(file.path(data_dir, "station_ai_regions.csv"), show_col_types = FALSE) %>% 
  mutate(gauge_id = as.character(gauge_id))

gage_trends <- readr::read_csv(file.path(data_dir, "gauge_trends.csv"), show_col_types = FALSE) %>% 
  mutate(gauge_id = as.character(gauge_id)) %>%
  subset(metric %in% metrics) %>% 
  dplyr::left_join(gage_mean[,c("gauge_id", "dec_lat_va", "dec_long_va")], by = "gauge_id") %>% 
  dplyr::left_join(gage_regions, by = "gauge_id") %>%
  dplyr::left_join(redundant_gages, by = "gauge_id") # Joins the redundancy flag

# Generate Dynamic Color Palettes
unique_regions <- na.omit(unique(gage_trends$region))
pal_regions <- scales::hue_pal()(length(unique_regions))
names(pal_regions) <- unique_regions

# Load your custom Peninsular Shapefile!
path_to_shapefile <- "D:/Peninsular India/Catchments/peninsular_catchment_merged.shp" # <--- UPDATE THIS!
sf_peninsula <- sf::st_read(path_to_shapefile)

p_thres <- 0.05

if(!dir.exists("figures_manuscript")) dir.create("figures_manuscript")

# ========================================================
# PLOT 1: MAP OF REDUNDANT GAGES
# ========================================================
sf_gages <- gage_trends %>%
  dplyr::select(dec_lat_va, dec_long_va, redundant) %>% 
  filter(!is.na(dec_lat_va) & !is.na(dec_long_va)) %>%
  unique() %>% 
  sf::st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4326) 

p_map <- ggplot() + 
  geom_sf(data = sf_peninsula, fill = "white", color = "black", size = 0.5) +
  # is.na(redundant) means it is NOT in the redundant list
  geom_sf(data = sf_gages, aes(color = is.na(redundant)), size = 2) +
  scale_color_manual(name = "Gauge Status",
                     values = c("TRUE" = "black", "FALSE" = col.cat.red),
                     labels = c("TRUE" = "Independent (Not Redundant)", "FALSE" = "Potentially Redundant")) +
  theme_void() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(title = "Map of Independent vs. Redundant Gauges")

ggsave(file.path("figures_manuscript/Mine2", "Redundancy_MapOfGages.png"),
       plot = p_map, width = 190, height = 150, units = "mm", bg="white")


# ========================================================
# PLOT 2: TREND SIGNIFICANCE OF REMOVED GAGES
# ========================================================
p_bar <- gage_trends %>% 
  subset(redundant == TRUE & is.finite(mk_p)) %>% 
  ggplot(aes(x = metric, fill = mk_p < p_thres)) +
  geom_bar() +
  scale_x_discrete(name = "Intermittency Signature",
                   labels = c("Zero_Flow_Days_sum" = "Annual\nNo-Flow Days",
                              "mean_dry_down_days" = "Days from Peak\nto No-Flow",
                              "hydro_doy" = "First\nNo-Flow Day")) +
  scale_y_continuous(name = "Number of Gages") +
  scale_fill_manual(name = "Mann-Kendall Significance", 
                    values = c("FALSE" = col.cat.yel, "TRUE" = col.cat.red), 
                    labels = c("FALSE" = "p > 0.05", "TRUE" = "p < 0.05")) +
  labs(title = "Trend Significance of the Redundant Gages") +
  theme_bw() + theme(legend.position = "bottom")

ggsave(file.path("figures_manuscript/Mine2", "Redundancy_BarChart.png"),
       plot = p_bar, width = 190, height = 120, units = "mm")


# ========================================================
# PLOT 3: COMPARE MEDIAN TRENDS (WITH VS WITHOUT REDUNDANT)
# ========================================================
# 1. Calculate medians using ONLY independent gauges
gage_trends_regionRemoveRedundant <- gage_trends %>% 
  subset(is.na(redundant) | redundant == FALSE) %>% 
  dplyr::group_by(region, metric) %>% 
  dplyr::summarize(mk_tau_median = median(mk_tau, na.rm = T), .groups="drop")

# 2. Calculate medians using ALL gauges, then join
gage_trends_region <- gage_trends %>% 
  dplyr::group_by(region, metric) %>% 
  dplyr::summarize(mk_tau_median = median(mk_tau, na.rm = T), .groups="drop") %>% 
  dplyr::left_join(gage_trends_regionRemoveRedundant, by = c("region", "metric"), suffix = c(".all", ".trim"))

# 3. Plot them against each other
p_compare <- ggplot(gage_trends_region, aes(x = mk_tau_median.all, y = mk_tau_median.trim, color = region, shape = metric)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray, linetype="dashed") +
  geom_point(size = 3) +
  scale_x_continuous(name = "Median Kendall \u03c4 (ALL gauges)") +
  scale_y_continuous(name = "Median Kendall \u03c4 (Trimmed for redundancy)") +
  scale_color_manual(name = "Region", values = pal_regions) +
  scale_shape_discrete(name = "Intermittency Signature",
                       labels = c("Zero_Flow_Days_sum" = "Annual No-Flow Days",
                                  "mean_dry_down_days" = "Days from Peak to No-Flow",
                                  "hydro_doy" = "First No-Flow Day")) +
  labs(title = "Effect of Removing Redundant Gauges on Regional Trends",
       subtitle = "If points fall on the dashed line, the redundant gauges did not bias the regional trend!") +
  theme_bw() + theme(legend.box = "vertical")

ggsave(file.path("figures_manuscript/Mine2", "Redundancy_CompareMedianTrend.png"),
       plot = p_compare, width = 190, height = 150, units = "mm")

print("Redundancy maps and comparisons generated successfully!")