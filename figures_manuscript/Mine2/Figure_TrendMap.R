# ========================================================
# UPDATED: Figure_TrendMap.R (Using Custom Shapefile)
# ========================================================
install.packages("sf")

source(file.path("code", "paths+packages.R"))
library(tidyverse)
library(patchwork)
library(sf) # Required for reading and plotting shapefiles!

# ========================================================
# 1. LOAD GIS SHAPEFILE
# ========================================================
# ---> INSERT THE PATH TO YOUR PENINSULAR SHAPEFILE HERE <---
path_to_shapefile <- "D:/Peninsular India/Catchments/peninsular_catchment_merged.shp" 

sf_peninsula <- sf::st_read(path_to_shapefile)

# ========================================================
# 2. LOAD & PREP TREND DATA
# ========================================================
col_lat <- "cwc_lat" 
col_lon <- "cwc_lon" 

gage_mean <- readr::read_csv(file.path("results/Mine", "Physiographic_data.csv"), show_col_types = FALSE) %>%
  mutate(gauge_id = as.character(gauge_id)) %>%
  rename(dec_lat_va = all_of(col_lat), dec_long_va = all_of(col_lon))

# Load the trends we calculated in Path A
site_trends <- readr::read_csv(file.path("results/Mine", "gauge_trends.csv"), show_col_types = FALSE) %>% 
  mutate(gauge_id = as.character(gauge_id)) %>%
  dplyr::left_join(gage_mean[,c("gauge_id", "dec_lat_va", "dec_long_va")], by = "gauge_id") %>%
  filter(!is.na(dec_lat_va) & !is.na(dec_long_va))

# Convert the regular dataframe into an "sf" spatial object using Lat/Lon
sf_site_trends <- site_trends %>%
  sf::st_as_sf(coords = c("dec_long_va", "dec_lat_va"), 
               crs = 4326) # 4326 is the standard WGS84 Lat/Lon coordinate system

if(!dir.exists("figures_manuscript")) dir.create("figures_manuscript")

# ========================================================
# 3. HELPER FUNCTION TO DRAW MAPS
# ========================================================
draw_trend_map <- function(data, metric_name, title_name, legend_name) {
  
  # Filter data for the specific metric
  df_plot <- subset(data, metric == metric_name & !is.na(sen_slope) & !is.na(mk_p))
  
  # Set limits so outliers don't wash out the color scale
  slope_limit <- quantile(abs(df_plot$sen_slope), 0.98, na.rm=TRUE) 
  
  ggplot() +
    # Draw your custom Peninsular boundary!
    geom_sf(data = sf_peninsula, fill = "white", color = "black", size = 0.5) +
    
    # Plot the gauges on top!
    geom_sf(data = df_plot, 
            aes(shape = mk_p < 0.05, color = sen_slope), 
            size = 2.5, alpha = 0.9) +
    
    # Styling
    scale_shape_manual(name = "Significant (p < 0.05)", 
                       values = c("FALSE" = 1, "TRUE" = 16), guide = "none") +
    scale_color_gradient2(name = legend_name, low = "blue", mid = "gray80", high = "red", 
                          limits = c(-slope_limit, slope_limit), oob = scales::squish) +
    labs(title = title_name,
         subtitle = "Filled Circles = Sig. Trend (p < 0.05)") +
    theme_void() + 
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))
}

# ========================================================
# 4. GENERATE THE 3 MAPS
# ========================================================

p_noflowdays <- draw_trend_map(
  data = sf_site_trends, 
  metric_name = "Zero_Flow_Days_sum", 
  title_name = "Trend, Annual No Flow Days", 
  legend_name = "Trend[days/yr]"
)

p_noflowperiods <- draw_trend_map(
  data = sf_site_trends, 
  metric_name = "mean_dry_down_days", 
  title_name = "Trend, Days from Peak to No-Flow", 
  legend_name = "Trend[days/yr]"
)

p_noflowlength <- draw_trend_map(
  data = sf_site_trends, 
  metric_name = "hydro_doy", 
  title_name = "Trend, First No-Flow Day", 
  legend_name = "Trend [day of year/yr]"
)

# ========================================================
# 5. COMBINE AND SAVE
# ========================================================

# Stitch together side-by-side using Patchwork
final_trend_map <- (p_noflowdays | p_noflowperiods | p_noflowlength) + 
  plot_layout(guides = "keep")

ggsave(file.path("figures_manuscript/Mine2", "Figure_TrendMap.png"), 
       plot = final_trend_map, width = 300, height = 150, units = "mm", bg="white")

print("Trend Maps generated successfully with your custom boundary!")