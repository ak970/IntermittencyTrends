# ========================================================
# UPDATED: RedundancyAnalysis.R (Using Catchment Shapefiles)
# ========================================================

source(file.path("code", "paths+packages.R"))
library(tidyverse)
library(sf)
library(stringr)

# ========================================================
# 1. LOAD MASTER DATA & PAD IDs
# ========================================================
data_dir <- "results/Mine"

# Read Physiographic data to get the list of valid gauges and their areas
gage_data <- readr::read_csv(file.path(data_dir, "Physiographic_data.csv"), show_col_types = FALSE) %>%
  dplyr::select(gauge_id, cwc_area) %>%
  mutate(
    # Pad the CSV IDs to exactly 5 digits (e.g., "4220" becomes "04220")
    gauge_id_padded = str_pad(as.character(gauge_id), width = 5, pad = "0")
  ) %>%
  filter(!is.na(cwc_area))

# ========================================================
# 2. LOAD SHAPEFILE & FILTER
# ========================================================
# ---> INSERT YOUR CATCHMENT SHAPEFILE PATH HERE <---
path_to_shapefile <- "D:/Peninsular India/CAMELS_IND/CAMELS_IND_Catchments_Streamflow_Sufficient3/shapefiles_catchment/combined_subcat.shp" 

cat("Loading shapefile and filtering to master dataset...\n")
catchments_sf <- sf::st_read(path_to_shapefile, quiet = TRUE) %>%
  mutate(
    # Ensure shapefile IDs are also padded just to be 100% safe
    gauge_id_padded = str_pad(as.character(gauge_id), width = 5, pad = "0")
  ) %>%
  # Filter shapefile to ONLY keep gauges that exist in our master dataset
  filter(gauge_id_padded %in% gage_data$gauge_id_padded)

cat(paste("Successfully matched", nrow(catchments_sf), "catchments between the shapefile and CSV.\n"))

# ========================================================
# 3. CALCULATE CENTROIDS & DISTANCES
# ========================================================
cat("Calculating catchment centroids (Center of Mass)...\n")

# FIX: Turn off strict spherical geometry and repair any broken shapefile polygons!
sf::sf_use_s2(FALSE)
catchments_sf <- sf::st_make_valid(catchments_sf)

# Note: sf may throw a standard warning about attributes being constant over geometries. Ignore it!
centroids_sf <- sf::st_centroid(catchments_sf)

cat("Calculating distances between centroids...\n")
dist_matrix_m <- sf::st_distance(centroids_sf)

# Convert from meters to Kilometers
dist_matrix_km <- as.numeric(dist_matrix_m) / 1000
dist_matrix_km <- matrix(dist_matrix_km, nrow = nrow(centroids_sf))

# Apply the PADDED gauge IDs to the matrix
rownames(dist_matrix_km) <- centroids_sf$gauge_id_padded
colnames(dist_matrix_km) <- centroids_sf$gauge_id_padded

# Convert the matrix into a clean "Long" format dataframe
centroid_distances_long <- as.data.frame(as.table(dist_matrix_km))
colnames(centroid_distances_long) <- c("gage1", "gage2", "dist_km")

centroid_distances_long <- centroid_distances_long %>%
  mutate(gage1 = as.character(gage1), gage2 = as.character(gage2)) %>%
  filter(gage1 != gage2)

# ========================================================
# 4. CALCULATE REDUNDANCY METRICS (DAR & SD)
# ========================================================

# Join the catchment areas (using the padded IDs to match perfectly)
centroid_distances_long <- centroid_distances_long %>%
  left_join(gage_data[,c("gauge_id_padded", "cwc_area")], by = c("gage1" = "gauge_id_padded")) %>%
  rename(area_sqkm_1 = cwc_area) %>%
  left_join(gage_data[,c("gauge_id_padded", "cwc_area")], by = c("gage2" = "gauge_id_padded")) %>%
  rename(area_sqkm_2 = cwc_area)

# Calculate SD and DAR
centroid_distances_long <- centroid_distances_long %>% 
  rowwise() %>% 
  mutate(
    DAR = max(area_sqkm_1 / area_sqkm_2, area_sqkm_2 / area_sqkm_1), 
    SD = dist_km / sqrt(0.5 * (area_sqkm_1 + area_sqkm_2))
  ) %>%
  ungroup()

# Gruber and Stedinger (2008) thresholds
redundant_in <- filter(centroid_distances_long, SD <= 0.50 & DAR <= 5)
redundant_in <- redundant_in %>% filter(gage1 < gage2) # Drop duplicate reverse-pairs

cat(paste(nrow(redundant_in), "potential overlapping (nested) pairs identified!\n"))

# ========================================================
# 5. ITERATIVE ELIMINATION LOOP
# ========================================================
redundant_out <- redundant_in
redundant_gages_padded <- c()

while (nrow(redundant_out) > 0) {
  
  remaining_gages_with_areas <- c(redundant_out$gage2, redundant_out$gage1) %>% 
    tibble::tibble(gauge_id_padded = .) %>% 
    dplyr::group_by(gauge_id_padded) %>% 
    dplyr::summarize(count = n(), .groups = "drop") %>% 
    dplyr::left_join(gage_data[,c("gauge_id_padded", "cwc_area")], by = "gauge_id_padded") %>% 
    dplyr::arrange(-count, -cwc_area)
  
  gid <- remaining_gages_with_areas$gauge_id_padded[1]
  redundant_gages_padded <- c(redundant_gages_padded, gid)
  
  redundant_out <- subset(redundant_out, gage1 != gid & gage2 != gid)
}

cat(paste("\nFinished! A total of", length(redundant_gages_padded), "redundant gauges were eliminated.\n"))

# ========================================================
# 6. UN-PAD IDs AND SAVE RESULTS
# ========================================================

final_redundant_df <- tibble(gauge_id_padded = redundant_gages_padded, redundant = TRUE) %>%
  # Strip the leading zeros back off so they perfectly match your other CSVs!
  mutate(gauge_id = as.character(as.numeric(gauge_id_padded))) %>%
  dplyr::select(gauge_id, redundant)

final_redundant_df %>% 
  readr::write_csv(file.path("results/Mine2", "RedundancyAnalysis_RedundantGages.csv"))

cat("Results safely un-padded and saved to 'results/Mine2/RedundancyAnalysis_RedundantGages.csv'!\n")