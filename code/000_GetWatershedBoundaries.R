## 000_GetWatershedBoundaries.R
# Goal: Obtain watershed boundaries for study gages.

library(tidyverse)
library(sf)

# load gages
df_gages <- read_csv(file.path("results", "00_SelectGagesForAnalysis_GageRegions.csv"))

# pad site_no with leading 0 if less than 8 digits
df_gages <- 
  df_gages |> 
  mutate(gage_ID_8dig = str_pad(gage_ID, width = 8, side = "left", pad = "0"))
df_gages$gage_ID_8dig[230] <- str_pad(df_gages$gage_ID_8dig[230], width = 10, side = "left", pad = "0")

# load gages
sf_gageloc <- st_read(file.path("data", "USGS_GageLocations.gpkg")) |> 
  subset(site_no %in% df_gages$gage_ID)


# figure out watershed boundaries
boundary_path <- "C:/Users/s947z036/OneDrive - University of Kansas/GIS_GeneralFiles/USGS_GageLoc/GAGES-II/"
sf_boundaries <- st_read(paste0(boundary_path, "GAGESII_basins_all.gpkg"))

sf_gage_boundaries <- 
  sf_boundaries |> 
  subset(GAGE_ID %in% df_gages$gage_ID_8dig)

ggplot() + 
  geom_sf(data = sf_gage_boundaries) +
  geom_sf(data = sf_gageloc)

# save boundaries
st_write(sf_gage_boundaries, file.path("data", "USGS_WatershedBoundaries.gpkg"))
