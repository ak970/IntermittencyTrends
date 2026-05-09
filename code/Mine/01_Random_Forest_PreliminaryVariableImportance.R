# ========================================================
# UPDATED: 01_RandomForest_PreliminaryVariableImportance.R
# ========================================================
# install.packages("ggplot2")

source(file.path("code", "paths+packages.R"))
library(tidymodels)
library(ranger)
library(partykit)
library(future.apply)
library(reshape2)
library(dplyr)

####
#### prep data
####

# 1. Load Data
gage_sample <- 
  readr::read_csv(file = file.path("results/Mine", "Physiographic_data.csv"))

gage_regions <- 
  readr::read_csv(file.path("results/Mine", "station_ai_regions.csv"))

gage_sample_annual <-
  readr::read_csv(file = file.path("results/Mine", "gauges_annual_summary_out.csv"))

## set up predictions
metrics <- c("Zero_Flow_Days_sum", "hydro_doy", "mean_dry_down_days")

## Temporary fix, if only select metrics are needed 
# metrics <- c("Zero_Flow_Days_sum")


# regions <- c("National", unique(gage_sample_annual$region))
regions <- c("National", na.omit(unique(gage_sample_annual$region)))

# # Temporary fix to pick up exactly where you left off:
# regions <- c("Humid")
# regions <- na.omit(unique(gage_sample_annual$region))
# regions <- regions[!(regions %in% c("Dry_sub_humid", "Semi_arid"))] # Mention classes to skip

# ========================================================
# 2. MANUALLY DEFINE PREDICTORS (Replace with your columns!)
# ========================================================

predictors_climate <- c(
  'tmax', 'tmin',
  'tavg', 'srad_lw(w/m2)', 'srad_sw(w/m2)', 'wind_u(m/s)', 'wind_v(m/s)',
  'wind(m/s)', 'rel_hum(%)', 'sm_lvl1(kg/m2)', 'sm_lvl2(kg/m2)',
  'sm_lvl3(kg/m2)', 'sm_lvl4(kg/m2)', 'p', 'pet', 'aet',
  'evap_canopy(mm/day)', 'evap_surface(mm/day)', 'tmax_djf', 'tmax_jjas',
  'tmax_mam', 'tmax_on', 'tmin_djf', 'tmin_jjas', 'tmin_mam', 'tmin_on',
  'tavg_djf', 'tavg_jjas', 'tavg_mam', 'tavg_on', 'p_djf', 'p_jjas',
  'p_mam', 'p_on', 'pet_djf', 'pet_jjas', 'pet_mam', 'pet_on', 'aet_djf',
  'aet_jjas', 'aet_mam', 'aet_on', 'evap_canopy(mm/day)_djf',
  'evap_canopy(mm/day)_jjas', 'evap_canopy(mm/day)_mam',
  'evap_canopy(mm/day)_on', 'evap_surface(mm/day)_djf',
  'evap_surface(mm/day)_jjas', 'evap_surface(mm/day)_mam',
  'evap_surface(mm/day)_on', 'spei3_drought_count', 'spei6_drought_count',
  'spei12_drought_count', 'spei24_drought_count', 'spei3_drought_valid_n',
  'spei6_drought_valid_n', 'spei12_drought_valid_n',
  'spei24_drought_valid_n', 'spei3_drought_max_spell',
  'spei6_drought_max_spell', 'spei12_drought_max_spell',
  'spei24_drought_max_spell', 'spei3_drought_count_frac',
  'spei6_drought_count_frac', 'spei12_drought_count_frac',
  'spei24_drought_count_frac'
)

predictors_human <- c(
  'urban_pct', 'Total_Dams_So_Far', 'Total_Volume_So_Far'# <-- YOUR ANNUAL HUMAN COLUMNS HERE (If you don't have any, just leave this as c() or delete the text inside)
)

predictors_static <- c(
  'num_dams', 'res_store_sum', 'total_storage', 'geol_porosity',
  'geol_permeability', 'carb_rocks_frac',
  'lai_mean', 'lai_min', 'lai_max', 'lai_diff',
  'soil_depth', 'soil_conductivity_top', 'soil_conductivity_sub',
  'soil_awc_top', 'soil_awc_sub', 'soil_awsc_min', 'soil_awsc_max',
  'soil_awsc_major', 'sand_frac_top', 'sand_frac_sub', 'silt_frac_top',
  'silt_frac_sub', 'clay_frac_top', 'clay_frac_sub', 'gravel_frac_top', 
  'gravel_frac_sub', 'bulkdens_top_major', 'bulkdense_top_mean',
  'bulkdens_sub_mean', 'org_carb_top_major', 'org_carb_top_mean',
  'org_carb_sub_major', 'org_carb_sub_mean', 'organic_frac_top',
  'organic_frac_sub', 'cwc_lat', 'cwc_lon',
  'elev_mean', 'elev_median', 'elev_min', 'elev_max', 'slope_mean',
  'slope_median', 'slope_min', 'slope_max', 'cwc_area',
  'dpsbar', 'sinuosity' # <-- YOUR STATIC COLUMNS HERE (Including static human vars)
)

# ========================================================



# ========================================================
# 2.5 FIX: Sanitize all column names to remove special characters!
# ========================================================
# This built-in R function converts things like "srad_lw(w/m2)" to "srad_lw.w.m2."
# so that the Random Forest formula doesn't crash!

names(gage_sample_annual) <- make.names(names(gage_sample_annual))
names(gage_sample) <- make.names(names(gage_sample))

predictors_climate <- make.names(predictors_climate)
predictors_human <- make.names(predictors_human)
predictors_static <- make.names(predictors_static)
metrics <- make.names(metrics)
# ========================================================



## 3. Calculate previous year climate metrics
# (Notice we only lag the climate variables, not the human/static ones!)
gage_sample_prevyear <- 
  gage_sample_annual %>% 
  dplyr::select(gauge_id, hydro_year, all_of(predictors_climate)) %>% 
  dplyr::mutate(wyearjoin = hydro_year + 1) %>% 
  dplyr::select(-hydro_year)

# Rename the columns to have a ".previous" suffix
names(gage_sample_prevyear)[names(gage_sample_prevyear) %in% predictors_climate] <- 
  paste0(names(gage_sample_prevyear)[names(gage_sample_prevyear) %in% predictors_climate], ".previous")

## 4. Combine into one master fit dataset
fit_data_in <- 
  gage_sample_annual %>% 
  dplyr::select(gauge_id, hydro_year, Sample, region, all_of(metrics), 
                all_of(predictors_climate), any_of(predictors_human)) %>% 
  # join with previous water year
  dplyr::left_join(gage_sample_prevyear, 
                   by = c("gauge_id", "hydro_year"="wyearjoin")) %>% 
  # join with static predictors
  dplyr::left_join(gage_sample %>% dplyr::select(gauge_id, any_of(predictors_static)), 
                   by = "gauge_id")

# Create the final list of all predictor names
predictors_climate_with_previous <- c(predictors_climate, paste0(predictors_climate, ".previous"))
predictors_all <- c(predictors_climate_with_previous, predictors_human, predictors_static)

###
### Filter out highly correlated predictor variables
###




###
### Filter out highly correlated & zero-variance predictor variables
###

# 1. Isolate the training data
train_data_only <- fit_data_in %>% 
  filter(Sample == "Train") %>% 
  dplyr::select(all_of(predictors_all))

# 2. Find and drop variables with zero variance (or 100% NAs)
sd_vals <- sapply(train_data_only, function(x) sd(x, na.rm = TRUE))
zero_var_cols <- names(sd_vals[is.na(sd_vals) | sd_vals == 0])

if(length(zero_var_cols) > 0) {
  print("Dropping variables with zero variance (constant values) or 100% NAs:")
  print(zero_var_cols)
  
  # Remove them from the active predictors list
  predictors_all <- predictors_all[!(predictors_all %in% zero_var_cols)]
  train_data_only <- train_data_only %>% dplyr::select(all_of(predictors_all))
}

# 3. Automatically find correlations > 0.9 and drop the redundant ones
check_cor <- cor(
  train_data_only, 
  use = "pairwise.complete.obs", 
  method = "pearson"
)

# Prevent any NAs from breaking the matrix logic
check_cor[is.na(check_cor)] <- 0 
check_cor[lower.tri(check_cor, diag = TRUE)] <- 0 

cor_high <- check_cor %>% 
  reshape2::melt() %>% 
  subset(abs(value) > 0.9 & Var1 != Var2) 

predictors_drop <- as.character(unique(cor_high$Var2))

# 4. Final trimmed predictors list to use in models
predictors_trimmed <- predictors_all[!(predictors_all %in% predictors_drop)]

print(paste("Dropped", length(predictors_drop), "highly correlated variables."))
print("Remaining predictors to be used in Random Forest:")
print(predictors_trimmed)


# # Automatically find correlations > 0.9 and drop the redundant ones
# check_cor <- cor(
#   fit_data_in %>% filter(Sample == "Train") %>% dplyr::select(all_of(predictors_all)), 
#   use = "pairwise.complete.obs", 
#   method = "pearson"
# )
# 
# # Prevent any NAs from breaking the matrix logic
# check_cor[is.na(check_cor)] <- 0 
# check_cor[lower.tri(check_cor, diag = TRUE)] <- 0 
# 
# cor_high <- check_cor %>% 
#   reshape2::melt() %>% 
#   subset(abs(value) > 0.9 & Var1 != Var2) 
# 
# predictors_drop <- as.character(unique(cor_high$Var2))
# 
# # trimmed predictors list to use in models
# predictors_trimmed <- predictors_all[!(predictors_all %in% predictors_drop)]
# 
# print(paste("Dropped", length(predictors_drop), "highly correlated variables."))
# print("Remaining predictors:")
# print(predictors_trimmed)

###
### begin loop through metrics and regions
###

# number of cores to use
ncores <- (parallel::detectCores() - 1)

# # Turn OFF parallel processing (forces 1 core for perfect reproducibility)
# ncores <- 1

## loop through metrics and regions
set.seed(1)
for (m in metrics){
  
  # subset to complete cases for the current metric
  fit_data_m <- 
    fit_data_in %>% 
    subset(Sample == "Train") %>% 
    dplyr::select(gauge_id, hydro_year, region, all_of(m), all_of(predictors_trimmed)) %>% 
    subset(complete.cases(.))
  
  # rename metric column
  names(fit_data_m)[names(fit_data_m) == m] <- "observed"
  
  for (r in regions){
    if (r == "National") {
      fit_data_r <- fit_data_m
    } else {
      fit_data_r <- subset(fit_data_m, region == r)
    }
    
    if(nrow(fit_data_r) < 20) {
      print(paste("Skipping", m, r, "- Not enough data."))
      next
    }
    
    fit_rf <- partykit::cforest(
      observed ~ .,
      data = dplyr::select(fit_data_r, -gauge_id, -hydro_year, -region),
      control = ctree_control(mincriterion = 0.95),
      ntree = 500, 
      applyfun = apply_seeded,
      cores = ncores
    )
    
    vi <- partykit::varimp(fit_rf, 
                           conditional = T, 
                           applyfun = apply_seeded,
                           cores = ncores)
    
    fit_rf_imp_i <- tibble::tibble(predictor = names(vi),
                                   ImpCondPerm = vi)
    
    if(!dir.exists("results")) dir.create("results")
    
    # write csv file separately for each metric and region
    fit_rf_imp_i %>% 
      readr::write_csv(file = file.path("results/Mine/Run3", paste0("01_RandomForest_PreliminaryVariableImportance_", m, "_", gsub(" ", "", r, fixed = TRUE), ".csv")))
    
    # status update
    print(paste0(m, " ", r, " complete, ", Sys.time()))
    
  }
}
