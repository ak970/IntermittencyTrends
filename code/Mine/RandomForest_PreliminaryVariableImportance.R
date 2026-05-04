# ========================================================
# UPDATED: 01_RandomForest_PreliminaryVariableImportance.R
# ========================================================

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
regions <- c("National", unique(gage_sample_annual$region))

# ========================================================
# 2. MANUALLY DEFINE PREDICTORS (Replace with your columns!)
# ========================================================

predictors_climate <- c(
  "precipitation", "temp" # <-- YOUR ANNUAL CLIMATE COLUMNS HERE
)

predictors_human <- c(
  # <-- YOUR ANNUAL HUMAN COLUMNS HERE (If you don't have any, just leave this as c() or delete the text inside)
)

predictors_static <- c(
  "latitude", "longitude", "area" # <-- YOUR STATIC COLUMNS HERE (Including static human vars)
)

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

# Automatically find correlations > 0.9 and drop the redundant ones
check_cor <- cor(
  fit_data_in %>% filter(Sample == "Train") %>% dplyr::select(all_of(predictors_all)), 
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

# trimmed predictors list to use in models
predictors_trimmed <- predictors_all[!(predictors_all %in% predictors_drop)]

print(paste("Dropped", length(predictors_drop), "highly correlated variables."))
print("Remaining predictors:")
print(predictors_trimmed)

###
### begin loop through metrics and regions
###

# number of cores to use
ncores <- (parallel::detectCores() - 1)

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
      readr::write_csv(file = file.path("results/Mine", paste0("01_RandomForest_PreliminaryVariableImportance_", m, "_", gsub(" ", "", r, fixed = TRUE), ".csv")))
    
    # status update
    print(paste0(m, " ", r, " complete, ", Sys.time()))
    
  }
}