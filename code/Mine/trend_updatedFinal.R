# ==============================
# CORRECTED CODE FROM 00a_SelectgaugesForAnalysis.R
# ==============================
source(file.path("code", "paths+packages.R"))

library(dplyr)
library(tibble)
library(tidyverse)
library(Metrics)

gauges_annual_summary <- read_csv("C:/Users/hp/Documents/GitHub/IntermittencyTrends/results/Mine/Final_Annual_Master_Dataset.csv")
region_df <- readr::read_csv("D:/Peninsular India/Merged_data/Output/Data_to_use/station_ai_regions.csv")

# FIX 1: Only replace "NA" strings in character columns to prevent type-mismatch errors
gauges_annual_summary <- gauges_annual_summary %>%
  mutate(across(where(is.character), ~na_if(., "NA")))

gauge_sample <- 
  gauges_annual_summary %>%
  dplyr::select(gauge_id) %>%
  distinct() %>%
  dplyr::left_join(region_df, by = "gauge_id")

# gauges_annual_for_trends <- gauges_annual_summary

gauges_annual_for_trends <-
  gauges_annual_summary %>% 
  dplyr::mutate(
    p.pet  = dplyr::if_else(is.na(pet) | pet == 0, NA_real_, p / pet),
    p.pet_djf  = dplyr::if_else(is.na(pet_djf) | pet_djf == 0, NA_real_, p_djf / pet_djf),
    p.pet_jjas = dplyr::if_else(is.na(pet_jjas) | pet_jjas == 0, NA_real_, p_jjas / pet_jjas),
    p.pet_mam = dplyr::if_else(is.na(pet_mam) | pet_mam == 0, NA_real_, p_mam / pet_mam),
    p.pet_on = dplyr::if_else(is.na(pet_on) | pet_on == 0, NA_real_, p_on / pet_on)
  )

# FIX 2: Explicitly define the columns to compute trends on (so it skips static data like latitude!)
# target_metrics <- c("Zero_Flow_Days_sum", "hydro_doy", "mean_dry_down_days")

# List ALL columns that should NOT be tested for trends over time.
# This MUST include identifiers, regions, and all STATIC topography variables.
cols_to_exclude <- c(
  "gauge_id", 
  "hydro_year", 
  "spei3_drought_count_frac", 
  "spei6_drought_count_frac", 
  "spei12_drought_count_frac", 
  "spei24_drought_count_frac"
)

# This dynamically finds columns that are NOT in the list AND are numeric!
cols_trend <- which(
  !names(gauges_annual_for_trends) %in% cols_to_exclude & 
    sapply(gauges_annual_for_trends, is.numeric)
)
nvar <- length(cols_trend)

# full year sequence
fulllengthwyears <- tibble(
  hydro_year = seq(min(gauges_annual_for_trends$hydro_year, na.rm=TRUE),
                   max(gauges_annual_for_trends$hydro_year, na.rm=TRUE))
)

sites <- unique(gauges_annual_for_trends$gauge_id)

# split year for Mann-Whitney
mw_yr_split <- 1998


## functions
R2 <- function(sim, obs) {
  if (length(sim) != length(obs)) stop("vectors not the same size")
  return((sum((obs-mean(obs))*(sim-mean(sim)))/
            ((sum((obs-mean(obs))^2)^0.5)*(sum((sim-mean(sim))^2)^0.5)))^2)
}


for (i in seq_along(sites)) {
  
  current <- subset(gauges_annual_for_trends, gauge_id == sites[i])
  
  current[current == -Inf] <- NA
  current[current == Inf] <- NA
  
  site_start <- TRUE
  
  for (col in cols_trend) {
    
    currentcolumnname <- colnames(current)[col]
    
    currentcolumn <- tibble(variable = dplyr::pull(current, col))
    currentcolumn$hydro_year <- current$hydro_year
    
    # ensure full time series
    currentcolumn <- right_join(currentcolumn, fulllengthwyears, by = "hydro_year")
    
    years_data <- sum(is.finite(currentcolumn$variable))
    
    # --------------------------
    # Mann-Whitney Test
    # --------------------------
    group1 <- currentcolumn$variable[currentcolumn$hydro_year <= mw_yr_split]
    group2 <- currentcolumn$variable[currentcolumn$hydro_year > mw_yr_split]
    
    if (sum(is.finite(group1)) > 5 & sum(is.finite(group2)) > 5) {
      mw_test <- wilcox.test(group1, group2)
      mw_p <- mw_test$p.value
    } else {
      mw_p <- NA
    }
    
    # --------------------------
    # TREND CALCULATIONS
    # --------------------------
    if (years_data >= 10) {
      
      i_finite <- which(is.finite(currentcolumn$variable))
      
      # Mann-Kendall
      manken <- rkt::rkt(currentcolumn$hydro_year, currentcolumn$variable)
      
      # Linear regression
      linfit <- lm(variable ~ hydro_year, data = currentcolumn)
      
      # FIX 3: Update Poisson check to look for your specific column names!
      if (currentcolumnname %in% c("Zero_Flow_Days_sum", "hydro_doy")) {
        
        pois <- glm(variable ~ hydro_year,
                    family = poisson(link = "log"),
                    data = currentcolumn)
        
        p_slope <- coef(pois)[2]
        p_r2 <- R2(predict(pois, currentcolumn[i_finite, ]),
                   currentcolumn$variable[i_finite])
        p_p <- summary(pois)$coef[2,4]
        
      } else {
        p_slope <- NA
        p_r2 <- NA
        p_p <- NA
      }
      
      trend <- tibble(
        metric = currentcolumnname,
        mk_tau = manken$tau,
        mk_p = manken$sl[1],
        sen_slope = manken$B,
        lin_slope = coef(linfit)[2],
        lin_r2 = summary(linfit)$r.squared,
        lin_p = summary(linfit)$coefficients[2,4],
        pois_slope = p_slope,
        pois_r2 = p_r2,
        pois_p = p_p,
        mw_p = mw_p,
        mw_meanGroup1 = mean(group1, na.rm = TRUE),
        mw_meanGroup2 = mean(group2, na.rm = TRUE),
        mw_medianGroup1 = median(group1, na.rm = TRUE),
        mw_medianGroup2 = median(group2, na.rm = TRUE),
        n_yrGroup1 = sum(is.finite(group1)),
        n_yrGroup2 = sum(is.finite(group2))
      )
      
    } else {
      
      trend <- tibble(
        metric = currentcolumnname,
        mk_tau = NA, mk_p = NA, sen_slope = NA,
        lin_slope = NA, lin_r2 = NA, lin_p = NA,
        pois_slope = NA, pois_r2 = NA, pois_p = NA,
        mw_p = mw_p,
        mw_meanGroup1 = mean(group1, na.rm = TRUE),
        mw_meanGroup2 = mean(group2, na.rm = TRUE),
        mw_medianGroup1 = median(group1, na.rm = TRUE),
        mw_medianGroup2 = median(group2, na.rm = TRUE),
        n_yrGroup1 = sum(is.finite(group1)),
        n_yrGroup2 = sum(is.finite(group2))
      )
    }
    
    if (site_start) {
      results <- trend
      site_start <- FALSE
    } else {
      results <- bind_rows(results, trend)
    }
  }
  
  results$gauge_id <- sites[i]
  
  if (i == 1) {
    gauge_trends <- results
  } else {
    gauge_trends <- bind_rows(gauge_trends, results)
  }
  
  print(paste0("Site ", i, " complete"))
}

# ==============================
# FINAL FILTERING
# ==============================

# FIX 4: Updated to your specific metric names so it doesn't drop all your data
valid_gauges <- 
  gauges_annual_summary %>%
  subset(is.finite(Zero_Flow_Days_sum) &
           is.finite(hydro_doy) &
           is.finite(mean_dry_down_days)) %>%
  dplyr::pull(gauge_id) %>%
  unique()

gauge_sample_out <- 
  gauge_sample %>%
  subset(gauge_id %in% valid_gauges)

# ==============================
# TRAIN / TEST SPLIT
# ==============================

set.seed(1)
frac_test <- 0.2

test <- 
  gauges_annual_summary %>%
  left_join(gauge_sample_out[,c("gauge_id","region")], by="gauge_id") %>%
  group_by(region) %>%
  sample_frac(frac_test) %>%
  ungroup() %>%
  select(gauge_id, hydro_year) %>%
  mutate(Sample = "Test",
         idyr = paste0(gauge_id, "_", hydro_year))

train <- 
  gauges_annual_summary %>%
  select(gauge_id, hydro_year) %>%
  mutate(Sample = "Train",
         idyr = paste0(gauge_id, "_", hydro_year)) %>%
  subset(!(idyr %in% test$idyr))

gauge_annual_sample <- 
  bind_rows(test, train) %>%
  select(-idyr)

# ==============================
# FINAL DATASET
# ==============================

gauges_annual_summary_out <- 
  left_join(gauges_annual_summary, gauge_annual_sample,
            by = c("gauge_id", "hydro_year")) %>%
  left_join(gauge_sample_out[,c("gauge_id","region")],
            by = "gauge_id")

# ==============================
# SAVE OUTPUTS
# ==============================

# Make sure the "results" folder exists in your working directory!
# if(!dir.exists("results")) dir.create("results")

readr::write_csv(gauge_trends, "results/Mine/gauge_trends.csv")
readr::write_csv(gauges_annual_summary_out, "results/Mine/gauges_annual_summary_out.csv")
