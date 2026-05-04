# ==============================
# FILTERED CODE FROM 00a_SelectgaugesForAnalysis.R
# Only contains the code for trend analysis and final dataset creation
# ==============================
# install.packages("rkt")
# install.packages("dplyr")
# install.packages("tibble")
# install.packages("Metrics")

getwd()

source(file.path("code", "paths+packages.R"))

library(dplyr)
library(tibble)
library(tidyverse)
library(Metrics)

gauges_annual_summary <- read_csv("D:/Peninsular India/Merged_data/Output/Data_to_use/Final_Annual_Master_Dataset.csv")
region_df <- readr::read_csv("D:/Peninsular India/Merged_data/Output/Data_to_use/station_ai_regions.csv")

gauges_annual_summary <- gauges_annual_summary %>%
  mutate(across(everything(), ~na_if(., "NA")))

gauge_sample <- 
  gauges_annual_summary %>%
  dplyr::select(gauge_id) %>%
  distinct() %>%
  dplyr::left_join(region_df, by = "gauge_id")


# # must contain: gauge_id, region
# gauge_sample <- 
#   gauge_sample %>%
#   dplyr::left_join(region_df, by = "gauge_id")

# ensure required columns exist
gauges_annual_for_trends <- gauges_annual_summary

# columns to compute trends (exclude id + year)
cols_trend <- which(!names(gauges_annual_for_trends) %in% c("gauge_id", "hydro_year", "spei3_drought_count_frac", "spei6_drought_count_frac", "spei12_drought_count_frac", "spei24_drought_count_frac"))
nvar <- length(cols_trend)

# full year sequence
fulllengthwyears <- tibble(
  hydro_year = seq(min(gauges_annual_for_trends$hydro_year),
                        max(gauges_annual_for_trends$hydro_year))
)

sites <- unique(gauges_annual_for_trends$gauge_id)

# split year for Mann-Whitney
mw_yr_split <- 1998

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
      
      # Poisson regression (only for count variables)
      if (currentcolumnname %in% c("annual_zero_flow_days", "hydro_doy"
                                   )) {
        
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
        mk_tau = NA,
        mk_p = NA,
        sen_slope = NA,
        lin_slope = NA,
        lin_r2 = NA,
        lin_p = NA,
        pois_slope = NA,
        pois_r2 = NA,
        pois_p = NA,
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

valid_gauges <- 
  gauges_annual_summary %>%
  subset(is.finite(annual_zero_flow_days) &
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

readr::write_csv(gauge_trends,
                 "results/gauge_trends.csv")

readr::write_csv(gauges_annual_summary_out,
                 "results/gauges_annual_summary_out.csv")