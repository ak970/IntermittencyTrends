# ==============================
# FILTERED CODE created independently
# ==============================

# install.packages("trend")

library(tidyverse)
library(trend) # Contains the mk.test and sens.slope functions


sampleAnnual_grand_sheet <- read_csv("D:/Peninsular India/Merged_data/Output/Data_to_use/Final_Annual_Master_Dataset.csv")


# 1. Prepare the dataset and pivot your three Target Variables
trend_prep <- sampleAnnual %>%
  # Keep only the ID, time, and target variables
  select(gauge_id, hydro_year, Zero_Flow_Days_sum, hydro_doy, mean_dry_down_days) %>%
  
  # Pivot to long format so we can calculate trends for all 3 metrics simultaneously!
  pivot_longer(
    cols = c(Zero_Flow_Days_sum, hydro_doy, mean_dry_down_days),
    names_to = "target_variable",
    values_to = "metric_value"
  ) %>%
  
  # Drop NAs (For example, years where the stream never dried, so dry_down is NA)
  filter(!is.na(metric_value)) %>%
  
  # Ensure the data is strictly ordered by year for the time-series test
  arrange(gauge_id, target_variable, hydro_year)

# 2. Run the Mann-Kendall and Sen's Slope Tests
trend_results <- trend_prep %>%
  group_by(gauge_id, target_variable) %>%
  summarize(
    n_years_valid = n(), # Count how many valid years of data exist for this metric
    
    # Run the tests ONLY if there are at least 10 valid years
    mk_tau = if(n_years_valid >= 10) mk.test(metric_value)$estimates["tau"] else NA_real_,
    mk_pvalue = if(n_years_valid >= 10) mk.test(metric_value)$p.value else NA_real_,
    sens_slope = if(n_years_valid >= 10) sens.slope(metric_value)$estimates else NA_real_,
    
    .groups = 'drop'
  ) %>%
  
  # 3. Categorize the trends based on significance (p < 0.05)
  mutate(
    trend_direction = case_when(
      is.na(mk_pvalue) ~ "Insufficient Data",
      mk_pvalue < 0.05 & sens_slope > 0 ~ "Increasing",
      mk_pvalue < 0.05 & sens_slope < 0 ~ "Decreasing",
      TRUE ~ "No Significant Trend"
    )
  )

# 4. Merge with your regions file to see regional patterns!
final_trend_results <- trend_results %>%
  left_join(gageregions, by = "gauge_id") # Replace 'gageregions' with your region dataframe name

# View the summary of your findings!
table(final_trend_results$target_variable, final_trend_results$trend_direction)