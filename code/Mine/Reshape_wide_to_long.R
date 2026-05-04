# install.packages("tidyverse")
# install.packages("lubridate")

# ===========================================================================
# Step 1: Reshaping your Wide Data to Long Data
# ===========================================================================

library(tidyverse)
# library(lubridate)

# Load your datasets
annual_zf_wide <- read_csv("D:/Peninsular India/CAMELS_IND/CAMELS_IND_Catchments_Streamflow_Sufficient3/Output/zero_flow_days_annual_0.005.csv") # Replace with your actual file path
daily_flow_wide <- read_csv("D:/Peninsular India/CAMELS_IND/CAMELS_IND_Catchments_Streamflow_Sufficient3/streamflow_timeseries/streamflow_observed.csv") # Replace with your actual file path
daily_boolean_wide <- read_csv("D:/Peninsular India/CAMELS_IND/CAMELS_IND_Catchments_Streamflow_Sufficient3/Output/zero_flow_days_0.005.csv") # Replace with your actual file path

annual_zf_long <- read_csv("D:/Peninsular India/CAMELS_IND/CAMELS_IND_Catchments_Streamflow_Sufficient3/Output/zero_flow_days_annual_long.csv") # Replace with your desired output path
daily_flow_long <- read_csv("D:/Peninsular India/CAMELS_IND/CAMELS_IND_Catchments_Streamflow_Sufficient3/Output/streamflow_observed_long.csv") # Replace with your desired output path
daily_boolean_long <- read_csv("D:/Peninsular India/CAMELS_IND/CAMELS_IND_Catchments_Streamflow_Sufficient3/Output/zero_flow_days_long.csv") # Replace with your desired output path

first_zf_data <- read_csv("D:/Peninsular India/CAMELS_IND/CAMELS_IND_Catchments_Streamflow_Sufficient3/Output/first_zero_flow_date_per_year_0.005.csv")

monthly_grand_sheet <- read_csv("D:/Peninsular India/Merged_data/Output/Final_Monthly_Master_Dataset_updated_with_drought.csv")

dry_down_annual <- read_csv("D:/Peninsular India/CAMELS_IND/CAMELS_IND_Catchments_Streamflow_Sufficient3/Output/dry_down_annual.csv") # Replace with your desired output path

# 1. Convert Annual Zero-Flow Data 
# Assuming your columns are: hydro_year | 1005 | 1006 | 1007 ...
annual_zf_long <- annual_zf_wide %>%
  pivot_longer(
    cols = -hydro_year, # Pivots all columns EXCEPT 'hydro_year'
    names_to = "gauge_id", 
    values_to = "annual_zero_flow_days"
  )

# 2. Convert Daily Flow Data
# Assuming your columns are: year | month | day | 1005 | 1006 ...
daily_flow_long <- daily_flow_wide %>%
  pivot_longer(
    cols = -c(year, month, day), # Pivots all columns EXCEPT year, month, and day
    names_to = "gauge_id", 
    values_to = "discharge"
  ) %>%
  mutate(date = make_date(year, month, day))


# Convert your daily boolean zero-flow dataset to Long format
daily_boolean_long <- daily_boolean_wide %>%
  pivot_longer(
    cols = -c(hydro_year, hydro_month, date, day, year, month, season), # Exclude all metadata cols
    names_to = "gauge_id",
    values_to = "is_zero_flow" # TRUE, FALSE, or NA
  ) %>%
  mutate(date = dmy(date))

  
# Export the reshaped datasets
write_csv(annual_zf_long, "D:/Peninsular India/CAMELS_IND/CAMELS_IND_Catchments_Streamflow_Sufficient3/Output/zero_flow_days_annual_long.csv") # Replace with your desired output path
write_csv(daily_flow_long, "D:/Peninsular India/CAMELS_IND/CAMELS_IND_Catchments_Streamflow_Sufficient3/Output/streamflow_observed_long.csv") # Replace with your desired output path
write_csv(daily_boolean_long, "D:/Peninsular India/CAMELS_IND/CAMELS_IND_Catchments_Streamflow_Sufficient3/Output/zero_flow_days_long.csv") # Replace with your desired output path




# ===========================================================================
# Step 2: Calculating the Dry-Down Period
# ===========================================================================

# -------------------------
# Good for smaller datasets
# -------------------------

# # Merge flow and boolean data
# daily_data <- left_join(daily_flow_long, daily_boolean_long, by = c("gauge_id", "date"))
# 
# # Calculate Dry-Down Period per gauge and hydro_year
# dry_down_annual <- daily_data %>%
#   arrange(gauge_id, date) %>%
#   group_by(gauge_id) %>%
#   mutate(
#     # Find 25th percentile of long-term mean daily flow for this specific gauge
#     q25_threshold = quantile(discharge, 0.25, na.rm = TRUE),
#     # Identify if a day is a "Peak"
#     is_peak = discharge > q25_threshold
#   ) %>%
#   group_by(gauge_id, hydro_year) %>%
#   mutate(
#     # This logic counts the days from the last 'peak' until a 'TRUE' zero-flow hits
#     last_peak_date = case_when(is_peak ~ date, TRUE ~ NA_Date_),
#     # Fill the peak date downward so non-peak days know when the last peak was
#     last_peak_date = zoo::na.locf(last_peak_date, na.rm = FALSE) 
#   ) %>%
#   filter(is_zero_flow == TRUE & !is.na(last_peak_date)) %>%
#   mutate(dry_down_days = as.numeric(date - last_peak_date)) %>%
#   # Aggregate to get the mean dry-down period for the hydro year
#   summarize(mean_dry_down_days = mean(dry_down_days, na.rm = TRUE), .groups = 'drop')



# Optimized Dry-Down Calculation
dry_down_annual <- daily_data %>%
  arrange(gauge_id, date) %>%
  
  # 1. Calculate threshold per gauge
  group_by(gauge_id) %>%
  mutate(
    q25_threshold = quantile(discharge, 0.25, na.rm = TRUE),
    # Use coalesce to safely handle any NA discharge values
    is_peak = coalesce(discharge > q25_threshold, FALSE) 
  ) %>%
  
  # 2. Roll dates forward using cummax trick
  group_by(gauge_id, hydro_year) %>%
  mutate(
    # If it's a peak, save the date as an integer. If not, make it 0.
    peak_date_num = if_else(is_peak, as.integer(date), 0L),
    
    # cummax() instantly carries the highest number (latest date) forward!
    last_peak_num = cummax(peak_date_num),
    
    # Convert any 0s (days before the first peak of the year) back to NA
    last_peak_num = na_if(last_peak_num, 0L)
  ) %>%
  
  # 3. Filter and calculate (only keep actual zero-flow days)
  filter(is_zero_flow == TRUE & !is.na(last_peak_num)) %>%
  mutate(
    # Math is extremely fast on integers
    dry_down_days = as.integer(date) - last_peak_num
  ) %>%
  
  # 4. Aggregate to annual
  summarize(
    mean_dry_down_days = mean(dry_down_days, na.rm = TRUE), 
    .groups = 'drop'
  )

write_csv(dry_down_annual, "D:/Peninsular India/CAMELS_IND/CAMELS_IND_Catchments_Streamflow_Sufficient3/Output/dry_down_annual.csv") # Replace with your desired output path


# ===========================================================================
# Step 3: No-Flow Timing (Hydro Day of Year)
# ===========================================================================

# Assuming your timing dataframe is called `first_zf_data`
first_zf_data <- first_zf_data %>%
  mutate(
    # Convert '3rd May 1980' to actual Date object. 
    # dmy() automatically handles things like "3rd May 1980"
    actual_date = dmy(first_zero_date), 
    
    # Calculate the start date of the hydrological year.
    # If hydro_year is 1980, it started on June 1, 1979.
    hydro_start_date = make_date(hydro_year - 1, 6, 1),
    
    # Calculate difference in days (adding 1 so June 1st = Day 1)
    hydro_doy = as.numeric(actual_date - hydro_start_date) + 1
  ) %>%
  select(gauge_id, hydro_year, first_zero_date, hydro_doy)

write_csv(first_zf_data, "D:/Peninsular India/CAMELS_IND/CAMELS_IND_Catchments_Streamflow_Sufficient3/Output/first_zero_flow_date_per_year_long.csv") # Replace with your desired output path



# ===========================================================================
# Step 4: Aggregating the Monthly Hydro-Climate Grand Sheet
# ===========================================================================


# 1. Identify your static columns
# Put the exact names of your static columns here
vars_static <- c('ai_mean', 'geol_porosity',
                 'geol_permeability', 'geol_class_1st_frac', 'carb_rocks_frac',
                 'lai_mean', 'lai_min', 'lai_max', 'lai_diff',
                 'soil_depth', 'soil_conductivity_top', 'soil_conductivity_sub',
                 'soil_awc_top', 'soil_awc_sub', 'soil_awsc_min', 'soil_awsc_max',
                 'soil_awsc_major', 'sand_frac_top', 'sand_frac_sub', 'silt_frac_top',
                 'silt_frac_sub', 'clay_frac_top', 'clay_frac_sub', 'gravel_frac_top',
                 'gravel_frac_sub', 'bulkdens_top_major', 'bulkdense_top_mean',
                 'bulkdens_sub_mean', 'org_carb_top_major', 'org_carb_top_mean',
                 'org_carb_sub_major', 'org_carb_sub_mean', 'organic_frac_top',
                 'organic_frac_sub', 'cwc_lat', 'cwc_lon', 'ghi_lat', 'ghi_lon',
                 'elev_mean', 'elev_median', 'elev_min', 'elev_max', 'slope_mean',
                 'slope_median', 'slope_min', 'slope_max', 'cwc_area',
                 'dpsbar', 'sinuosity') # Add any others you have!

# 2. Extract the static data into its own table
static_topo_data <- monthly_grand_sheet %>%
  select(gauge_id, all_of(vars_static)) %>%
  distinct() # This brilliantly shrinks the millions of rows down to just ONE row per gauge_id!

# 3. Now, aggregate ONLY the dynamic climate variables (Same as Step 4)
# , 'spei3', 'spei6',
# 'spei12', 'spei24', , , 


# 1. Define your two lists of column names
vars_to_sum <- c('Zero_Flow_Days', 'prcp(mm/day)', 'pet(mm/day)', 'aet_gleam(mm/day)',
                 'evap_canopy(mm/day)', 'evap_surface(mm/day)')
vars_to_mean <- c('tmax(C)', 'tmin(C)', 'tavg(C)', 'srad_lw(w/m2)', 'srad_sw(w/m2)', 'rel_hum(%)',
                  'sm_lvl1(kg/m2)', 'sm_lvl2(kg/m2)', 'sm_lvl3(kg/m2)', 'sm_lvl4(kg/m2)') # Add any other rate/average variables here
vars_already_annual <- c('urban_pct', 'Total_Dams_So_Far',
                         'Total_Volume_So_Far')

longest_run <- function(x) {
  x <- ifelse(is.na(x), 0, x)  # treat NA as non-drought
  r <- rle(x)
  if (!any(r$values == 1)) return(0)
  max(r$lengths[r$values == 1])
}

# 2. Aggregate the climate/grand sheet data
annual_climate_data <- monthly_grand_sheet %>%
  group_by(gauge_id, hydro_year) %>%
  summarize(
    # Sum the monthly variables
    across(all_of(vars_to_sum), ~sum(.x, na.rm = TRUE), .names = "{.col}_sum"),
    
    # Average the monthly variables
    across(all_of(vars_to_mean), ~mean(.x, na.rm = TRUE), .names = "{.col}_mean"),
    
    # Just grab the first value for variables that are already annual!
    across(all_of(vars_already_annual), ~first(.x), .names = "{.col}"),
    
    # =====================================================
    # 🔴 SPEI DROUGHT METRICS
    # =====================================================
    
    # 1. Drought counts (number of drought months)
    across(all_of(paste0(spei_cols, "_drought")),
           ~sum(.x, na.rm = TRUE),
           .names = "{.col}_count"),
    
    # 2. Total valid months (for fraction)
    across(all_of(paste0(spei_cols, "_drought")),
           ~sum(!is.na(.x)),
           .names = "{.col}_valid_n"),
    
    # 3. Longest drought spell
    across(all_of(paste0(spei_cols, "_drought")),
           longest_run,
           .names = "{.col}_max_spell"),
    
    
    .groups = 'drop'
  ) %>%
  
  # =====================================================
  # 🟢 Post-processing: drought fraction
  # =====================================================
  mutate(
    across(ends_with("_drought_count"),
         ~ .x / get(sub("_count", "_valid_n", cur_column())),
         .names = "{.col}_frac")
)




master_annual_df <- annual_zf_long %>%
  left_join(dry_down_annual, by = c("gauge_id", "hydro_year")) %>%
  left_join(first_zf_data, by = c("gauge_id", "hydro_year")) %>%
  left_join(annual_climate_data, by = c("gauge_id", "hydro_year")) %>%
  left_join(static_topo_data, by = "gauge_id") # Joins your freshly extracted static data!

write_csv(master_annual_df, "D:/Peninsular India/Merged_data/Output/Final_Annual_Master_Dataset.csv") # Replace with your desired output path

