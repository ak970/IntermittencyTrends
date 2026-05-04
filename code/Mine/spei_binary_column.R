# Step 2: SPEI binary values

df <- read_csv("D:/Peninsular India/Merged_data/Output/Final_Monthly_Master_Dataset_updated.csv")



spei_cols <- c("spei3", "spei6", "spei12", "spei24")


for (col in spei_cols) {
  new_col <- paste0(col, "_drought")
  df[[new_col]] <- ifelse(is.na(df[[col]]), NA,
                          ifelse(df[[col]] < -1, 1, 0))
}



write_csv(df, "D:/Peninsular India/Merged_data/Output/Final_Monthly_Master_Dataset_updated_with_drought.csv")
