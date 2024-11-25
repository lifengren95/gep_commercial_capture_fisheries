############################################################
# Project: Global GEP: Fisheries-Calculation
# Author: Lifeng Ren
# Date Created: 11/24/2024
# Last Modified: 11/24/2024
# Description: This script with takes in the cleaned csvs and calculate the GEP-fisheries
############################################################

#------------------------------------------------------------
# 1. Setup----
#------------------------------------------------------------

# 1.1 Load Required Libraries
library(dplyr)
library(data.table)

# 1.2 Define Working Directory
setwd("D:/Users/lifengren/Dropbox/400_research/440_UMN/gep/gep_fisheries/scripts")
#setwd("C:/Users/lifengren/Dropbox/400_research/440_UMN/gep/gep_fisheries/scripts")

# Define output directory
output_dir <- "../intermediate/gep_fish/"

# Create the directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Output directory created:", output_dir, "\n")
} else {
  cat("Output directory already exists:", output_dir, "\n")
}

#------------------------------------------------------------
# 2. Data Import
#------------------------------------------------------------

# Import the data using fread
fisheries_value <- fread("../intermediate/fishstatj_cleaned/fishstatj_cleaned.csv")


# Reorder the dataset by specified columns
fisheries_value <- fisheries_value[order(
  iso3,
  year,
  fao_area_code,
  isscaap_group,
  isscaap_division,
  aqua_prod_quant_tlw,
  aqua_prod_value_usd1000,
  capture_prod_quant_tlw,
  domestic_price_usd1000pertonne
)]

# Rearrange the column positions
fisheries_value <- fisheries_value[, .(
  iso3,
  year,
  fao_area_code,
  isscaap_group,
  isscaap_division,
  aqua_prod_quant_tlw,
  aqua_prod_value_usd1000,
  capture_prod_quant_tlw,
  domestic_price_usd1000pertonne,
  aqua_price_usd1000pertlw,
  export_quant_tpw,
  import_quant_tpw,
  export_value_usd1000,
  import_value_usd1000,
  import_price_usd1000pertpw,
  export_price_usd1000pertpw,
  price_aqua_exists,
  price_export_exists,
  price_import_exists,
  num_existing_prices
)]

# Identify rows where the condition is met
fisheries_value[
  (is.na(aqua_prod_value_usd1000) | aqua_prod_value_usd1000 == 0) & 
    aqua_prod_quant_tlw > 0,
  `:=`(
    aqua_prod_value_usd1000 = aqua_prod_quant_tlw * domestic_price_usd1000pertonne,
    aqua_price_usd1000pertlw = domestic_price_usd1000pertonne
  )
]

# Display the updated rows
updated_rows <- fisheries_value[
  (is.na(aqua_prod_value_usd1000) | aqua_prod_value_usd1000 == 0) & 
    aqua_prod_quant_tlw > 0
]

cat("Number of rows updated:", nrow(updated_rows), "\n")


# Ensure fisheries_value is a data.table
setDT(fisheries_value)

# Step 1: Calculate cap_value_usd1000
fisheries_value[, capture_prod_quant_tlw := as.numeric(capture_prod_quant_tlw)]
fisheries_value[, domestic_price_usd1000pertonne := as.numeric(domestic_price_usd1000pertonne)]

fisheries_value[, cap_value_usd1000 := capture_prod_quant_tlw * domestic_price_usd1000pertonne]

# Step 2: Aggregate by iso3 and year
total_values_iso3_year <- fisheries_value[, .(
  total_aqua_value_usd1000 = sum(aqua_prod_value_usd1000, na.rm = TRUE),
  total_cap_value_usd1000 = sum(cap_value_usd1000, na.rm = TRUE)
), by = .(iso3, year)]

# Step 3: Aggregate by iso3, fao_area_code, and year
total_values_iso3_area_year <- fisheries_value[, .(
  total_aqua_value_usd1000 = sum(aqua_prod_value_usd1000, na.rm = TRUE),
  total_cap_value_usd1000 = sum(cap_value_usd1000, na.rm = TRUE)
), by = .(iso3, fao_area_code, year)]

# Step 4: Aggregate by iso3, isscaap_division, and year

# Aggregate by iso3, isscaap_division, and year
total_values_iso3_division_year <- fisheries_value[, .(
  total_aqua_value_usd1000 = sum(aqua_prod_value_usd1000, na.rm = TRUE),
  total_cap_value_usd1000 = sum(cap_value_usd1000, na.rm = TRUE)
), by = .(iso3, isscaap_division, year)]

# Add total_value_usd1000 column
total_values_iso3_year[, total_value_usd1000 := total_aqua_value_usd1000 + total_cap_value_usd1000]

# Add total_value_usd1000 column
total_values_iso3_area_year[, total_value_usd1000 := total_aqua_value_usd1000 + total_cap_value_usd1000]

# Add total_value_usd1000 column
total_values_iso3_division_year[, total_value_usd1000 := total_aqua_value_usd1000 + total_cap_value_usd1000]

###### Using the NatRes_Share from GTAP as the percentage from Nature Contribution

library(readxl)

# Read the first sheet of the Excel file
iso3_gtap141_mapping <- read_excel("../raw_data/gtapv7_65x141/iso3_gtap141_mapping.xlsx", sheet = 1)

# Read the first sheet of another Excel file
fsh_endowment_gtap <- read_excel("../raw_data/gtapv7_65x141/fsh_endowment_gtap.xlsx", sheet = 1)

# Convert to data.table for efficient processing
setDT(iso3_gtap141_mapping)
setDT(fsh_endowment_gtap)

# Ensure consistent case for 'gtap141_code' in both datasets
iso3_gtap141_mapping <- iso3_gtap141_mapping[, gtap141_code := toupper(gtap141_code)]
fsh_endowment_gtap <- fsh_endowment_gtap[, gtap141_code := toupper(sub("^\\d+\\s+", "", gtap141_code))]

# Merge the two datasets by 'gtap141_code'
iso3_natres_share <- merge(
  iso3_gtap141_mapping[, .(iso3, gtap141_code)],
  fsh_endowment_gtap[, .(gtap141_code, NatRes_Share)],
  by = "gtap141_code",
  all.x = TRUE
)

# Check for unmatched rows
unmatched <- iso3_natres_share[is.na(NatRes_Share)]
if (nrow(unmatched) > 0) {
  print("Unmatched rows:")
  print(unmatched)
}

# Check for duplicates in gtap141_code in either dataset
duplicate_check_mapping <- iso3_gtap141_mapping[, .N, by = gtap141_code][N > 1]
duplicate_check_endowment <- fsh_endowment_gtap[, .N, by = gtap141_code][N > 1]

if (nrow(duplicate_check_mapping) > 0) {
  print("Duplicate gtap141_code in iso3_gtap141_mapping:")
  print(duplicate_check_mapping)
}
if (nrow(duplicate_check_endowment) > 0) {
  print("Duplicate gtap141_code in fsh_endowment_gtap:")
  print(duplicate_check_endowment)
}

# Merge the iso3_natres_share with total_values_iso3_year by 'iso3'
gep_fish_iso3_year <- merge(
  total_values_iso3_year,
  iso3_natres_share[, .(iso3, NatRes_Share)],
  by = "iso3",
  all.x = TRUE
)

# Calculate GEP_fish as total_value * NatRes_Share
gep_fish_iso3_year[, GEP_fish := total_value_usd1000 * NatRes_Share]

# For total_values_iso3_area_year
gep_fish_iso3_area_year <- merge(
  total_values_iso3_area_year,
  iso3_natres_share[, .(iso3, NatRes_Share)], # Select relevant columns
  by = "iso3",
  all.x = TRUE # Keep all rows from the main dataset
)

# For total_values_iso3_division_year
gep_fish_iso3_division_year <- merge(
  total_values_iso3_division_year,
  iso3_natres_share[, .(iso3, NatRes_Share)], # Select relevant columns
  by = "iso3",
  all.x = TRUE # Keep all rows from the main dataset
)

# Step 3: Calculate GEP_fish as total_value_usd1000 * NatRes_Share
# For total_values_iso3_area_year
gep_fish_iso3_area_year[, GEP_fish := total_value_usd1000 * NatRes_Share]

# For total_values_iso3_division_year
gep_fish_iso3_division_year[, GEP_fish := total_value_usd1000 * NatRes_Share]



####Save GEP Data####


# Save the gep_fish_iso3_year dataset
fwrite(gep_fish_iso3_year, paste0(output_dir,"gep_fish_iso3_year.csv"))

# Save the gep_fish_iso3_area_year dataset
fwrite(gep_fish_iso3_area_year, paste0(output_dir,"gep_fish_iso3_faoarea_year.csv"))

# Save the gep_fish_iso3_division_year dataset
fwrite(gep_fish_iso3_division_year, paste0(output_dir,"gep_fish_iso3_isscaapdivision_year.csv"))

