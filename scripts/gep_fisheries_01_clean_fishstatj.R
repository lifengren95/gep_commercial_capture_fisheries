############################################################
# Project: Global GEP: Fisheries-Data Preparation
# Author: Lifeng Ren
# Date Created: 11/22/2024
# Last Modified: 11/22/2024
# Description: This script with takes in the csvs saved from the FishStatj platform
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
output_dir <- "../intermediate/fishstatj_cleaned/"

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

# List of datasets to process

# 2.1 Load Data
# List of file paths for all datasets
file_paths <- c(
  "../raw_data/fishstatj/capture_prod_quant.csv",
  "../raw_data/fishstatj/aqua_prod_quant.csv",
  "../raw_data/fishstatj/aqua_prod_value.csv",
  "../raw_data/fishstatj/trade_quant.csv",
  "../raw_data/fishstatj/trade_value.csv"
)

# List of variable names for the datasets
dataset_names <- c(
  "capture_prod_quant",
  "aqua_prod_quant",
  "aqua_prod_value",
  "trade_quant",
  "trade_value"
)

# Initialize an empty list to store the processed datasets
processed_data <- list()

#------------------------------------------------------------
# 3. Data Cleaning and Preprocessing
#------------------------------------------------------------


# 3.1 Clean Data

# For loop to read, clean, and store datasets
for (i in seq_along(file_paths)) {
  # Read the data
  data <- fread(file_paths[i], fill = TRUE)
  
  # Identify and drop columns matching "S", "S.1", ..., "S.24"
  columns_to_drop <- grep("^S(\\.\\d+)?$", colnames(data))
  data <- data[, -columns_to_drop, with = FALSE]
  
  # Remove square brackets from year column names
  colnames(data) <- gsub("\\[(\\d{4})\\]", "\\1", colnames(data))
  
  # Store the processed dataset in the list with its name
  processed_data[[dataset_names[i]]] <- data
}

# Assign processed datasets back to individual variables
capture_prod_quant <- processed_data[["capture_prod_quant"]]
aqua_prod_quant <- processed_data[["aqua_prod_quant"]]
aqua_prod_value <- processed_data[["aqua_prod_value"]]
trade_quant <- processed_data[["trade_quant"]]
trade_value <- processed_data[["trade_value"]]

rm(data, processed_data)

# Clean Column Names
# Rename columns for capture_prod_quant
setnames(capture_prod_quant, old = c(
  "Country (ISO3 code)", 
  "ASFIS species (Code)", 
  "FAO major fishing area (Code)", 
  "Unit (Short Name)"
), new = c(
  "iso3", 
  "isscaap_group", 
  "fao_area_code", 
  "unit_short"
))

# Rename columns for aqua_prod_quant
setnames(aqua_prod_quant, old = c(
  "Country (ISO3 code)", 
  "ASFIS species (Code)", 
  "FAO major fishing area (Code)", 
  "Environment (2-alpha code)", 
  "Unit (Short Name)"
), new = c(
  "iso3", 
  "isscaap_group", 
  "fao_area_code", 
  "env_code", 
  "unit_short"
))

# Rename columns for aqua_prod_value
setnames(aqua_prod_value, old = c(
  "Country (ISO3 code)", 
  "ASFIS species (Code)", 
  "FAO major fishing area (Code)", 
  "Environment (2-alpha code)", 
  "Unit (Short Name)"
), new = c(
  "iso3", 
  "isscaap_group", 
  "fao_area_code", 
  "env_code", 
  "unit_short"
))
# Rename columns for trade_quant
setnames(trade_quant, old = c(
  "Reporting country (ISO3 code)", 
  "Commodity (Code)", 
  "Trade flow (Name)", 
  "Unit (Short Name)"
), new = c(
  "iso3", 
  "isscaap_group", 
  "trade_flow", 
  "unit_short"
))

# Rename columns for trade_value
setnames(trade_value, old = c(
  "Reporting country (ISO3 code)", 
  "Commodity (Code)", 
  "Trade flow (Name)", 
  "Unit (Short Name)"
), new = c(
  "iso3", 
  "isscaap_group", 
  "trade_flow", 
  "unit_short"
))

# Drop the "Unit" column for each dataset
capture_prod_quant <- capture_prod_quant[, !"Unit", with = FALSE]
aqua_prod_quant <- aqua_prod_quant[, !"Unit", with = FALSE]
aqua_prod_value <- aqua_prod_value[, !"Unit", with = FALSE]
trade_quant <- trade_quant[, !"Unit", with = FALSE]
trade_value <- trade_value[, !"Unit", with = FALSE]

# Define a list of datasets
datasets <- list(
  "aqua_prod_quant" = aqua_prod_quant,
  "aqua_prod_value" = aqua_prod_value,
  "capture_prod_quant" = capture_prod_quant,
  "trade_quant" = trade_quant,
  "trade_value" = trade_value
)

# Process each dataset to retain only columns >= 1984
for (name in names(datasets)) {
  # Access the dataset
  data <- datasets[[name]]
  
  # Identify columns with years >= 1984
  year_cols <- grep("^\\d{4}$", colnames(data)) # Match columns with year format
  year_cols_to_keep <- year_cols[as.numeric(colnames(data)[year_cols]) >= 1984]
  
  # Keep only columns >= 1984 and non-year columns
  datasets[[name]] <- data[, c(setdiff(colnames(data), colnames(data)[year_cols]), colnames(data)[year_cols_to_keep]), with = FALSE]
}

# Assign processed datasets back to their original variables
aqua_prod_quant <- datasets[["aqua_prod_quant"]]
aqua_prod_value <- datasets[["aqua_prod_value"]]
capture_prod_quant <- datasets[["capture_prod_quant"]]
trade_quant <- datasets[["trade_quant"]]
trade_value <- datasets[["trade_value"]]

# Re-encode iso3 to UTF-8 for all datasets
capture_prod_quant[, iso3 := iconv(as.character(iso3), from = "", to = "UTF-8", sub = "byte")]
aqua_prod_quant[, iso3 := iconv(as.character(iso3), from = "", to = "UTF-8", sub = "byte")]
aqua_prod_value[, iso3 := iconv(as.character(iso3), from = "", to = "UTF-8", sub = "byte")]
trade_quant[, iso3 := iconv(as.character(iso3), from = "", to = "UTF-8", sub = "byte")]
trade_value[, iso3 := iconv(as.character(iso3), from = "", to = "UTF-8", sub = "byte")]

# Updated filter_iso3 function
filter_iso3 <- function(data) {
  iso3_lengths <- nchar(data$iso3, allowNA = TRUE)
  data[!is.na(iso3_lengths) & iso3_lengths == 3]
}

# Apply the function to each dataset
capture_prod_quant <- filter_iso3(capture_prod_quant)
aqua_prod_quant <- filter_iso3(aqua_prod_quant)
aqua_prod_value <- filter_iso3(aqua_prod_value)
trade_quant <- filter_iso3(trade_quant)
trade_value <- filter_iso3(trade_value)

# Reshape Dataset into Panel

## aqua_prod_quant

aqua_prod_quant[, c('env_code', 'unit_short') := NULL]

# Reshape from wide to long format
aqua_prod_quant <- melt(
  aqua_prod_quant,
  id.vars = c('iso3', 'isscaap_group', 'fao_area_code'),
  variable.name = 'year',
  value.name = 'aqua_prod_quant_tlw'
)

# Convert 'year' to numeric
aqua_prod_quant[, year := as.numeric(as.character(year))]

## aqua_prod_value

# Drop 'env_code' and 'unit_short' columns
aqua_prod_value[, c('env_code', 'unit_short') := NULL]

# Reshape from wide to long format
aqua_prod_value <- melt(
  aqua_prod_value,
  id.vars = c('iso3', 'isscaap_group', 'fao_area_code'),
  variable.name = 'year',
  value.name = 'aqua_prod_value_usd1000'
)

# Convert 'year' to numeric
aqua_prod_value[, year := as.numeric(as.character(year))]

## capture_prod_quant

# Drop 'env_code' and 'unit_short' columns
capture_prod_quant[, c('unit_short') := NULL]

# Reshape from wide to long format
capture_prod_quant <- melt(
  capture_prod_quant,
  id.vars = c('iso3', 'isscaap_group', 'fao_area_code'),
  variable.name = 'year',
  value.name = 'capture_prod_quant_tlw'
)

# Convert 'year' to numeric
capture_prod_quant[, year := as.numeric(as.character(year))]


## trade_quant

# Get the names of the year columns (assuming they are four-digit numbers)
year_cols <- grep("^\\d{4}$", names(trade_quant), value = TRUE)
# Check the data types of the year columns
sapply(trade_quant[, ..year_cols], class)
# Convert year columns to numeric
trade_quant[, (year_cols) := lapply(.SD, as.numeric), .SDcols = year_cols]
# Verify the data types
sapply(trade_quant[, ..year_cols], class)


# Reshape from wide to long format
trade_quant <- melt(
  trade_quant,
  id.vars = c('iso3', 'isscaap_group', 'trade_flow'),
  measure.vars = year_cols,
  variable.name = 'year',
  value.name = 'trade_quant_tpw'
)

# Convert 'year' to numeric
trade_quant[, year := as.numeric(as.character(year))]


## trade_value

# Get the names of the year columns (assuming they are four-digit numbers)
year_cols <- grep("^\\d{4}$", names(trade_value), value = TRUE)
# Check the data types of the year columns
sapply(trade_value[, ..year_cols], class)
# Convert year columns to numeric
trade_value[, (year_cols) := lapply(.SD, as.numeric), .SDcols = year_cols]
# Verify the data types
sapply(trade_value[, ..year_cols], class)


# Reshape from wide to long format
trade_value <- melt(
  trade_value,
  id.vars = c('iso3', 'isscaap_group', 'trade_flow'),
  measure.vars = year_cols,
  variable.name = 'year',
  value.name = 'trade_value_usd1000'
)

# Convert 'year' to numeric
trade_value[, year := as.numeric(as.character(year))]

# Merge all data

## Production first
# Ensure all datasets are data.tables (if not already)
setDT(aqua_prod_quant)
setDT(aqua_prod_value)
setDT(capture_prod_quant)

# Check and unify data types for key columns
# For 'isscaap_group' and 'fao_area_code', ensure they are character
aqua_prod_quant[, `:=`(
  isscaap_group = as.character(isscaap_group),
  fao_area_code = as.character(fao_area_code)
)]
aqua_prod_value[, `:=`(
  isscaap_group = as.character(isscaap_group),
  fao_area_code = as.character(fao_area_code)
)]
capture_prod_quant[, `:=`(
  isscaap_group = as.character(isscaap_group),
  fao_area_code = as.character(fao_area_code)
)]

# Merge aqua_prod_quant and aqua_prod_value first
prod_data <- merge(
  aqua_prod_quant, 
  aqua_prod_value, 
  by = c('iso3', 'isscaap_group', 'fao_area_code', 'year'), 
  all = TRUE
)

# Now merge the result with capture_prod_quant
prod_data <- merge(
  prod_data, 
  capture_prod_quant, 
  by = c('iso3', 'isscaap_group', 'fao_area_code', 'year'), 
  all = TRUE
)


# Trade Data Merge

# Ensure both datasets are data.tables (if not already)
setDT(trade_quant)
setDT(trade_value)

# Convert key columns to the same data type
trade_quant[, `:=`(
  iso3 = as.character(iso3),
  isscaap_group = as.character(isscaap_group),
  trade_flow = as.character(trade_flow),
  year = as.numeric(year)
)]

trade_value[, `:=`(
  iso3 = as.character(iso3),
  isscaap_group = as.character(isscaap_group),
  trade_flow = as.character(trade_flow),
  year = as.numeric(year)
)]

# Function to check for duplicates
check_duplicates <- function(data, keys) {
  dup_rows <- data[duplicated(data, by = keys), ]
  if (nrow(dup_rows) > 0) {
    cat("Duplicates found:\n")
    print(dup_rows)
  } else {
    cat("No duplicates found.\n")
  }
}

# Define the key columns
key_cols <- c('iso3', 'isscaap_group', 'trade_flow', 'year')

# Check for duplicates in trade_quant
check_duplicates(trade_quant, key_cols)

# Check for duplicates in trade_value
check_duplicates(trade_value, key_cols)

# Merge trade_quant and trade_value
trade_data <- merge(
  trade_quant,
  trade_value,
  by = key_cols,
  all = TRUE  # Performs a full outer join
)

# Convert to lowercase and remove plural 's' for consistency
trade_data[, trade_flow := tolower(trade_flow)]
trade_data[, trade_flow := sub("s$", "", trade_flow)]

# Verify unique trade_flow values
unique(trade_data$trade_flow)

# Reshape the data
trade_data <- dcast(
  trade_data,
  iso3 + isscaap_group + year ~ trade_flow,
  value.var = c("trade_quant_tpw", "trade_value_usd1000"),
  sep = "_"
)

# Rename columns
setnames(
  trade_data,
  old = c(
    "trade_quant_tpw_exports (incl. reexports)",
    "trade_quant_tpw_import",
    "trade_value_usd1000_exports (incl. reexports)",
    "trade_value_usd1000_import"
  ),
  new = c(
    "export_quant_tpw",
    "import_quant_tpw",
    "export_value_usd1000",
    "import_value_usd1000"
  )
)


# Price Calculation for trade and production

## Trade Price Calculation

# Define a function to calculate price with custom rules
calculate_price <- function(value, quant) {
  result <- rep(NA_real_, length(value))  # Initialize result with NA of numeric type
  
  # Identify valid indices where both value and quant are not NA and quant is not zero
  idx_valid <- !is.na(value) & !is.na(quant) & quant != 0
  
  # Case: 0 divided by anything (excluding 0 and NA) = 0
  idx_zero_numerator <- idx_valid & value == 0
  result[idx_zero_numerator] <- 0
  
  # Case: Non-zero numerator and valid denominator
  idx_nonzero_numerator <- idx_valid & value != 0
  result[idx_nonzero_numerator] <- value[idx_nonzero_numerator] / quant[idx_nonzero_numerator]
  
  # Cases where result remains NA:
  # - Anything divided by NA = NA
  # - Anything divided by 0 = NA
  # - NA divided by Anything = NA
  # The result is already initialized with NA, so no action needed
  
  return(result)
}

# Apply the function to calculate import price
trade_data[, import_price_usd1000pertpw := calculate_price(import_value_usd1000, import_quant_tpw)]

# Apply the function to calculate export price
trade_data[, export_price_usd1000pertpw := calculate_price(export_value_usd1000, export_quant_tpw)]

# Apply the function to calculate aqua price
prod_data[, aqua_price_usd1000pertlw := calculate_price(aqua_prod_value_usd1000, aqua_prod_quant_tlw)]


# Define the key columns
key_cols_trade <- c("iso3", "isscaap_group", "year")

# Check for duplicates
duplicate_trade <- trade_data[duplicated(trade_data, by = key_cols_trade)]

if (nrow(duplicate_trade) > 0) {
  cat("Duplicates found in trade_data:\n")
  print(duplicate_trade)

  # Optionally, aggregate duplicates by summing the trade quantities and values
  trade_data <- trade_data[, .(
    export_quant_tpw = sum(export_quant_tpw, na.rm = TRUE),
    import_quant_tpw = sum(import_quant_tpw, na.rm = TRUE),
    export_value_usd1000 = sum(export_value_usd1000, na.rm = TRUE),
    import_value_usd1000 = sum(import_value_usd1000, na.rm = TRUE),
    export_price_usd1000pertpw = mean(export_price_usd1000pertpw, na.rm = TRUE),
    import_price_usd1000pertpw = mean(import_price_usd1000pertpw, na.rm = TRUE)
  ), by = key_cols_trade]

  cat("Duplicates have been aggregated.\n")
} else {
  cat("No duplicates found in trade_data.\n")
}


# Define the key columns for merging
key_cols_merge <- c("iso3", "isscaap_group", "year")

# Perform the merge
merged_data <- merge(
  prod_data,
  trade_data,
  by = key_cols_merge,
  all.x = TRUE  # Left join to keep all records from prod_data
)

################################################################################

# # Aquaculture Price Fixing
# 
# # Filter rows where aqua_prod_quant_tlw is not 0 and aqua_prod_value_usd1000 is 0
# aqua_issue_data <- merged_data[
#   aqua_prod_quant_tlw != 0 & aqua_prod_value_usd1000 == 0
# ]

################################################################################


# Domestic Price Interpolation

# This Domestic Price could be used for the Capture Price and for those Aquaculture Price is 0


# Ensure merged_data is a data.table
setDT(merged_data)

# Initialize domestic_price_usd1000pertonne with NA
merged_data[, domestic_price_usd1000pertonne := NA_real_]

# Create logical vectors to check which prices exist (>0)
merged_data[, price_aqua_exists := !is.na(aqua_price_usd1000pertlw) & aqua_price_usd1000pertlw > 0]
merged_data[, price_export_exists := !is.na(export_price_usd1000pertpw) & export_price_usd1000pertpw > 0]
merged_data[, price_import_exists := !is.na(import_price_usd1000pertpw) & import_price_usd1000pertpw > 0]

# Count the number of existing prices
merged_data[, num_existing_prices := price_aqua_exists + price_export_exists + price_import_exists]


# Case 1: Only One Price Exists

merged_data[
  price_aqua_exists & !price_export_exists & !price_import_exists,
  domestic_price_usd1000pertonne := aqua_price_usd1000pertlw
]

merged_data[
  !price_aqua_exists & price_export_exists & !price_import_exists,
  domestic_price_usd1000pertonne := export_price_usd1000pertpw
]

merged_data[
  !price_aqua_exists & !price_export_exists & price_import_exists,
  domestic_price_usd1000pertonne := import_price_usd1000pertpw
]

missing_after_case1 <- merged_data[is.na(domestic_price_usd1000pertonne)]
cat("Number of missing domestic_price_usd1000pertonne after Case 1:", nrow(missing_after_case1), "\n")


# Case 2: Two Prices Exist

# Case 2a: Aquaculture and export prices exist (TRUE, TRUE, FALSE)
merged_data[
  price_aqua_exists & price_export_exists & !price_import_exists,
  domestic_price_usd1000pertonne := aqua_price_usd1000pertlw
]

# Case 2b: Aquaculture and import prices exist (TRUE, FALSE, TRUE)
merged_data[
  price_aqua_exists & !price_export_exists & price_import_exists,
  domestic_price_usd1000pertonne := aqua_price_usd1000pertlw
]

# Case 2c: Export and import prices exist (FALSE, TRUE, TRUE)
merged_data[
  !price_aqua_exists & price_export_exists & price_import_exists & (import_quant_tpw + export_quant_tpw) > 0,
  domestic_price_usd1000pertonne := (
    (import_quant_tpw * import_price_usd1000pertpw) +
      (export_quant_tpw * export_price_usd1000pertpw)
  ) / (import_quant_tpw + export_quant_tpw)
]

# Handle division by zero or infinite values
merged_data[
  !price_aqua_exists & price_export_exists & price_import_exists & (import_quant_tpw + export_quant_tpw) > 0 & is.infinite(domestic_price_usd1000pertonne),
  domestic_price_usd1000pertonne := NA_real_
]

# Show missing values after Case 2
missing_after_case2 <- merged_data[is.na(domestic_price_usd1000pertonne)]
cat("Number of missing domestic_price_usd1000pertonne after Case 2:", nrow(missing_after_case2), "\n")

# Case 3: All Prices Exist (TRUE, TRUE, TRUE)

merged_data[
price_aqua_exists & price_export_exists & price_import_exists,
domestic_price_usd1000pertonne := aqua_price_usd1000pertlw
]

missing_after_case3 <- merged_data[is.na(domestic_price_usd1000pertonne)]
cat("Number of missing domestic_price_usd1000pertonne after Case 3:", nrow(missing_after_case3), "\n")

# Step 3.1: Use Mean Domestic Price for iso3, isscaap_group, and year

# Calculate mean domestic price
mean_domestic_price_by_iso3_species_year <- merged_data[
  !is.na(domestic_price_usd1000pertonne),
  .(mean_domestic_price_iso3_species_year = mean(domestic_price_usd1000pertonne, na.rm = TRUE)),
  by = .(iso3, isscaap_group, year)
]

# Merge and fill missing values
merged_data <- merge(
  merged_data,
  mean_domestic_price_by_iso3_species_year,
  by = c("iso3", "isscaap_group", "year"),
  all.x = TRUE
)

merged_data[
  is.na(domestic_price_usd1000pertonne),
  domestic_price_usd1000pertonne := mean_domestic_price_iso3_species_year
]

# Remove temporary column
merged_data[, mean_domestic_price_iso3_species_year := NULL]

# Display missing values after Step 3.1
missing_after_step3_1 <- merged_data[is.na(domestic_price_usd1000pertonne)]
cat("Number of missing domestic_price_usd1000pertonne after Step 3.1:", nrow(missing_after_step3_1), "\n")


# Step 3.2: Use Mean Domestic Price from Previous and Next Years

# Sort data
setorder(merged_data, iso3, isscaap_group, fao_area_code, year)

# Create lag and lead domestic prices
merged_data[, domestic_price_lag1 := shift(domestic_price_usd1000pertonne, type = "lag"), by = .(iso3, isscaap_group, fao_area_code)]
merged_data[, domestic_price_lead1 := shift(domestic_price_usd1000pertonne, type = "lead"), by = .(iso3, isscaap_group, fao_area_code)]

# Fill missing domestic prices
merged_data[
  is.na(domestic_price_usd1000pertonne),
  domestic_price_usd1000pertonne := rowMeans(.SD, na.rm = TRUE),
  .SDcols = c("domestic_price_lag1", "domestic_price_lead1")
]

# Remove temporary columns
merged_data[, c("domestic_price_lag1", "domestic_price_lead1") := NULL]

# Display missing values after Step 3.2
missing_after_step3_2 <- merged_data[is.na(domestic_price_usd1000pertonne)]
cat("Number of missing domestic_price_usd1000pertonne after Step 3.2:", nrow(missing_after_step3_2), "\n")


# Step 3.3: Use Mean Domestic Price for isscaap_group, fao_area_code, and year Across All iso3

# Calculate mean domestic price
mean_domestic_price_by_species_area_year <- merged_data[
  !is.na(domestic_price_usd1000pertonne),
  .(mean_domestic_price_species_area_year = mean(domestic_price_usd1000pertonne, na.rm = TRUE)),
  by = .(isscaap_group, fao_area_code, year)
]

# Merge and fill missing values
merged_data <- merge(
  merged_data,
  mean_domestic_price_by_species_area_year,
  by = c("isscaap_group", "fao_area_code", "year"),
  all.x = TRUE
)

merged_data[
  is.na(domestic_price_usd1000pertonne),
  domestic_price_usd1000pertonne := mean_domestic_price_species_area_year
]

# Remove temporary column
merged_data[, mean_domestic_price_species_area_year := NULL]

# Display missing values after Step 3.3
missing_after_step3_3 <- merged_data[is.na(domestic_price_usd1000pertonne)]
cat("Number of missing domestic_price_usd1000pertonne after Step 3.3:", nrow(missing_after_step3_3), "\n")


### New Step: Interpolate Using Division-Level Mean

#### Create `isscaap_division` Column

# (Include the code to create `isscaap_division` as shown earlier)

# Create a mapping vector
isscaap_division_mapping <- list(
  "1" = c("11", "12", "13"),
  "2" = c("21", "22", "23", "24", "25"),
  "3" = c("31", "32", "33", "34", "35", "36", "37", "38", "39"),
  "4" = c("41", "42", "43", "44", "45", "46", "47"),
  "5" = c("51", "52", "53", "54", "55", "56", "57", "58"),
  "6" = c("61", "62", "63", "64"),
  "7" = c("71", "72", "73", "74", "75", "76", "77"),
  "8" = c("81", "82", "83"),
  "9" = c("91", "92", "93", "94")
)

# Convert isscaap_group to character if it's not already
merged_data[, isscaap_group := as.character(isscaap_group)]

# Create a reverse mapping for easy lookup
division_lookup <- data.table(
  isscaap_group = unlist(isscaap_division_mapping),
  isscaap_division = rep(names(isscaap_division_mapping), times = sapply(isscaap_division_mapping, length))
)

# Merge the division information into merged_data
merged_data <- merge(
  merged_data,
  division_lookup,
  by = "isscaap_group",
  all.x = TRUE
)

# Check for any isscaap_group codes not mapped
unmapped_groups <- merged_data[is.na(isscaap_division), unique(isscaap_group)]
if (length(unmapped_groups) > 0) {
  cat("The following isscaap_group codes are not mapped to any division:", paste(unmapped_groups, collapse = ", "), "\n")
}

# Convert isscaap_division to numeric
merged_data[, isscaap_division := as.numeric(isscaap_division)]

#### Interpolate Using Division-Level Mean

# Filter missing domestic prices for years >= 2000
missing_after_step3_3 <- merged_data[is.na(domestic_price_usd1000pertonne) & year >= 2000]

# Calculate mean domestic price at the division level
mean_domestic_price_by_division <- merged_data[
  !is.na(domestic_price_usd1000pertonne) & !is.na(isscaap_division),
  .(mean_domestic_price_division = mean(domestic_price_usd1000pertonne, na.rm = TRUE)),
  by = .(iso3, fao_area_code, year, isscaap_division)
]

# Merge and fill missing values
merged_data <- merge(
  merged_data,
  mean_domestic_price_by_division,
  by = c("iso3", "fao_area_code", "year", "isscaap_division"),
  all.x = TRUE
)

merged_data[
  is.na(domestic_price_usd1000pertonne),
  domestic_price_usd1000pertonne := mean_domestic_price_division
]

# Remove the temporary mean column
merged_data[, mean_domestic_price_division := NULL]

# Step 4: Interpolate Using iso3-year-fao_area-division Average

# Identify rows where domestic_price_usd1000pertonne is still NA
remaining_missing <- merged_data[is.na(domestic_price_usd1000pertonne)]
cat("Number of observations with missing domestic_price_usd1000pertonne before division-level interpolation:", nrow(remaining_missing), "\n")

# Calculate mean domestic price at the iso3-year-fao_area-division level
mean_domestic_price_by_iso3_year_area_division <- merged_data[
  !is.na(domestic_price_usd1000pertonne) & !is.na(isscaap_division),
  .(mean_domestic_price_iso3_year_area_division = mean(domestic_price_usd1000pertonne, na.rm = TRUE)),
  by = .(iso3, year, fao_area_code, isscaap_division)
]

# Merge and fill missing values
merged_data <- merge(
  merged_data,
  mean_domestic_price_by_iso3_year_area_division,
  by = c("iso3", "year", "fao_area_code", "isscaap_division"),
  all.x = TRUE
)

merged_data[
  is.na(domestic_price_usd1000pertonne),
  domestic_price_usd1000pertonne := mean_domestic_price_iso3_year_area_division
]

# Remove the temporary mean column
merged_data[, mean_domestic_price_iso3_year_area_division := NULL]

# Display remaining missing values
remaining_missing <- merged_data[is.na(domestic_price_usd1000pertonne)]
cat("Number of observations with missing domestic_price_usd1000pertonne after iso3-year-fao_area-division-level interpolation:", nrow(remaining_missing), "\n")

# Step 5: Interpolate using year–FAO area–division average across all ISO3

# Calculate mean domestic price at the year–FAO area–division level
mean_domestic_price_by_year_area_division <- merged_data[
  !is.na(domestic_price_usd1000pertonne) & !is.na(isscaap_division),
  .(mean_domestic_price_year_area_division = mean(domestic_price_usd1000pertonne, na.rm = TRUE)),
  by = .(year, fao_area_code, isscaap_division)
]

# Merge the calculated means back into merged_data
merged_data <- merge(
  merged_data,
  mean_domestic_price_by_year_area_division,
  by = c("year", "fao_area_code", "isscaap_division"),
  all.x = TRUE
)

# Fill missing domestic prices using the year–FAO area–division level mean
merged_data[
  is.na(domestic_price_usd1000pertonne),
  domestic_price_usd1000pertonne := mean_domestic_price_year_area_division
]

# Remove the temporary mean column
merged_data[, mean_domestic_price_year_area_division := NULL]

# Check remaining missing values
remaining_missing <- merged_data[is.na(domestic_price_usd1000pertonne)]
cat("Number of observations with missing domestic_price_usd1000pertonne after year–FAO area–division interpolation:", nrow(remaining_missing), "\n")


# Step 6: Interpolate using year–ISO3–division average across all FAO areas

# Calculate mean domestic price at the year–ISO3–division level
mean_domestic_price_by_year_iso3_division <- merged_data[
  !is.na(domestic_price_usd1000pertonne) & !is.na(isscaap_division),
  .(mean_domestic_price_year_iso3_division = mean(domestic_price_usd1000pertonne, na.rm = TRUE)),
  by = .(year, iso3, isscaap_division)
]

# Merge the calculated means back into merged_data
merged_data <- merge(
  merged_data,
  mean_domestic_price_by_year_iso3_division,
  by = c("year", "iso3", "isscaap_division"),
  all.x = TRUE
)

# Fill missing domestic prices using the year–ISO3–division level mean
merged_data[
  is.na(domestic_price_usd1000pertonne),
  domestic_price_usd1000pertonne := coalesce(domestic_price_usd1000pertonne, mean_domestic_price_year_iso3_division)
]

# Remove the temporary mean column
merged_data[, mean_domestic_price_year_iso3_division := NULL]

# Step 7: Interpolate using year–ISSCAP group average across all iso3 and fao_area_code

# Convert isscaap_group to character if it's not already
merged_data[, isscaap_group := as.character(isscaap_group)]

# Calculate mean domestic price at the year–isscaap_group level
mean_domestic_price_by_year_group <- merged_data[
  !is.na(domestic_price_usd1000pertonne),
  .(mean_domestic_price_year_group = mean(domestic_price_usd1000pertonne, na.rm = TRUE)),
  by = .(year, isscaap_group)
]

# Merge the calculated means back into merged_data
merged_data <- merge(
  merged_data,
  mean_domestic_price_by_year_group,
  by = c("year", "isscaap_group"),
  all.x = TRUE
)

# Fill missing domestic prices using the year–isscaap_group level mean
merged_data[
  is.na(domestic_price_usd1000pertonne),
  domestic_price_usd1000pertonne := mean_domestic_price_year_group
]

# Remove the temporary mean column
merged_data[, mean_domestic_price_year_group := NULL]

####Step 8: Interpolate Using Year–ISSCAP Division Average

# Calculate mean domestic price at the year–isscaap_division level
mean_domestic_price_by_year_division <- merged_data[
  !is.na(domestic_price_usd1000pertonne) & !is.na(isscaap_division),
  .(mean_domestic_price_year_division = mean(domestic_price_usd1000pertonne, na.rm = TRUE)),
  by = .(year, isscaap_division)
]

# Merge the calculated means back into merged_data
merged_data <- merge(
  merged_data,
  mean_domestic_price_by_year_division,
  by = c("year", "isscaap_division"),
  all.x = TRUE
)

# Fill missing domestic prices using the year–isscaap_division level mean
merged_data[
  is.na(domestic_price_usd1000pertonne),
  domestic_price_usd1000pertonne := mean_domestic_price_year_division
]

# Remove the temporary mean column
merged_data[, mean_domestic_price_year_division := NULL]

# # Identify remaining missing domestic prices
# remaining_missing <- merged_data[is.na(domestic_price_usd1000pertonne) & year >= 2000]
# cat("Number of observations with missing domestic_price_usd1000pertonne after division-level interpolation:", nrow(remaining_missing), "\n")
# Only 2052 missing values: about 0.6% and we have no-missing data after 2017

# Save data
fwrite(merged_data, paste0(output_dir,"fishstatj_cleaned.csv"))


################################################################################

# BELOW IS DRAFT; DROP LATER FOR SUBMISSION

################################################################################
# ### Case 2: Two prices exist (>0)
# 
# #### Case 2.1: aqua + import exist
# merged_data[
#   num_existing_prices == 2 & price_aqua_exists & price_import_exists & !price_export_exists,
#   domestic_price_usd1000pertonne := aqua_price_usd1000pertlw
# ]
# 
# # Show missing values after Case 2.1
# missing_after_case2_1 <- merged_data[is.na(domestic_price_usd1000pertonne)]
# cat("Number of missing domestic_price_usd1000pertonne after Case 2.1:", nrow(missing_after_case2_1), "\n")
# 
# #### Case 2.2: aqua + export exist
# merged_data[
#   num_existing_prices == 2 & price_aqua_exists & price_export_exists & !price_import_exists,
#   domestic_price_usd1000pertonne := aqua_price_usd1000pertlw
# ]
# 
# # Show missing values after Case 2.2
# missing_after_case2_2 <- merged_data[is.na(domestic_price_usd1000pertonne)]
# cat("Number of missing domestic_price_usd1000pertonne after Case 2.2:", nrow(missing_after_case2_2), "\n")
# 
# #### Case 2.3: import + export exist
# # Define condition for Case 2.3
# case_2_3_condition <- merged_data[
#   num_existing_prices == 2 & price_import_exists & price_export_exists & !price_aqua_exists & (import_quant_tpw + export_quant_tpw) > 0,
#   .I
# ]
# 
# # Perform calculation for Case 2.3
# # Calculate total_trade_quant, weights, and domestic_price outside the data.table to avoid the := error
# if (length(case_2_3_condition) > 0) {
#   total_trade_quant <- merged_data[case_2_3_condition, import_quant_tpw + export_quant_tpw]
#   W_import <- merged_data[case_2_3_condition, import_quant_tpw / total_trade_quant]
#   W_export <- merged_data[case_2_3_condition, export_quant_tpw / total_trade_quant]
#   
#   # Avoid division by zero and handle NA, Inf values
#   W_import <- fifelse(is.na(W_import) | is.infinite(W_import), 0, W_import)
#   W_export <- fifelse(is.na(W_export) | is.infinite(W_export), 0, W_export)
#   
#   # Calculate domestic_price
#   domestic_price <- W_import * merged_data[case_2_3_condition, import_price_usd1000pertpw] +
#     W_export * merged_data[case_2_3_condition, export_price_usd1000pertpw]
#   
#   # Assign domestic_price to domestic_price_usd1000pertonne
#   merged_data[case_2_3_condition, domestic_price_usd1000pertonne := domestic_price]
# }
# 
# # Show missing values after Case 2.3
# missing_after_case2_3 <- merged_data[is.na(domestic_price_usd1000pertonne)]
# cat("Number of missing domestic_price_usd1000pertonne after Case 2.3:", nrow(missing_after_case2_3), "\n")
# 
# ### Case 3: None of the prices exist (>0)
# 
# #### Step 3.1: Use mean domestic price for iso3-species-year across all fao_area_code
# mean_domestic_price_by_iso3_species_year <- merged_data[
#   !is.na(domestic_price_usd1000pertonne),
#   .(mean_domestic_price_iso3_species_year = mean(domestic_price_usd1000pertonne, na.rm = TRUE)),
#   by = .(iso3, isscaap_group, year)
# ]
# 
# # Merge and fill missing values
# merged_data <- merge(
#   merged_data,
#   mean_domestic_price_by_iso3_species_year,
#   by = c("iso3", "isscaap_group", "year"),
#   all.x = TRUE
# )
# 
# merged_data[
#   is.na(domestic_price_usd1000pertonne),
#   domestic_price_usd1000pertonne := mean_domestic_price_iso3_species_year
# ]
# 
# # Remove temporary column
# merged_data[, mean_domestic_price_iso3_species_year := NULL]
# 
# # Show missing values after Step 3.1
# missing_after_step3_1 <- merged_data[is.na(domestic_price_usd1000pertonne)]
# cat("Number of missing domestic_price_usd1000pertonne after Step 3.1:", nrow(missing_after_step3_1), "\n")
# 
# #### Step 3.2: Use mean domestic price from previous and next years
# # Sort data
# setorder(merged_data, iso3, isscaap_group, fao_area_code, year)
# 
# # Create lag and lead domestic prices
# merged_data[, domestic_price_lag1 := shift(domestic_price_usd1000pertonne, 1, type = "lag"), by = .(iso3, isscaap_group, fao_area_code)]
# merged_data[, domestic_price_lead1 := shift(domestic_price_usd1000pertonne, 1, type = "lead"), by = .(iso3, isscaap_group, fao_area_code)]
# 
# # Fill missing domestic prices with mean of adjacent years
# merged_data[
#   is.na(domestic_price_usd1000pertonne),
#   domestic_price_usd1000pertonne := rowMeans(.SD, na.rm = TRUE),
#   .SDcols = c("domestic_price_lag1", "domestic_price_lead1")
# ]
# 
# # Remove temporary columns
# merged_data[, c("domestic_price_lag1", "domestic_price_lead1") := NULL]
# 
# # Show missing values after Step 3.2
# missing_after_step3_2 <- merged_data[is.na(domestic_price_usd1000pertonne)]
# cat("Number of missing domestic_price_usd1000pertonne after Step 3.2:", nrow(missing_after_step3_2), "\n")
# 
# #### Step 3.3: Use mean domestic price for species-fao_area-year across all iso3
# mean_domestic_price_by_species_area_year <- merged_data[
#   !is.na(domestic_price_usd1000pertonne),
#   .(mean_domestic_price_species_area_year = mean(domestic_price_usd1000pertonne, na.rm = TRUE)),
#   by = .(isscaap_group, fao_area_code, year)
# ]
# 
# # Merge and fill missing values
# merged_data <- merge(
#   merged_data,
#   mean_domestic_price_by_species_area_year,
#   by = c("isscaap_group", "fao_area_code", "year"),
#   all.x = TRUE
# )
# 
# merged_data[
#   is.na(domestic_price_usd1000pertonne),
#   domestic_price_usd1000pertonne := mean_domestic_price_species_area_year
# ]
# 
# # Remove temporary column
# merged_data[, mean_domestic_price_species_area_year := NULL]
# 
# # Show missing values after Step 3.3
# missing_after_step3_3 <- merged_data[is.na(domestic_price_usd1000pertonne)]
# cat("Number of missing domestic_price_usd1000pertonne after Step 3.3:", nrow(missing_after_step3_3), "\n")
# 
# # Remove helper columns
# merged_data[, c("price_aqua_exists", "price_export_exists", "price_import_exists", "num_existing_prices") := NULL]
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ############################
# ##########################
# 
# # If the Capture prod Quant is NA or 0, Then it means we do not need to 
# # calculate the Capture Price as there is no Capture Value added
# 
# # But if the Capture Prod Quant is not NA nor 0, then it means we need to find a 
# # way to calculate the price_capture, which means we need at least one of 
# # three price (aqua_price, export_price, import_price)
# # existing to calcualte the price_capture
# 
# # Case 1: Only one price is available: 
# # Explnation: If only one of the three prices 
# # (aqua_price_usd1000pertlw, export_price_usd1000pertpw, import_price_usd1000pertpw) 
# # is available, use it as the estimate for capture_price_usd1000pertlw.
# 
# # Initialize capture_price_usd1000pertlw with NA
# merged_data[, capture_price_usd1000pertlw := NA_real_]
# 
# # 
# merged_data[
#   !is.na(capture_prod_quant_tlw) & capture_prod_quant_tlw != 0 &
#     (
#       (!is.na(aqua_price_usd1000pertlw) & (is.na(export_price_usd1000pertpw) | export_price_usd1000pertpw == 0) & (is.na(import_price_usd1000pertpw) | import_price_usd1000pertpw == 0)) |
#         ((is.na(aqua_price_usd1000pertlw) | aqua_price_usd1000pertlw == 0) & !is.na(export_price_usd1000pertpw) & (is.na(import_price_usd1000pertpw) | import_price_usd1000pertpw == 0)) |
#         ((is.na(aqua_price_usd1000pertlw) | aqua_price_usd1000pertlw == 0) & (is.na(export_price_usd1000pertpw) | export_price_usd1000pertpw == 0) & !is.na(import_price_usd1000pertpw))
#     ),
#   capture_price_usd1000pertlw := fifelse(
#     !is.na(aqua_price_usd1000pertlw) & aqua_price_usd1000pertlw != 0, aqua_price_usd1000pertlw,
#     fifelse(
#       !is.na(export_price_usd1000pertpw) & export_price_usd1000pertpw != 0, export_price_usd1000pertpw,
#       import_price_usd1000pertpw
#     )
#   )
# ]
# 
# 
# 
# # Case 2: if 2 of three existing
# 
# # Case 2.1: aqua_price and export_price are available
# 
# # Q_domestic = (Q_Cap+Q_aqua)
# # Then P_domestic = P_export = W_cap * P_cap + W_aqua * P_aqua,
# # where W_cap = Q_Cap/(Q_Cap+Q_aqua); W_aqua = Q_aqua/(Q_Cap+Q_aqua)
# # That means P_cap = (P_export-W_aqua * P_aqua)/W_cap
# 
# 
# # Calculate weights W_cap and W_aqua first
# merged_data[
#   !is.na(capture_prod_quant_tlw) & capture_prod_quant_tlw != 0 &
#     !is.na(aqua_price_usd1000pertlw) & aqua_price_usd1000pertlw != 0 &
#     !is.na(export_price_usd1000pertpw) & export_price_usd1000pertpw != 0 &
#     (is.na(import_price_usd1000pertpw) | import_price_usd1000pertpw == 0),
#   `:=`(
#     W_cap = capture_prod_quant_tlw / (capture_prod_quant_tlw + aqua_prod_quant_tlw),
#     W_aqua = aqua_prod_quant_tlw / (capture_prod_quant_tlw + aqua_prod_quant_tlw)
#   )
# ]
# 
# # Now calculate capture_price_usd1000pertlw using the weights
# merged_data[
#   !is.na(capture_prod_quant_tlw) & capture_prod_quant_tlw != 0 &
#     !is.na(aqua_price_usd1000pertlw) & aqua_price_usd1000pertlw != 0 &
#     !is.na(export_price_usd1000pertpw) & export_price_usd1000pertpw != 0 &
#     (is.na(import_price_usd1000pertpw) | import_price_usd1000pertpw == 0),
#   capture_price_usd1000pertlw := (export_price_usd1000pertpw - W_aqua * aqua_price_usd1000pertlw) / W_cap
# ]
# 
# # 2.2) aqua_price + import_price existing
# 
# 
# 
# 
# 
# # Case 3: all three price existing
# 
# 
# # Case 4: None of three price existing-we use the following code to seperate out the no-price subset first
# 
# # Filter the merged_data
# caputure_data_no_price <- merged_data[
#   !is.na(capture_prod_quant_tlw) & capture_prod_quant_tlw != 0 &  # capture_prod_quant_tlw is not NA or 0
#     (is.na(aqua_price_usd1000pertlw) | aqua_price_usd1000pertlw == 0) &  # aqua_price_usd1000pertlw is NA or 0
#     (is.na(export_price_usd1000pertpw) | export_price_usd1000pertpw == 0) &  # export_price_usd1000pertpw is NA or 0
#     (is.na(import_price_usd1000pertpw) | import_price_usd1000pertpw == 0)  # import_price_usd1000pertpw is NA or 0
# ]
# 
# ## Then, in this case, we try the following steps to do interpolation for the capture_price
# 
# # Step 4.1: Try use the capture_price iso3-species-year aggreate at all all fao_area's mean to interpolate
# 
# # Step 4.2: Try use the mean of capture_price: (iso3-species-year_t-1 and iso3-species-year_t+1)
# 
# # Step 4.3: Try use the mean of capture_price: (species-fao_area-year) for all iso3
# 
# ## Check if there is still existing no capture_price cases
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# --------------
# # 3.3 Trade Data Merge
# 
# # Merge trade_quant and trade_value by Country and species
# merged_trade <- merge(
#   trade_quant,
#   trade_value,
#   by = c("Country", "species"),
#   all = TRUE,  # Include all rows from both datasets
#   suffixes = c("_trade_quant", "_trade_value")
# )
# 
# # Drop unnecessary columns, including any extra unit_trade_price.1 if it exists
# merged_trade[, c("trade_flow_trade_quant", "trade_flow_trade_value", "Unit_trade_quant", "Unit_trade_value") := NULL]
# 
# # Add a single column for unit_trade_price with the value "1000USD/TWL"
# merged_trade[, unit_trade_price := "1000USD/TWL"]
# 
# # Identify year columns for trade_quant and trade_value
# year_cols_quant <- grep("_trade_quant$", colnames(merged_trade), value = TRUE)
# year_cols_value <- grep("_trade_value$", colnames(merged_trade), value = TRUE)
# 
# # Ensure both have matching years
# matching_years <- intersect(
#   sub("_trade_quant$", "", year_cols_quant),
#   sub("_trade_value$", "", year_cols_value)
# )
# 
# # Calculate year_trade_price for each matching year
# for (year in matching_years) {
#   col_quant <- paste0(year, "_trade_quant")
#   col_value <- paste0(year, "_trade_value")
#   col_price <- paste0(year, "_trade_price")
#   
#   # Add the new column for trade price
#   merged_trade[, (col_price) := ifelse(!is.na(get(col_quant)) & get(col_quant) != 0,
#                                        get(col_value) / get(col_quant), 
#                                        NA)]
# }
# 
# # Keep only Country, species, and trade_price columns
# trade_price_cols <- grep("_trade_price$", colnames(merged_trade), value = TRUE)
# 
# # Ensure only one unit_trade_price column is included
# trade_price <- merged_trade[, c("Country", "species", trade_price_cols), with = FALSE]
# trade_price[, unit_trade_price := "1000USD/TWL"]  # Add unit_trade_price after filtering columns
# 
# 
# # View the resulting trade_price_data
# View(trade_price)
# 
# 
# fwrite(trade_price, "../raw_data/fishstatj/calculated_export_trade_price.csv")
# 
# 
# # Reshape capture_prod_quant
# capture_prod_quant <- melt(
#   data = capture_prod_quant,
#   id.vars = c("Country", "species", "fao_area", "Unit"),  # Columns to keep as is
#   measure.vars = patterns("^\\d{4}_capture_prod_quant$"),  # Yearly columns
#   variable.name = "Year",  # Name for the melted variable
#   value.name = "capture_prod_quant"  # Name for the melted value
# )
# 
# # Extract the year from the column names
# capture_prod_quant[, Year := as.numeric(gsub("_capture_prod_quant", "", Year))]
# 
# # Verify the reshaped data
# head(capture_prod_quant)
# 
# # Reshape aqua_prod_quant
# aqua_prod_quant <- melt(
#   data = aqua_prod_quant,
#   id.vars = c("Country", "species", "fao_area", "Unit"),
#   measure.vars = patterns("^\\d{4}_aqua_prod_quant$"),
#   variable.name = "Year",
#   value.name = "aqua_prod_quant"
# )
# 
# # Extract the year
# aqua_prod_quant[, Year := as.numeric(gsub("_aqua_prod_quant", "", Year))]
# 
# # Verify the reshaped data
# head(aqua_prod_quant)
# 
# 
# # Reshape aqua_prod_value
# aqua_prod_value <- melt(
#   data = aqua_prod_value,
#   id.vars = c("Country", "species", "fao_area", "Unit"),
#   measure.vars = patterns("^\\d{4}_aqua_prod_value$"),
#   variable.name = "Year",
#   value.name = "aqua_prod_value"
# )
# 
# # Extract the year
# aqua_prod_value[, Year := as.numeric(gsub("_aqua_prod_value", "", Year))]
# 
# # Verify the reshaped data
# head(aqua_prod_value)
# 
# # Reshape trade_price
# trade_price <- melt(
#   data = trade_price,
#   id.vars = c("Country", "species", "unit_trade_price"),  # Columns to keep as is
#   measure.vars = patterns("^\\d{4}_trade_price$"),  # Yearly columns
#   variable.name = "Year",  # Name for the melted variable
#   value.name = "trade_price"  # Name for the melted value
# )
# 
# # Extract the year from the column names
# trade_price[, Year := as.numeric(gsub("_trade_price", "", Year))]
# 
# # Merge aqua_prod_quant and aqua_prod_value
# merged_aqua <- merge(
#   aqua_prod_quant,
#   aqua_prod_value,
#   by = c("Country", "species", "fao_area", "Year"),
#   all = TRUE  # Ensure all rows are retained
# )
# 
# # Drop the Unit.x and Unit.y columns
# merged_aqua[, c("Unit.x", "Unit.y") := NULL]
# 
# # Rename columns for clarity
# setnames(merged_aqua, old = c("aqua_prod_quant", "aqua_prod_value"),
#          new = c("aqua_prod_quant_tlw", "aqua_prod_value_usd1000"))
# 
# # Step 1: Process capture_prod_quant
# # Drop the Unit column if it exists
# capture_prod_quant[, Unit := NULL]
# 
# # Rename the capture_prod_quant columns
# setnames(capture_prod_quant, old = c("capture_prod_quant"), new = c("capture_prod_quant_tlw"))
# 
# # Step 2: Merge capture_prod_quant with merged_aqua
# merged_prod <- merge(
#   merged_aqua,
#   capture_prod_quant,
#   by = c("Country", "species", "fao_area", "Year"),
#   all = TRUE  # Retain all rows from both datasets
# )
# 
# 
# # Step 1: Drop the unit_trade_price column (if it exists)
# if ("unit_trade_price" %in% colnames(trade_price)) {
#   trade_price[, unit_trade_price := NULL]
# }
# 
# setnames(trade_price, old = "trade_price", new = "export_price_usd1000per_tpw")
# 
# # Step 2: Merge merged_prod with trade_price
# fishstatj_cleaned_dt <- merge(
#   merged_prod,
#   trade_price,
#   by = c("Country", "species", "Year"),
#   all = TRUE  # Retain all rows from both datasets
# )
# 
# # Define the columns to check for all NA
# cols_to_check <- c("aqua_prod_quant_tlw", "aqua_prod_value_usd1000", "capture_prod_quant_tlw")
# 
# # Create a logical vector indicating which rows to keep
# rows_to_keep <- rowSums(!is.na(fishstatj_cleaned_dt[, ..cols_to_check])) > 0
# 
# # Subset the data.table using the logical vector
# fishstatj_cleaned_dt <- fishstatj_cleaned_dt[rows_to_keep]
# 
# # Verify the result
# head(fishstatj_cleaned_dt)
# 
# # Ensure both columns are numeric to avoid errors
# fishstatj_cleaned_dt[, aqua_prod_value_usd1000 := as.numeric(aqua_prod_value_usd1000)]
# fishstatj_cleaned_dt[, aqua_prod_quant_tlw := as.numeric(aqua_prod_quant_tlw)]
# 
# # Create the new column
# fishstatj_cleaned_dt[, aqua_prod_price_usd1000per_tlw := 
#                        ifelse(!is.na(aqua_prod_quant_tlw) & aqua_prod_quant_tlw != 0, 
#                               aqua_prod_value_usd1000 / aqua_prod_quant_tlw, 
#                               NA)
# ]
# 
# 
# # Filter out rows where Country equals "Totals - Tonnes - live weight" or "Totals - Number"
# fishstatj_cleaned_dt <- fishstatj_cleaned_dt[Country != "Totals - Tonnes - live weight"]
# fishstatj_cleaned_dt <- fishstatj_cleaned_dt[Country != "Totals - Number"]
# 
# fwrite(fishstatj_cleaned_dt, "../raw_data/fishstatj/fishstatj_cleaned_dt.csv")
# gc()
