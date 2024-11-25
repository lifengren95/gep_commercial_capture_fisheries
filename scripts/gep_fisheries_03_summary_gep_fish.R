############################################################
# Project: Global GEP: Fisheries-Summary
# Author: Lifeng Ren
# Date Created: 11/24/2024
# Last Modified: 11/24/2024
# Description: This script calculates the summary statistics for GEP-fisheries
############################################################

#------------------------------------------------------------
# 1. Setup----
#------------------------------------------------------------

# 1.1 Load Required Libraries
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
# 2. Data Import----
#------------------------------------------------------------

# Import the data using fread
gep_fish_iso3_year <- fread("../intermediate/gep_fish/gep_fish_iso3_year.csv")

#------------------------------------------------------------
# 3. GEP_fish Summary
#------------------------------------------------------------

# Ensure the data is a data.table
setDT(gep_fish_iso3_year)

# Add global total GEP for each year (convert to USD billion)
global_gep_fish_summary <- gep_fish_iso3_year[, {
  num_countries = uniqueN(iso3)
  max_gep = max(GEP_fish, na.rm = TRUE) / 1e6  # Convert to billion
  min_gep = min(GEP_fish, na.rm = TRUE) / 1e6  # Convert to billion
  median_gep = median(GEP_fish, na.rm = TRUE) / 1e6  # Convert to billion
  total_gep = sum(GEP_fish, na.rm = TRUE) / 1e6  # Convert to billion
  
  # Identify countries with max and min GEP
  country_max = iso3[GEP_fish == max_gep * 1e6]
  country_min = iso3[GEP_fish == min_gep * 1e6]
  
  # Calculate absolute differences from median
  abs_diff = abs(GEP_fish - median_gep * 1e6)
  min_abs_diff = min(abs_diff, na.rm = TRUE)
  
  # Identify country or countries closest to median GEP
  country_median = iso3[abs_diff == min_abs_diff]
  
  .(year, num_countries, max_gep, min_gep, median_gep, total_gep,
    country_max = paste(country_max, collapse = ", "),
    country_min = paste(country_min, collapse = ", "),
    country_median = paste(country_median, collapse = ", "))
}, by = year]

# View the summary
print(global_gep_fish_summary)

# Check for duplicate column names
if (anyDuplicated(names(global_gep_fish_summary))) {
  print("Duplicate column names found:")
  print(names(global_gep_fish_summary)[duplicated(names(global_gep_fish_summary))])
}

# Remove duplicate columns
if (anyDuplicated(names(global_gep_fish_summary))) {
  global_gep_fish_summary <- global_gep_fish_summary[, !duplicated(names(global_gep_fish_summary)), with = FALSE]
}

fwrite(global_gep_fish_summary, paste0(output_dir,"global_gep_fish_summary.csv"))


library(ggplot2)

# Convert year to numeric for plotting
global_gep_fish_summary$year <- as.numeric(global_gep_fish_summary$year)

# Create a line plot of global total GEP_fish over time
gep_fish_plot <- ggplot(global_gep_fish_summary, aes(x = year, y = total_gep)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "red", size = 3) +
  labs(
    title = "Global Total GEP from Fisheries (1984-2019)",
    x = "Year",
    y = "Global GEP (USD Billion)"
  ) +
  theme_minimal()

# Ensure the output directory has a trailing slash
output_dir <- ifelse(grepl("/$", output_dir), output_dir, paste0(output_dir, "/"))

# Save the plot as a PNG file
ggsave(filename = paste0(output_dir, "global_gep_fish_line_plot.png"), 
       plot = gep_fish_plot, 
       width = 6, 
       height = 4, 
       dpi = 300)
