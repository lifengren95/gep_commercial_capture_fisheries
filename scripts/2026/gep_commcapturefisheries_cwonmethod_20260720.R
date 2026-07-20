###############################################################################
# Project: GEP Fisheries
# Script:  Estimate commercial fisheries provisioning service
#
# Author:  Lifeng Ren
# Task:    Estimate 2019 commercial fisheries economic rent using CWON data
#          and merge the estimates with the GEP country template.
# Date:    2026-07-20
#
# Inputs:
#   - CWON CPI data
#   - CWON fisheries economic-rent data
#   - GEP service-country template
#
# Outputs:
#   - fish_provision_gep_20260720.xlsx
#   - fish_provision_gep_20260720.csv
###############################################################################


# =============================================================================
# 1. Initialize workspace
# =============================================================================

rm(list = ls())

options(
  stringsAsFactors = FALSE,
  scipen = 999
)

setwd(
  "F:/Dropbox/400_research/430_projects/432_UMN/443_gep/gep_fisheries/scripts/2026"
)


# =============================================================================
# 2. Load libraries
# =============================================================================

library(haven)
library(dplyr)
library(readr)
library(writexl)


# =============================================================================
# 3. Define input and output paths
# =============================================================================

cpi_file <- file.path(
  "../../raw_data/cwon",
  "FR_WLD_2024_195",
  "FR_WLD_2024_195",
  "Reproducibility package",
  "Working",
  "cpi2019.dta"
)

econ_rent_file <- file.path(
  "../../raw_data/cwon",
  "FR_WLD_2024_195",
  "FR_WLD_2024_195",
  "Reproducibility package",
  "Input",
  "User_update",
  "EconRent_Analysis_AllYears.dta"
)

gep_template_file <- file.path(
  "../../raw_data",
  "EXAMPLE_SERVICE_gep.csv"
)

output_dir <- file.path(
  "../../intermediate",
  "gep_fish",
  "2026"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# =============================================================================
# 4. Read input data
# =============================================================================

cpi2019 <- read_dta(cpi_file)

econ_rent_all_years <- read_dta(econ_rent_file)

example_service_gep <- read_csv(
  gep_template_file,
  show_col_types = FALSE
)


# =============================================================================
# 5. Clean CPI data
# =============================================================================

cpi2019 <- cpi2019 %>%
  filter(
    year >= 2009,
    year <= 2019
  ) %>%
  group_by(year) %>%
  mutate(
    cpi2019 = if_else(
      is.na(cpi2019),
      mean(cpi2019, na.rm = TRUE),
      cpi2019
    )
  ) %>%
  ungroup() %>%
  rename(
    wb_code = countrycode
  ) %>%
  mutate(
    wb_code = as.character(wb_code),
    year = as.character(year)
  ) %>%
  select(
    wb_code,
    year,
    cpi2019
  )


# Optional data check
# cpi2019 %>%
#   summarise(n_missing = sum(is.na(cpi2019)))


# =============================================================================
# 6. Clean fisheries economic-rent data
# =============================================================================

econ_rent_all_years <- econ_rent_all_years %>%
  select(
    Year,
    countryname,
    wb_code,
    FAOCatch,
    FAO_landval,
    FAOVarCost,
    FAOFixCost,
    FAOTotCost,
    FAO_landval2018,
    FAOProfit,
    FAOEconRent
  ) %>%
  rename(
    year = Year
  ) %>%
  mutate(
    wb_code = as.character(wb_code),
    year = as.character(year)
  ) %>%
  filter(
    year %in% as.character(2009:2019),
    !wb_code %in% c("CYP", "MAF", "PSE"),
    !(wb_code == "CUW" & year %in% c("2009", "2010"))
  )


# =============================================================================
# 7. Merge CPI and economic-rent data
# =============================================================================

econ_rent_with_cpi <- econ_rent_all_years %>%
  left_join(
    cpi2019,
    by = c("wb_code", "year")
  ) %>%
  mutate(
    resrent_2019usd = FAOEconRent / cpi2019 * 100
  )


# =============================================================================
# 8. Estimate country-level economic-rent trends
# =============================================================================

resrent_trends <- econ_rent_with_cpi %>%
  mutate(
    t = as.integer(year)
  ) %>%
  filter(
    t >= 2009,
    t <= 2018
  ) %>%
  group_by(wb_code) %>%
  group_modify(
    ~ {
      valid_data <- .x %>%
        filter(
          !is.na(t),
          is.finite(resrent_2019usd)
        )
      
      tibble(
        n_years = n_distinct(valid_data$t),
        
        mean_resrent_2009_2018 = if (nrow(valid_data) > 0) {
          mean(valid_data$resrent_2019usd)
        } else {
          NA_real_
        },
        
        beta_hat = if (n_distinct(valid_data$t) >= 2) {
          unname(
            coef(
              lm(
                resrent_2019usd ~ t,
                data = valid_data
              )
            )[["t"]]
          )
        } else {
          NA_real_
        }
      )
    }
  ) %>%
  ungroup() %>%
  mutate(
    resrent_2019_hat =
      mean_resrent_2009_2018 +
      beta_hat * (2019 - 2013.5),
    
    positive_resrent_2019_hat =
      pmax(resrent_2019_hat, 0)
  )


# =============================================================================
# 9. Calculate total estimated positive rent
# =============================================================================

total_positive_resrent_2019 <- resrent_trends %>%
  summarise(
    total_positive_resrent_2019_hat = sum(
      positive_resrent_2019_hat,
      na.rm = TRUE
    )
  )

# View total if needed
# total_positive_resrent_2019


# =============================================================================
# 10. Merge estimates with the GEP country template
# =============================================================================

resrent_trends <- resrent_trends %>%
  rename(
    iso3_r250_label = wb_code
  ) %>%
  mutate(
    iso3_r250_label = as.character(iso3_r250_label)
  )

example_service_gep <- example_service_gep %>%
  mutate(
    iso3_r250_label = as.character(iso3_r250_label)
  ) %>%
  left_join(
    resrent_trends,
    by = "iso3_r250_label"
  )


# Optional matching check
# unmatched_resrent <- resrent_trends %>%
#   anti_join(
#     example_service_gep %>%
#       distinct(iso3_r250_label),
#     by = "iso3_r250_label"
#   )
#
# unmatched_resrent
#
# Channel Islands is unmatched, which is acceptable.


# =============================================================================
# 11. Create final commercial-fisheries dataset
# =============================================================================

gep_commfish_2019_CWON <- example_service_gep %>%
  select(
    iso3_r250_id,
    iso3_r250_label,
    iso3_r250_name,
    positive_resrent_2019_hat
  ) %>%
  rename(
    commfish_provision = positive_resrent_2019_hat
  ) %>%
  mutate(
    commfish_provision = as.numeric(commfish_provision)
  )


# =============================================================================
# 12. Export results
# =============================================================================

# Full-precision Excel output
write_xlsx(
  gep_commfish_2019_CWON,
  file.path(
    output_dir,
    "fish_provision_gep_20260720.xlsx"
  )
)


# Excel-friendly CSV with values rounded to whole 2019 USD
gep_commfish_2019_CWON_csv <- gep_commfish_2019_CWON %>%
  mutate(
    commfish_provision = round(commfish_provision)
  )

write_excel_csv(
  gep_commfish_2019_CWON_csv,
  file.path(
    output_dir,
    "fish_provision_gep_20260720.csv"
  ),
  na = ""
)


# =============================================================================
# 13. Final checks
# =============================================================================

message("Script completed successfully.")
message("Output directory: ", normalizePath(output_dir))

print(total_positive_resrent_2019)