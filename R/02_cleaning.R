# ------------------------------------------------------------------
# 02_cleaning.R
# Step 2 - turn the raw OWID file into a clean country-year table
#
# What happens here:
#   1. drop the region / income-group aggregates
#   2. keep 2000-2022 (electricity mix is patchy before 2000 and
#      GDP stops at 2022)
#   3. keep only the columns needed for the questions
#   4. deal with missing values (interpolation inside each country,
#      0 for power sources a country simply doesn't have)
#   5. add gdp per capita and continent
# Output: data/processed/energy_clean.csv
# ------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(countrycode)

energy_raw <- read.csv("data/raw/owid-energy-data.csv", stringsAsFactors = FALSE)

# ---- 1. countries only ----
energy <- energy_raw %>%
  filter(iso_code != "", !grepl("^OWID", iso_code))

# ---- 2. time window ----
energy <- energy %>% filter(year >= 2000, year <= 2022)

# ---- 3. columns we actually need ----
keep_cols <- c("country", "iso_code", "year", "population", "gdp",
               "energy_per_capita", "per_capita_electricity",
               "electricity_generation", "carbon_intensity_elec",
               "renewables_share_elec", "fossil_share_elec",
               "coal_share_elec", "gas_share_elec", "oil_share_elec",
               "nuclear_share_elec", "hydro_share_elec",
               "solar_share_elec", "wind_share_elec")
energy <- energy %>% select(all_of(keep_cols))

cat("After filtering:", nrow(energy), "rows x", ncol(energy), "cols\n")
cat("Missing values per column BEFORE treatment:\n")
print(colSums(is.na(energy)))

# ---- 4. missing values ----
# (a) ~50 small territories (e.g. Aruba, Cook Islands) have no GDP at all.
#     You can't impute a whole country's GDP from nothing, so drop them.
no_gdp <- energy %>% group_by(country) %>%
  summarise(all_na = all(is.na(gdp))) %>% filter(all_na) %>% pull(country)
cat("\nCountries dropped for having no GDP at all:", length(no_gdp), "\n")
energy <- energy %>% filter(!country %in% no_gdp)

# (b) For gaps inside a country's own series, linear interpolation over
#     time makes sense - GDP or energy use doesn't jump randomly between
#     years. rule = 2 carries the nearest value to the edges.
fill_series <- function(x, yr) {
  if (sum(!is.na(x)) < 2) return(x)        # not enough points to interpolate
  approx(yr, x, xout = yr, rule = 2)$y
}
num_cols <- setdiff(keep_cols, c("country", "iso_code", "year"))
energy <- energy %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(across(all_of(num_cols), ~ fill_series(.x, year))) %>%
  ungroup()

# (c) A missing nuclear/solar/wind share usually just means the country
#     doesn't have that source. If the country reports electricity at all
#     we set those to 0 rather than throwing the row away.
source_cols <- c("coal_share_elec", "gas_share_elec", "oil_share_elec",
                 "nuclear_share_elec", "hydro_share_elec",
                 "solar_share_elec", "wind_share_elec")
energy <- energy %>%
  mutate(across(all_of(source_cols),
                ~ ifelse(is.na(.x) & !is.na(electricity_generation), 0, .x)))

# (d) whatever is still missing after that is dropped
before <- nrow(energy)
energy <- energy %>% drop_na()
cat("Rows dropped with leftover NAs:", before - nrow(energy), "\n")

# ---- 5. new features ----
energy <- energy %>%
  mutate(
    gdp_per_capita = gdp / population,
    continent = countrycode(iso_code, origin = "iso3c", destination = "continent")
  )
# Kosovo's code (OWID_KOS) is already gone, but just in case
energy <- energy %>% filter(!is.na(continent))

# sanity checks - shares have to be between 0 and 100
stopifnot(all(energy$renewables_share_elec >= 0 & energy$renewables_share_elec <= 100))
stopifnot(!anyNA(energy))

cat("\nCLEAN DATA:", nrow(energy), "rows x", ncol(energy), "columns,",
    n_distinct(energy$country), "countries\n")
print(table(energy$continent))

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
write.csv(energy, "data/processed/energy_clean.csv", row.names = FALSE)
