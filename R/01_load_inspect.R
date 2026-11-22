# ------------------------------------------------------------------
# 01_load_inspect.R
# Step 1 - get the raw data and have a first look at it
#
# Dataset : Our World in Data (OWID) Energy dataset
# Source  : https://github.com/owid/energy-data
# One row = one country (or region) in one year, 1900-2025
#
# The csv is about 9 MB so I don't keep it in the repo, the script
# downloads it the first time it runs.
# ------------------------------------------------------------------

library(dplyr)

raw_url  <- "https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv"
raw_path <- "data/raw/owid-energy-data.csv"

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
if (!file.exists(raw_path)) {
  message("Downloading raw data from OWID ...")
  download.file(raw_url, raw_path, mode = "wb")
}

energy_raw <- read.csv(raw_path, stringsAsFactors = FALSE)

# ---- size ----
cat("Raw size:", nrow(energy_raw), "rows x", ncol(energy_raw), "columns\n")
cat("Years:", min(energy_raw$year), "to", max(energy_raw$year), "\n")
cat("Distinct 'country' names:", n_distinct(energy_raw$country), "\n")

# ---- column types ----
# nearly everything is numeric, only country and iso_code are text
print(table(sapply(energy_raw, class)))

# ---- aggregates ----
# A lot of rows are not countries at all, e.g. "World", "Africa",
# "High-income countries", "Europe (EI)". They either have an empty
# iso_code or an OWID_ code (OWID_WRL etc). Keeping them would double
# count everything, so they get removed in the cleaning script.
aggregates <- energy_raw %>%
  filter(iso_code == "" | grepl("^OWID", iso_code)) %>%
  distinct(country)
cat("\nAggregate / region rows found:", nrow(aggregates), "names, e.g.\n")
print(head(aggregates$country, 10))

# ---- missing values ----
miss_pct <- sort(colMeans(is.na(energy_raw)) * 100, decreasing = TRUE)
cat("\nOverall % of cells missing:", round(mean(is.na(energy_raw)) * 100, 1), "%\n")
cat("Columns that are more than 50% empty:", sum(miss_pct > 50), "of", ncol(energy_raw), "\n")
print(round(head(miss_pct, 10), 1))

# missing values by decade - the early years are basically empty
by_decade <- energy_raw %>%
  mutate(decade = floor(year / 10) * 10) %>%
  group_by(decade) %>%
  summarise(pct_missing = round(mean(is.na(across(everything()))) * 100, 1))
print(by_decade, n = 20)
