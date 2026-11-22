# ------------------------------------------------------------------
# 05_encoding.R
# Step 5 - encode the categorical variables
#
# Two categorical features:
#   continent    - nominal (no natural order) -> ONE-HOT encoding
#   income_band  - ordinal (Low < Lower-middle < Upper-middle < High)
#                  -> LABEL encoding 1,2,3,4 so the order is kept
#
# income_band is built from GDP per capita using the yearly quartiles,
# so "High" means top 25% of countries in that year.
# Output: data/processed/energy_encoded.csv
# ------------------------------------------------------------------

library(dplyr)

energy <- read.csv("data/processed/energy_clean_logged.csv", stringsAsFactors = FALSE)

# ---- build the ordinal income band (per year, so inflation/growth
#      over time doesn't push every country into "High" by 2022) ----
energy <- energy %>%
  group_by(year) %>%
  mutate(income_band = cut(gdp_per_capita,
                           breaks = quantile(gdp_per_capita, c(0, .25, .5, .75, 1)),
                           labels = c("Low", "Lower-middle", "Upper-middle", "High"),
                           include.lowest = TRUE)) %>%
  ungroup()

# ---- LABEL encoding (ordinal) ----
energy$income_code <- as.integer(energy$income_band)   # Low=1 ... High=4
cat("Label encoding of income_band:\n")
print(unique(energy[, c("income_band", "income_code")]) %>% arrange(income_code))

# ---- ONE-HOT encoding (nominal) ----
# model.matrix with "- 1" gives one 0/1 column per continent
# (no reference level dropped, which is fine for PCA / EDA)
onehot <- model.matrix(~ continent - 1, data = energy)
colnames(onehot) <- gsub("continent", "cont_", colnames(onehot))
energy <- cbind(energy, onehot)

cat("\nOne-hot columns added:", paste(colnames(onehot), collapse = ", "), "\n")
cat("Every row has exactly one continent flag:", all(rowSums(onehot) == 1), "\n")
print(head(energy[, c("country", "continent", colnames(onehot))], 5))

write.csv(energy, "data/processed/energy_encoded.csv", row.names = FALSE)
cat("\nEncoded data:", nrow(energy), "rows x", ncol(energy), "columns\n")
