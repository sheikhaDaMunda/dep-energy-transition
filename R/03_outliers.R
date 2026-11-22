# ------------------------------------------------------------------
# 03_outliers.R
# Step 3 - find outliers and decide what to do with them
#
# Method: 1.5 x IQR rule (Tukey) on the per-capita variables.
# Values outside [Q1 - 1.5*IQR, Q3 + 1.5*IQR] are flagged.
#
# Decision: the outliers here are real countries (Qatar, Iceland,
# Kuwait...) not typing errors, so deleting them would delete the most
# interesting part of the story. Instead the heavily skewed variables are
# log-transformed, which pulls the extremes in without losing them.
# Output: data/processed/energy_clean_logged.csv + boxplot figure
# ------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

energy <- read.csv("data/processed/energy_clean.csv", stringsAsFactors = FALSE)

iqr_flag <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr
}

check_vars <- c("gdp_per_capita", "energy_per_capita",
                "per_capita_electricity", "carbon_intensity_elec", "population")

outlier_summary <- sapply(energy[check_vars], function(x) sum(iqr_flag(x)))
cat("Number of IQR outliers per variable:\n")
print(outlier_summary)

# who are the extreme energy users? (latest year)
cat("\nTop energy users per person in 2022 (kWh):\n")
energy %>% filter(year == 2022) %>%
  arrange(desc(energy_per_capita)) %>%
  select(country, energy_per_capita, gdp_per_capita) %>%
  head(8) %>% print()

# skewness before / after log - shows why the log helps
skew <- function(x) mean((x - mean(x))^3) / sd(x)^3
cat("\nSkewness before -> after log10:\n")
for (v in check_vars[c(1, 2, 3, 5)]) {
  cat(sprintf("  %-24s %6.2f -> %5.2f\n", v, skew(energy[[v]]), skew(log10(energy[[v]] + 1))))
}

# ---- boxplots (standardised so they fit on one axis) ----
long <- energy %>%
  select(all_of(check_vars)) %>%
  mutate(across(everything(), ~ as.numeric(scale(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "z")

p <- ggplot(long, aes(x = reorder(variable, z, FUN = max), y = z)) +
  geom_boxplot(fill = "#9ecae1", outlier.colour = "#d62728", outlier.alpha = 0.4) +
  coord_flip() +
  labs(title = "Outliers before transformation (z-scores)",
       x = NULL, y = "Standardised value") +
  theme_minimal(base_size = 16)
ggsave("outputs/figures/fig1_outliers_boxplot.png", p, width = 9, height = 5, dpi = 300, bg = "white")

# ---- apply the log transform ----
energy <- energy %>%
  mutate(log_gdp_pc      = log10(gdp_per_capita),
         log_energy_pc   = log10(energy_per_capita + 1),
         log_elec_pc     = log10(per_capita_electricity + 1),
         log_population  = log10(population))

write.csv(energy, "data/processed/energy_clean_logged.csv", row.names = FALSE)
cat("\nSaved", nrow(energy), "rows with log columns added\n")
