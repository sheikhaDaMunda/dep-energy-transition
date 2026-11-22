# ------------------------------------------------------------------
# 04_eda.R
# Step 4 - exploratory data analysis
#
# Questions this script tries to answer:
#   Q1. How has the world's electricity mix changed from 2000 to 2022?
#   Q2. Do richer countries use more energy per person? (covariation)
#   Q3. Do richer countries have greener electricity?
#   Q4. Which continents have cut the carbon intensity of their power most?
#   Q5. Which variables move together? (correlation matrix)
# All figures go to outputs/figures/
# ------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(corrplot)

energy <- read.csv("data/processed/energy_clean_logged.csv", stringsAsFactors = FALSE)
theme_set(theme_minimal(base_size = 16))
cont_cols <- c(Africa = "#e6a817", Americas = "#d62728", Asia = "#2ca02c",
               Europe = "#1f77b4", Oceania = "#9467bd")

# ---- quick summary statistics (variation of each feature) ----
summary(energy[, c("gdp_per_capita", "energy_per_capita", "renewables_share_elec",
                   "fossil_share_elec", "carbon_intensity_elec")])

# ================= Q1: global electricity mix ======================
# weight every country's shares by how much electricity it generates,
# otherwise Tuvalu would count as much as China
mix <- energy %>%
  mutate(other_renew = pmax(renewables_share_elec - hydro_share_elec -
                              solar_share_elec - wind_share_elec, 0)) %>%
  group_by(year) %>%
  summarise(across(c(coal_share_elec, gas_share_elec, oil_share_elec,
                     nuclear_share_elec, hydro_share_elec, wind_share_elec,
                     solar_share_elec, other_renew),
                   ~ weighted.mean(.x, electricity_generation))) %>%
  pivot_longer(-year, names_to = "source", values_to = "share") %>%
  mutate(source = gsub("_share_elec", "", source),
         source = factor(source, levels = c("coal", "oil", "gas", "nuclear",
                                            "hydro", "other_renew", "wind", "solar")))

p1 <- ggplot(mix, aes(year, share, fill = source)) +
  geom_area(colour = "white", linewidth = 0.2) +
  scale_fill_manual(values = c(coal = "#4d4d4d", oil = "#8c564b", gas = "#bcbd22",
                               nuclear = "#9467bd", hydro = "#1f77b4",
                               other_renew = "#17becf", wind = "#2ca02c",
                               solar = "#ffbf00")) +
  labs(title = "Q1. Global electricity mix (generation-weighted)",
       x = NULL, y = "% of electricity", fill = NULL)
ggsave("outputs/figures/fig2_global_mix.png", p1, width = 9, height = 5.5, dpi = 300, bg = "white")

ren <- mix %>% filter(source %in% c("hydro", "other_renew", "wind", "solar")) %>%
  group_by(year) %>% summarise(r = sum(share))
cat("\nQ1 renewables share of world electricity: 2000 =",
    round(ren$r[ren$year == 2000], 1), "%, 2022 =", round(ren$r[ren$year == 2022], 1), "%\n")
cat("   wind+solar 2000 vs 2022:",
    round(sum(mix$share[mix$year == 2000 & mix$source %in% c("wind", "solar")]), 1), "->",
    round(sum(mix$share[mix$year == 2022 & mix$source %in% c("wind", "solar")]), 1), "%\n")

# ================= Q2: wealth vs energy use ========================
r_q2 <- cor(energy$log_gdp_pc, energy$log_energy_pc)
cat("\nQ2 Pearson r (log GDP pc vs log energy pc):", round(r_q2, 2), "\n")

latest <- energy %>% filter(year == 2022)
label_q2 <- c("Qatar", "Iceland", "Trinidad and Tobago", "Niger", "Ethiopia",
              "United States", "China", "India", "Pakistan", "Switzerland")
p2 <- ggplot(latest, aes(gdp_per_capita, energy_per_capita, colour = continent)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(aes(group = 1), method = "lm", colour = "black", se = FALSE, linewidth = 0.7) +
  geom_text_repel(data = filter(latest, country %in% label_q2),
                  aes(label = country), size = 4.5, show.legend = FALSE) +
  scale_x_log10(labels = scales::label_comma()) +
  scale_y_log10(labels = scales::label_comma()) +
  scale_colour_manual(values = cont_cols) +
  labs(title = sprintf("Q2. Richer countries use more energy (r = %.2f)", r_q2),
       x = "GDP per capita (int-$, log)", y = "Energy per person (kWh, log)", colour = NULL)
ggsave("outputs/figures/fig3_gdp_vs_energy.png", p2, width = 9, height = 6, dpi = 300, bg = "white")

# ================= Q3: wealth vs renewables ========================
r_q3 <- cor(energy$log_gdp_pc, energy$renewables_share_elec)
cat("Q3 Pearson r (log GDP pc vs renewables share):", round(r_q3, 2), "\n")

label_q3 <- c("Iceland", "Norway", "Paraguay", "Ethiopia", "Democratic Republic of Congo",
              "Qatar", "Saudi Arabia", "Kuwait", "Nepal", "Germany", "Pakistan", "South Africa")
p3 <- ggplot(latest, aes(gdp_per_capita, renewables_share_elec, colour = continent)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(data = filter(latest, country %in% label_q3),
                  aes(label = country), size = 4.5, show.legend = FALSE, max.overlaps = 20) +
  scale_x_log10(labels = scales::label_comma()) +
  scale_colour_manual(values = cont_cols) +
  labs(title = sprintf("Q3. Wealth does NOT predict green power (r = %.2f)", r_q3),
       x = "GDP per capita (int-$, log)", y = "Renewables % of electricity", colour = NULL)
ggsave("outputs/figures/fig4_gdp_vs_renewables.png", p3, width = 9, height = 6, dpi = 300, bg = "white")

cat("   Countries with >90% renewable electricity in 2022:",
    sum(latest$renewables_share_elec > 90), "\n")
cat("   ...of which in Africa:",
    sum(latest$renewables_share_elec > 90 & latest$continent == "Africa"), "\n")

# ================= Q4: carbon intensity by continent ===============
ci <- energy %>% group_by(continent, year) %>%
  summarise(ci = weighted.mean(carbon_intensity_elec, electricity_generation), .groups = "drop")
p4 <- ggplot(ci, aes(year, ci, colour = continent)) +
  geom_line(linewidth = 1.4) +
  scale_colour_manual(values = cont_cols) +
  labs(title = "Q4. Carbon intensity of electricity by continent",
       x = NULL, y = "gCO2 per kWh (generation-weighted)", colour = NULL)
ggsave("outputs/figures/fig5_carbon_intensity.png", p4, width = 9, height = 5.5, dpi = 300, bg = "white")

ci_change <- ci %>% filter(year %in% c(2000, 2022)) %>%
  pivot_wider(names_from = year, values_from = ci) %>%
  mutate(pct_change = round((`2022` - `2000`) / `2000` * 100, 1))
cat("\nQ4 change in carbon intensity 2000 -> 2022:\n"); print(ci_change)

# ================= Q5: correlation matrix ==========================
corr_vars <- energy %>%
  select(log_gdp_pc, log_energy_pc, log_elec_pc, log_population,
         carbon_intensity_elec, coal_share_elec, gas_share_elec, oil_share_elec,
         nuclear_share_elec, hydro_share_elec, solar_share_elec, wind_share_elec)
cm <- cor(corr_vars)
png("outputs/figures/fig6_correlation.png", width = 9, height = 8, units = "in", res = 300)
corrplot(cm, method = "color", type = "lower", addCoef.col = "black",
         number.cex = 0.8, tl.col = "black", tl.cex = 1.1, tl.srt = 45,
         col = colorRampPalette(c("#b2182b", "white", "#2166ac"))(200),
         title = "Q5. Correlation matrix", mar = c(0, 0, 2, 0))
dev.off()

cat("\nStrongest correlations:\n")
cm_l <- as.data.frame(as.table(cm)) %>% filter(as.character(Var1) < as.character(Var2)) %>%
  arrange(desc(abs(Freq)))
print(head(cm_l, 6))
