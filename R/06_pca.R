# ------------------------------------------------------------------
# 06_pca.R
# Step 6 - Principal Component Analysis
#
# 13 numeric features go in: the log per-capita measures, the power mix
# shares, carbon intensity and the label-encoded income band.
# Left out on purpose:
#   - fossil_share_elec / renewables_share_elec, because they are just
#     sums of the individual sources (perfect multicollinearity)
#   - the one-hot continent columns, which are used to colour the plots
#     instead, so continent doesn't dominate the components
# All features are centred and scaled first, otherwise variables measured
# in big units (gCO2/kWh) would swamp the percentage shares.
# ------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(factoextra)

energy <- read.csv("data/processed/energy_encoded.csv", stringsAsFactors = FALSE)

pca_vars <- c("log_gdp_pc", "log_energy_pc", "log_elec_pc", "log_population",
              "carbon_intensity_elec", "coal_share_elec", "gas_share_elec",
              "oil_share_elec", "nuclear_share_elec", "hydro_share_elec",
              "solar_share_elec", "wind_share_elec", "income_code")
X <- energy[, pca_vars]

pca <- prcomp(X, center = TRUE, scale. = TRUE)

# ---- how much variance each component explains ----
eig <- get_eigenvalue(pca)
print(round(eig, 2))
n_keep <- sum(eig$eigenvalue > 1)          # Kaiser rule
cat("\nComponents with eigenvalue > 1 (Kaiser):", n_keep,
    "- together explain", round(eig$cumulative.variance.percent[n_keep], 1), "% of variance\n")
cat("Dimensions reduced from", length(pca_vars), "to", n_keep, "\n")

# ---- loadings: what each PC actually means ----
load <- round(pca$rotation[, 1:4], 2)
print(load)
for (k in 1:3) {
  top <- sort(abs(pca$rotation[, k]), decreasing = TRUE)[1:4]
  cat(sprintf("PC%d top loadings: %s\n", k,
              paste(sprintf("%s (%+.2f)", names(top), pca$rotation[names(top), k]), collapse = ", ")))
}

# ---- scree plot ----
p_scree <- fviz_eig(pca, addlabels = TRUE, ncp = 10, barfill = "#4c72b0",
                    barcolor = "#4c72b0", linecolor = "#d62728") +
  geom_hline(yintercept = 100 / length(pca_vars), linetype = "dashed") +
  labs(title = "Scree plot - variance explained per component") +
  theme_minimal(base_size = 16)
ggsave("outputs/figures/fig7_scree.png", p_scree, width = 9, height = 5.5, dpi = 300, bg = "white")

# ---- biplot of 2022 countries, coloured by continent ----
cont_cols <- c(Africa = "#e6a817", Americas = "#d62728", Asia = "#2ca02c",
               Europe = "#1f77b4", Oceania = "#9467bd")
idx22 <- which(energy$year == 2022)
scores22 <- as.data.frame(pca$x[idx22, 1:2])
scores22$continent <- energy$continent[idx22]
scores22$country   <- energy$country[idx22]

arrows <- as.data.frame(pca$rotation[, 1:2]) * 7      # stretch arrows so they are readable next to the points
arrows$var <- sub("carbon_intensity_elec", "carbon_intensity", gsub("_share_elec", "", rownames(arrows)))

label_pca <- c("Qatar", "Iceland", "Norway", "Ethiopia", "China", "India",
               "Pakistan", "France", "Germany", "Saudi Arabia", "Paraguay", "South Africa")
p_bi <- ggplot(scores22, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, colour = "grey80") + geom_vline(xintercept = 0, colour = "grey80") +
  geom_point(aes(colour = continent), size = 2.6, alpha = 0.75) +
  ggrepel::geom_text_repel(data = filter(scores22, country %in% label_pca),
                           aes(label = country), size = 4) +
  geom_segment(data = arrows, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")), colour = "grey25") +
  ggrepel::geom_label_repel(data = arrows, aes(x = PC1, y = PC2, label = var),
                            size = 3.8, fontface = "bold", fill = "white", alpha = 0.85,
                            label.size = 0, min.segment.length = 1) +
  scale_colour_manual(values = cont_cols) +
  labs(title = "PCA biplot (2022 countries)",
       x = sprintf("PC1 (%.1f%%)", eig$variance.percent[1]),
       y = sprintf("PC2 (%.1f%%)", eig$variance.percent[2]), colour = NULL) +
  theme_minimal(base_size = 16)
ggsave("outputs/figures/fig8_biplot.png", p_bi, width = 9, height = 7, dpi = 300, bg = "white")

# ---- do the components separate the continents? ----
cat("\nMean PC1 / PC2 score by continent:\n")
energy %>% mutate(PC1 = pca$x[, 1], PC2 = pca$x[, 2]) %>%
  group_by(continent) %>% summarise(PC1 = round(mean(PC1), 2), PC2 = round(mean(PC2), 2)) %>%
  print()

# save the reduced dataset (scores) for anyone who wants to model with it
reduced <- cbind(energy[, c("country", "year", "continent")], round(pca$x[, 1:n_keep], 4))
write.csv(reduced, "outputs/pca_scores.csv", row.names = FALSE)
