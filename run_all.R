# Runs the whole pipeline in order. Run from the project root:
#   Rscript run_all.R
# Packages needed: dplyr, tidyr, ggplot2, ggrepel, corrplot, countrycode, factoextra, scales

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

scripts <- c("R/01_load_inspect.R", "R/02_cleaning.R", "R/03_outliers.R",
             "R/04_eda.R", "R/05_encoding.R", "R/06_pca.R")

for (s in scripts) {
  cat("\n==========", s, "==========\n")
  source(s, echo = FALSE)
}
cat("\nDone - figures are in outputs/figures/\n")
