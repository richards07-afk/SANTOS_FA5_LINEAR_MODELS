# ============================================================
# FA 5 - LINEAR MODELS: Palmer Penguins
# Reproducible analysis for Parts A-G
# Run from the repository root: source("R/FA5_Linear_Models.R")
# Base R only; no additional packages are required.
# ============================================================

options(stringsAsFactors = FALSE, scipen = 999)

dir.create("figures", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

# A. Data preparation -------------------------------------------------
data_file <- "data/penguins.csv"
if (!file.exists(data_file)) {
  stop("Cannot find data/penguins.csv. Run this script from the repository root.")
}
penguins <- read.csv(data_file, na.strings = c("", "NA"))

cat("STRUCTURE OF RAW DATA\\n")
str(penguins)
cat("\\nSUMMARY OF RAW DATA\\n")
print(summary(penguins))
cat("\\nMISSING VALUES BY VARIABLE\\n")
print(colSums(is.na(penguins)))

# Keep rows with non-missing response and both explanatory variables.
penguins_clean <- subset(
  penguins,
  !is.na(body_mass_g) & !is.na(species) & !is.na(sex)
)
penguins_clean$species <- factor(
  penguins_clean$species,
  levels = c("Adelie", "Chinstrap", "Gentoo")
)
penguins_clean$sex <- factor(
  penguins_clean$sex,
  levels = c("female", "male")
)

cat("\\nFINAL SAMPLE SIZE:", nrow(penguins_clean), "\\n")
cat("ROWS REMOVED:", nrow(penguins) - nrow(penguins_clean), "\\n")
write.csv(penguins_clean, "output/penguins_clean.csv", row.names = FALSE)

# B. Exploratory data analysis ---------------------------------------
overall_stats <- data.frame(
  n = sum(!is.na(penguins_clean$body_mass_g)),
  mean = mean(penguins_clean$body_mass_g),
  sd = sd(penguins_clean$body_mass_g),
  min = min(penguins_clean$body_mass_g),
  max = max(penguins_clean$body_mass_g)
)
species_stats <- do.call(rbind, lapply(split(penguins_clean$body_mass_g,
                                             penguins_clean$species), function(x) {
  c(n = length(x), mean = mean(x), sd = sd(x))
}))
sex_stats <- do.call(rbind, lapply(split(penguins_clean$body_mass_g,
                                         penguins_clean$sex), function(x) {
  c(n = length(x), mean = mean(x), sd = sd(x))
}))
cell_stats <- aggregate(
  body_mass_g ~ species + sex, data = penguins_clean,
  FUN = function(x) c(n = length(x), mean = mean(x), sd = sd(x))
)
cat("\\nOVERALL DESCRIPTIVE STATISTICS\\n"); print(overall_stats)
cat("\\nDESCRIPTIVE STATISTICS BY SPECIES (n, mean, SD)\\n"); print(species_stats)
cat("\\nDESCRIPTIVE STATISTICS BY SEX (n, mean, SD)\\n"); print(sex_stats)
cat("\\nDESCRIPTIVE STATISTICS BY SPECIES AND SEX\\n"); print(cell_stats)

png("figures/body_mass_by_species.png", width = 1200, height = 800, res = 140)
boxplot(body_mass_g ~ species, data = penguins_clean,
        xlab = "Species", ylab = "Body mass (g)",
        main = "Penguin Body Mass by Species", col = "grey90")
dev.off()

png("figures/body_mass_by_sex.png", width = 1200, height = 800, res = 140)
boxplot(body_mass_g ~ sex, data = penguins_clean,
        xlab = "Sex", ylab = "Body mass (g)",
        main = "Penguin Body Mass by Sex", col = "grey90")
dev.off()

means <- aggregate(body_mass_g ~ species + sex, data = penguins_clean, FUN = mean)
png("figures/species_sex_interaction.png", width = 1200, height = 800, res = 140)
interaction.plot(
  x.factor = means$species, trace.factor = means$sex,
  response = means$body_mass_g, fun = mean, type = "b",
  pch = c(16, 17), lty = c(1, 2),
  xlab = "Penguin Species", ylab = "Mean Body Mass (g)",
  main = "Species × Sex Interaction in Mean Body Mass",
  trace.label = "Sex"
)
dev.off()

# C. One-way ANOVA ----------------------------------------------------
oneway_model <- aov(body_mass_g ~ species, data = penguins_clean)
oneway_table <- summary(oneway_model)[[1]]
cat("\\nONE-WAY ANOVA: BODY MASS BY SPECIES\\n")
print(oneway_table)
write.csv(as.data.frame(oneway_table), "output/oneway_anova.csv")

# D. Dummy-variable regression ---------------------------------------
# Adelie is the reference category.
dummy_model <- lm(body_mass_g ~ species, data = penguins_clean)
cat("\\nDUMMY-VARIABLE REGRESSION (REFERENCE = ADELIE)\\n")
print(summary(dummy_model))
write.csv(coef(summary(dummy_model)), "output/dummy_regression_coefficients.csv")

# E. Two-way ANOVA with interaction ----------------------------------
twoway_model <- aov(body_mass_g ~ species * sex, data = penguins_clean)
twoway_table <- summary(twoway_model)[[1]]
cat("\\nTWO-WAY ANOVA WITH INTERACTION\\n")
print(twoway_table)
write.csv(as.data.frame(twoway_table), "output/twoway_anova.csv")

# F/G. Full regression model and interaction --------------------------
full_model <- lm(body_mass_g ~ species * sex, data = penguins_clean)
cat("\\nFULL REGRESSION MODEL: BODY MASS ~ SPECIES * SEX\\n")
print(summary(full_model))
cat("\\nANOVA TABLE FOR FULL MODEL\\n")
print(anova(full_model))
write.csv(coef(summary(full_model)), "output/full_model_coefficients.csv")
capture.output(summary(full_model), file = "output/full_model_summary.txt")
capture.output(anova(full_model), file = "output/full_model_anova.txt")

cat("\\nANALYSIS COMPLETE. See figures/ and output/.\\n")
