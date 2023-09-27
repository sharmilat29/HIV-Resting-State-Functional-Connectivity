# RSFC comparison between HIV+ and HIV-

rm(list = ls())

library(ggplot2)
library(tidyr)
library(corrplot)
library(forcats)
library(tableone)

source("../functions/get_correlation_matrix.R")
source("../functions/covariate_correction.R")
source("../functions/lasso_regression.R")
source("../functions/anova_all.R")
source("../functions/run_analysis.R")
source("../functions/generate_barplots.R")

alpha <- 0.05

# Read input csv files

hiv.plus <- read.csv("../data/hivplus.csv")
hiv.minus <- read.csv("../data/hivminus.csv")
masterData <- read.csv("../data/masterData.csv")

# Table 1 - Demographic information

table1.1 <- CreateTableOne(vars = c("Age", "Sex", "Race", "Education"), data = masterData, factorVars = c("Sex", "Race"), strata = "hiv_status")

##########################################

# # Compute correlation matrices for both groups (optional: only used to generate correlation heatmaps)
# 
# hplus <- get_correlation_matrix(hiv.plus)
# hminus <- get_correlation_matrix(hiv.minus)
# 
# write.csv(x = hplus, file = "../data/new_hplusCorrelationMatrix.csv", row.names = FALSE)
# write.csv(x = hminus, file = "../data/new_hminusCorrelationMatrix.csv", row.names = FALSE)
# 
# # Save plots to file
# 
# pdf(file = "../output/1_hplus_corr.pdf")
# corrplot(hplus, method = "color", title = "HIV+", mar = c(0,0,1,0))
# dev.off()
# pdf(file = "../output/1_hminus_corr.pdf")
# corrplot(hminus, method = "color", title = "HIV-", mar = c(0,0,1,0))
# dev.off()

##########################################

# Run main analysis

result_hp.hm <- run_analysis(masterData, "hiv_status")

# Generate figures

pdf(file = "../output/1_hplusminus.pdf")
hp.hm_barplot <- generate_barplots(masterData, "hiv_status", result_hp.hm, alpha, "HIV status")
dev.off()

