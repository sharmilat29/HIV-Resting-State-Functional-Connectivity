### RSFC comparison between HIV+ Cognitively Impaired (HPCI) and HIV- Cognitively Normal (HMCN)

rm(list = ls())

library(ggplot2)
library(tidyr)
library(corrplot)
library(forcats)
library(tableone)

source("../functions/get_correlation_matrix.R")
source("../functions/lasso_regression.R")
source("../functions/covariate_correction.R")
source("../functions/anova_all.R")
source("../functions/run_analysis.R")
source("../functions/generate_barplots.R")

alpha <- 0.05

# Read input csv files

hiv.plus <- read.csv("../data/hivplus.csv")
hiv.minus <- read.csv("../data/hivminus.csv")
masterData <- read.csv("../data/masterData.csv")

# Filter groups based on Impaired_GDS value

hiv.plus.CI <- hiv.plus[(hiv.plus$Impairment == 1), ]
hiv.plus.CI <- hiv.plus.CI[!is.na(hiv.plus.CI$Impairment), ]

hiv.minus.CN <- hiv.minus[(hiv.minus$Impairment == 0), ]
hiv.minus.CN <- hiv.minus.CN[!is.na(hiv.minus.CN$Impairment), ]

# Combine both groups into one dataframe

df <- rbind(hiv.plus.CI, hiv.minus.CN)

# Table 1 - Demographic information

table2.1 <- CreateTableOne(vars = c("Age", "Sex", "Race", "Education"), data = df, factorVars = c("Sex", "Race"), strata = "hiv_status")

##########################################

# # Compute correlation matrices for both groups (optional: only used to generate correlation heatmaps)
# 
# hpci <- get_correlation_matrix(hiv.plus.CI)
# hmcn <- get_correlation_matrix(hiv.minus.CN)
# 
# write.csv(x = hpci, file = "../data/HPCI_CorrelationMatrix.csv", row.names = FALSE)
# write.csv(x = hmcn, file = "../data/HMCN_CorrelationMatrix.csv", row.names = FALSE)
# 
# # Save plots to file
# 
# pdf(file = "../output/2_hplus_hpci_corr.pdf")
# corrplot(hpci, method = "color", title = "HIV+", mar = c(0,0,1,0))
# dev.off()
# pdf(file = "../output/2_hminus_hmcn_corr.pdf")
# corrplot(hmcn, method = "color", title = "HIV-", mar = c(0,0,1,0))
# dev.off()

##########################################

# Run main analysis

result_hpci.hmcn <- run_analysis(df, "hiv_status")

# Generate figures

pdf(file = "../output/2_hpci_hmcn.pdf")

hpci.hmcn_barplot <- generate_barplots(df, "hiv_status", result_hpci.hmcn, alpha, "HIV status")

dev.off()


