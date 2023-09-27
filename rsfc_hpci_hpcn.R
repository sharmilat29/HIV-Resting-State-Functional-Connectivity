### RSFC comparison between HIV+ Cognitively Impaired (HPCI) and HIV- Cognitively Normal (HPCN)

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

# Read input CSV files

hiv.plus <- read.csv("../data/hivplus.csv")

# Split the dataframe into two groups based on cognition status

hpci <- hiv.plus[hiv.plus$Impairment == 1, ]
hpci <- hpci[!is.na(hpci$Impairment), ]
hpcn <- hiv.plus[hiv.plus$Impairment == 0, ]
hpcn <- hpcn[!is.na(hpcn$Impairment), ]

# Combine both groups into one dataframe

df <- rbind(hpci, hpcn)
gds.info <- read.csv("../data/gds_domain_score.csv")
gds.info <- gds.info[gds.info$Missing_Total<4, ]

#Calculating domain scores
gds.info$Learning_DS <- rowMeans(gds.info[, c("HVLTLearn_GDS", "BVMTLearn_GDS")], na.rm = TRUE)
gds.info$DelayedRecall_DS <- rowMeans(gds.info[, c("HVLTDelay_GDS", "BVMTDelay_GDS")], na.rm = TRUE)
gds.info$ExecFunction_DS <- rowMeans(gds.info[, c("TrailsB_GDS", "LNS_GDS", "Verb_GDS", "CWIT3_GDS")], na.rm = TRUE)
gds.info$Psychomotor_DS <- rowMeans(gds.info[, c("TrailsA_GDS", "SS_GDS", "DS_GDS", "GPDom_GDS", "GPNonDom_GDS")], na.rm = TRUE)
gds.info$Language_DS <- rowMeans(gds.info[, c("Animals_GDS", "FAS_GDS")], na.rm = TRUE)

df1 <- merge(df, gds.info[, c("pid_scandate", "Total_GDS", "Learning_DS", "DelayedRecall_DS", "ExecFunction_DS", "Psychomotor_DS", "Language_DS")], by = "pid_scandate")

# Table 1 - Demographic information

table3.1 <- CreateTableOne(vars = c("Age", "Sex", "Race", "Education", "Duration_Of_Infection", "Recent_Viral_Load", "Total_GDS", "Learning_DS", "DelayedRecall_DS", "ExecFunction_DS", "Psychomotor_DS", "Language_DS"), data = df1, factorVars = c("Sex", "Race"), strata = "Impairment")

##########################################

# # Compute correlation matrices for both groups (optional: only used to generate correlation heatmaps)
# 
# hplus.ci <- get_correlation_matrix(hpci)
# hplus.cn <- get_correlation_matrix(hpcn)
# 
# write.csv(x = hplus.ci, file = "../data/hplus_CI_CorrelationMatrix.csv", row.names = FALSE)
# write.csv(x = hplus.cn, file = "../data/hplus_CN_CorrelationMatrix.csv", row.names = FALSE)
# 
# # Save plots to file
# 
# pdf(file = "../figures/2_1_2022/3_hplus_ci_corr.pdf")
# corrplot(hplus.ci, method = "color", title = "HIV+ Cognitively Impaired", mar = c(0,0,1,0))
# dev.off()
# pdf(file = "../figures/2_1_2022/3_hplus_cn_corr.pdf")
# corrplot(hplus.cn, method = "color", title = "HIV+ Cognitively Normal", mar = c(0,0,1,0))
# dev.off()

##########################################

# Run main analysis

result_hpci.hpcn <- run_analysis(df1, "Impairment")

# Generate figures

if(result_hpci.hpcn != 0) 
{
    pdf(file = "../output/3_hplus_ci_cn.pdf")
    hpci.hpcn_barplot <- generate_barplots(df, "Impairment", result_hpci.hpcn, alpha, "Impairment status")
    dev.off()
}

density_plot <- ggplot(df1, aes(x = Total_GDS)) + geom_histogram(bins = 40, color = "black", fill = "slategray2") + xlim(0, 5)
density_plot <- density_plot + geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray48") + theme_minimal()
density_plot <- density_plot + xlab("GDS Score") + ylab("Number of subjects")
print(density_plot)
