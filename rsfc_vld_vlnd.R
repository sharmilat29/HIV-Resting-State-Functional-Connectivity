### RSFC comparison between HIV+ Viral Load Detectable vs not Detectable (VLD vs VLND)

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

# Split the dataframe into two groups based on viral load detectable status

hpvld <- hiv.plus[hiv.plus$Recent_Viral_Load > 20, ]
hpvld <- hpvld[!is.na(hpvld$vl_detectable), ]
hpvlnd <- hiv.plus[hiv.plus$Recent_Viral_Load < 21, ]
hpvlnd <- hpvlnd[!is.na(hpvlnd$vl_detectable), ]

# Combine both groups into one dataframe

df <- rbind(hpvld, hpvlnd)
df[, 4] = df[, 4] / 12;  # converting duration of infection to # of years
#df[, 5] = log(df[,5], base = 10)

cd4cd8 <- read.csv("../data/cd4cd8.csv")

cd4cd8$ratio <- cd4cd8$recent_cd4 / cd4cd8$recent_cd8

df1 <- merge(df, cd4cd8, by = "pid_scandate", all.x = TRUE)

# Table 1 - Demographic information

table4.1 <- CreateTableOne(vars = c("Age", "Sex", "Race", "Education", "Duration_Of_Infection", "Recent_Viral_Load", "recent_cd4", "nadir_cd4", "ratio"), data = df1, factorVars = c("Sex", "Race"), strata = "vl_detectable")

##########################################

# # Compute correlation matrices for both groups (optional: only used to generate correlation heatmaps)
# 
# hplus.vld <- get_correlation_matrix(hpvld)
# hplus.vlnd <- get_correlation_matrix(hpvlnd)
# 
# write.csv(x = hplus.vld, file = "../data/hplus_VLD_CorrelationMatrix.csv", row.names = FALSE)
# write.csv(x = hplus.vlnd, file = "../data/hplus_VLND_CorrelationMatrix.csv", row.names = FALSE)
# 
# # Save plots to file
# 
# pdf(file = "../figures/2_1_2022/4_hplus_vld_corr.pdf")
# corrplot(hplus.vld, method = "color", title = "HIV+ Viral Load Detectable", mar = c(0,0,1,0))
# dev.off()
# pdf(file = "../figures/2_1_2022/4_hplus_vlnd_corr.pdf")
# corrplot(hplus.vlnd, method = "color", title = "HIV+ Viral Load Not Detectable", mar = c(0,0,1,0))
# dev.off()

##########################################


# Run main analysis

result_vld.vlnd <- run_analysis(df, "vl_detectable")

# Generate figures

if(result_vld.vlnd != 0) 
{
    pdf(file = "../output/4_hplus_vld_vlnd.pdf")
    vld.vlnd_barplot <- generate_barplots(df, "vl_detectable", result_vld.vlnd, alpha, "Viral load detectable")
    dev.off()
}

