# MAIN SCRIPT - Run this to execute all analyses for 4 subgroups
# 1. HIV + vs HIV -
# 2. HIV + Cognitively Impaired vs HIV - Cognitively Normal
# 3. HIV + Cognitively Impaired vs HIV + Cognitively Normal
# 4. HIV + Viral Load Detectable vs HIV + Viral Load Undetectable

rm(list = ls())

source("./dataExtractandClean.R")
source("./rsfc_hivplusminus.R")
source("./rsfc_hpci_hmcn.R")
source("./rsfc_hpci_hpcn.R")
source("./rsfc_vld_vlnd.R")
