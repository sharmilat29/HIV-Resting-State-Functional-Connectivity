library(plyr)
library(dplyr)

rm(list = ls())

df1 <- read.csv("../data/composite_matrices_2_1_2022_new.csv")
df2 <- read.csv("../data/rsfc_data_new.csv")
df3 <- read.csv("../data/rsfc_scannerInfo.csv")
df4 <- read.csv("../data/hiv_gds_new.csv")
df5 <- read.csv("../data/extra_clinical.csv")
df6 <- read.csv("../data/gds_domain_score.csv")

#study <- read.csv("../data/HIV_Study_Info.csv")

# Merging dataframes 

df45 <- merge(df4, df5, by = "pid_scandate", all.x = TRUE)

df21 <- merge(df2, df1, by = "pid_scandate", all.y = TRUE)

df321 <- merge(df3, df21, by = "pid_scandate", all.y = TRUE)

df4321 <- merge(df45, df321, by = "pid_scandate", all.y = TRUE)

write.csv(df4321, "../data/hiv_masterList_notCleaned.csv")

# Removing non-trio data

#hiv.trio <- df4321[df4321$Scanner %in% c("TRIO", "TrioTim"), ]

hiv.trio <- df4321

# Removing longitudinal data

hiv.one.entry <- ddply(hiv.trio, "study_id", function(d1) return(d1[d1$Age==max(d1$Age), ]))
hiv.total <- distinct(hiv.one.entry, study_id, .keep_all = TRUE)

# Validating values per column

h1 <- subset(hiv.total, Impaired_GDS == 0 | Impaired_GDS == 1)

h1 <- subset(h1, Sex == 0 | Sex == 1)

h1 <- subset(h1, Race >= 0 & Race < 10)

h1 <- subset(h1, !is.na(SM_x_SM))

masterData <- subset(h1, Education >= 0 & Education < 30)

names(masterData)[names(masterData) == 'Impaired_GDS'] = 'Impairment'

# Splitting the masterdata by HIV status

hiv.plus <- masterData[masterData$hiv_status == 1, ]
hiv.minus <- masterData[masterData$hiv_status == 0, ]

# Save all dataframes as CSV files

write.csv(hiv.plus, "../data/hivplus.csv")
write.csv(hiv.minus, "../data/hivminus.csv")
write.csv(masterData, "../data/masterData.csv")


df_final <- merge(masterData, df6, by = "pid_scandate", all.x = TRUE)

hplus_final <- merge(hiv.plus, df6, by = "pid_scandate", all.x = TRUE)

cartData <- read.csv("../data/cart_rsfc.csv")

hplus_cart <- merge(hplus_final, cartData, by = "pid_scandate", all.x = TRUE)

# Validate time_on_cart < time_of_infection

hp_cart <- subset(hplus_cart, hplus_cart$duration_of_infection >= hplus_cart$duration_of_art)

