## code to prepare `AllWhalesLungeTableTrunc` dataset goes here
library(tidyverse)

LungeBeatsTable <- read_csv("data-raw/LungeBeatsTable.csv")

AllWhalesLungeTableTrunc <- read_csv("data-raw/AllWhalesLungeTableTrunc.csv")

AllWhalesLungeTableTrunc$FinalGyroOsFreq = LungeBeatsTable$FinalGyroOsFreq
AllWhalesLungeTableTrunc$EnCostMSByUSquared = (AllWhalesLungeTableTrunc$EnCostkJ / AllWhalesLungeTableTrunc$MassSKR) / (AllWhalesLungeTableTrunc$SpdBegDecel^2)

usethis::use_data(AllWhalesLungeTableTrunc, overwrite = TRUE)
