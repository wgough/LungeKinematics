## code to prepare `AllWhalesAvgs` dataset goes here
library(tidyverse)

ShirelFilterData <- read_csv("data-raw/CompleteLungesandDives.csv")

AllWhalesLungeTableWithFilter <- merge(ShirelFilterData, AllWhalesLungeTableTrunc, by=c("whaleName", "LungeI")) %>%
  mutate(EngulfFilt = LengthDecel+purge2) %>%
  mutate(ApproEngulfFilt = LengthAcc+LengthDecel+purge2) %>%
  mutate(EFRatio = EngulfFilt/Period)

AllWhalesAvgsWithFilt <- AllWhalesLungeTableWithFilter %>%
  group_by(Species,whaleName) %>%
  summarize(meanLengthDecel = mean(LengthDecel),
            meanPurge2 = mean(purge2),
            meanEngulfFilt = mean(EngulfFilt),
            meanTotLength = mean(TotLength.x),
            meanPeriod = mean(Period),
            meanEFRatio = mean(EFRatio),
            meanApproEngulfFilt = mean(ApproEngulfFilt),
            meanSpdBegDecel = mean(SpdBegDecel)) # %>%
  # select(-LungeI,-timeoflunge)

usethis::use_data(AllWhalesAvgsWithFilt, overwrite = TRUE)
