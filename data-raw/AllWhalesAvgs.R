## code to prepare `AllWhalesAvgs` dataset goes here
library(tidyverse)

AllWhalesAvgs <- AllWhalesLungeTableTrunc %>%
  group_by(Species,whaleName) %>%
  summarize(Location = Location,
            meanDepth = mean(depth),
            meanLengthAcc = mean(LengthAcc),
            meanLengthDecel = mean(LengthDecel),
            meanTotLength = mean(TotLength),
            meanSpdBegDecel = mean(SpdBegDecel),
            meanPeriod = mean(Period),
            meanEnCostMS = mean(EnCostMS),
            meanEnCostkJ = mean(EnCostkJ),
            meanEnPerLungeShirel = mean(EnPerLungeShirel),
            meanModelWaterEngulfFullShirel = mean(ModelWaterEngulfFullShirel),
            meanEnRatioShirel = mean(EnRatioShirel),
            StrokesPerAccel = (mean(LengthAcc)/mean(Period))) # %>%
  # select(-LungeI,-timeoflunge)

usethis::use_data(AllWhalesAvgs, overwrite = TRUE)
