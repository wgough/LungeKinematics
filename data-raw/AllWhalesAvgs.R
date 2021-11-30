## code to prepare `AllWhalesAvgs` dataset goes here
library(tidyverse)

AllWhalesAvgs <- AllWhalesLungeTableTrunc %>%
  group_by(Species,whaleName) %>%
  summarize(meanDepth = mean(depth),
            meanLengthAcc = mean(LengthAcc),
            meanLengthDecel = mean(LengthDecel),
            meanMassSKR = mean(MassSKR),
            meanTotLength = mean(TotLength),
            meanPitchBegDecel = mean(PitchBegDecel),
            meanSpdBegDecel = mean(SpdBegDecel),
            meanPeriod = mean(Period),
            meanEnCostMS = mean(EnCostMS),
            meanEnCostkJ = mean(EnCostkJ),
            meanEnPerLungeDirectAdjust = mean(EnPerLungeDirectAdjust),
            meanEnPerLungeDirectAdjustMS = mean(EnPerLungeDirectAdjustMS),
            meanModelWaterEngulfFullDirectAdjust = mean(ModelWaterEngulfFullDirectAdjust),
            meanModelWaterEngulfFullEllipse = mean(ModelWaterEngulfFullEllipse),
            meanModelWaterEngulfFullShirel = mean(ModelWaterEngulfFullShirel),
            meanEnRatioDirectAdjust = mean(EnRatioDirectAdjust),
            meanEnRatioDive = mean(EnRatioDive),
            meanEnRatioDay = mean(EnRatioDay),
            StrokesPerAccel = (mean(LengthAcc)/mean(Period)))

usethis::use_data(AllWhalesAvgs, overwrite = TRUE)

