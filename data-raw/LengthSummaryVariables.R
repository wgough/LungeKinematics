## code to prepare body length dependant summary datasets for use with geom_pointrange
library(tidyverse)

AllWhalesMaxSpdSummary <- summarySE(LungeKinematics::AllWhalesLungeTableTrunc, measurevar="SpdBegDecel", groupvars=c("whaleName"))
AllWhalesWaterEngulfSummary <- summarySE(LungeKinematics::AllWhalesLungeTableTrunc, measurevar="ModelWaterEngulfFull", groupvars=c("whaleName"))
AllWhalesDecelerationSummary <- summarySE(LungeKinematics::AllWhalesLungeTableTrunc, measurevar="Deceleration", groupvars=c("whaleName"))
AllWhalesTravelPerVGBSummary <- summarySE(LungeKinematics::AllWhalesLungeTableTrunc, measurevar="DistancePerVGBLength", groupvars=c("whaleName"))

MaxSpdLength <- LungeKinematics::AllWhalesAvgs %>%
  select(Species, whaleName, meanTotLength) %>%
  left_join(AllWhalesMaxSpdSummary, MaxSpdLength, by = "whaleName") %>%
  mutate(quartspd25 = SpdBegDecel_mean-(0.5*iqr),
         quartspd75 = SpdBegDecel_mean+(0.5*iqr))

MWaterLength <- LungeKinematics::AllWhalesAvgs %>%
  select(Species, whaleName, meanTotLength) %>%
  left_join(AllWhalesWaterEngulfSummary, MWaterLength, by = "whaleName") %>%
  mutate(quartwater25 = ModelWaterEngulfFull_mean-(0.5*iqr),
         quartwater75 = ModelWaterEngulfFull_mean+(0.5*iqr))

DecelerationLength <- LungeKinematics::AllWhalesAvgs %>%
  select(Species, whaleName, meanTotLength) %>%
  left_join(AllWhalesDecelerationSummary, DecelerationLength, by = "whaleName") %>%
  mutate(quartdecel25 = Deceleration_mean-(0.5*iqr),
         quartdecel75 = Deceleration_mean+(0.5*iqr))

DistancePerVGBLength <- LungeKinematics::AllWhalesAvgs %>%
  select(Species, whaleName, meanTotLength) %>%
  left_join(AllWhalesTravelPerVGBSummary, DistancePerVGBLength, by = "whaleName") %>%
  mutate(quartdist25 = DistancePerVGBLength_mean-(0.5*iqr),
         quartdist75 = DistancePerVGBLength_mean+(0.5*iqr))

usethis::use_data(MaxSpdLength, overwrite = TRUE)
usethis::use_data(MWaterLength, overwrite = TRUE)
usethis::use_data(DecelerationLength, overwrite = TRUE)
usethis::use_data(DistancePerVGBLength, overwrite = TRUE)

