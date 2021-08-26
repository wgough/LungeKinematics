## code to prepare `EnergeticsLength` dataset goes here
library(tidyverse)

AllWhalesWaterEngulfSummary <- summarySE(LungeKinematics::AllWhalesLungeTableTrunc, measurevar="ModelWaterEngulfFullDirectAdjust", groupvars=c("whaleName"))

MWaterLength <- LungeKinematics::AllWhalesAvgs %>%
  select(Species, whaleName, meanTotLength) %>%
  left_join(AllWhalesWaterEngulfSummary, MWaterLength, by = "whaleName") %>%
  mutate(quartwater25 = ModelWaterEngulfFullDirectAdjust_mean-(0.5*iqr),
         quartwater75 = ModelWaterEngulfFullDirectAdjust_mean+(0.5*iqr))

usethis::use_data(MWaterLength, overwrite = TRUE)


