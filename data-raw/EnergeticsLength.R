## code to prepare `EnergeticsLength` dataset goes here
library(tidyverse)

AllWhalesEnergyOutAbsSummary <- summarySE(LungeKinematics::AllWhalesLungeTableTrunc, measurevar="EnCostkJ", groupvars=c("whaleName"))
AllWhalesEnergyOutMSSummary <- summarySE(LungeKinematics::AllWhalesLungeTableTrunc, measurevar="EnCostMS", groupvars=c("whaleName"))
AllWhalesEnergyInAbsSummary <- summarySE(LungeKinematics::AllWhalesLungeTableTrunc, measurevar="EnPerLunge", groupvars=c("whaleName"))
AllWhalesEnergyInMSSummary <- summarySE(LungeKinematics::AllWhalesLungeTableTrunc, measurevar="EnPerLungeMS", groupvars=c("whaleName"))
AllWhalesEnergyRatioLungeSummary <- summarySE(LungeKinematics::AllWhalesLungeTableTrunc, measurevar="EnRatio", groupvars=c("whaleName"))
AllWhalesEnergyRatioDiveSummary <- summarySE(LungeKinematics::AllWhalesLungeTableTrunc, measurevar="EnRatioDive", groupvars=c("whaleName"))
AllWhalesEnergyRatioDaySummary <- summarySE(LungeKinematics::AllWhalesLungeTableTrunc, measurevar="EnRatioDay", groupvars=c("whaleName"))

EnergyOutAbsLength <- LungeKinematics::AllWhalesAvgs %>%
  select(Species, whaleName, meanTotLength) %>%
  left_join(AllWhalesEnergyOutAbsSummary, EnergyOutAbsLength, by = "whaleName") %>%
  mutate(quartcost25 = EnCostkJ_mean-(0.5*iqr),
         quartcost75 = EnCostkJ_mean+(0.5*iqr))

EnergyOutMSLength <- LungeKinematics::AllWhalesAvgs %>%
  select(Species, whaleName, meanTotLength) %>%
  left_join(AllWhalesEnergyOutMSSummary, EnergyOutMSLength, by = "whaleName") %>%
  mutate(quartcost25 = EnCostMS_mean-(0.5*iqr),
         quartcost75 = EnCostMS_mean+(0.5*iqr))

EnergyInAbsLength <- LungeKinematics::AllWhalesAvgs %>%
  select(Species, whaleName, meanTotLength) %>%
  left_join(AllWhalesEnergyInAbsSummary, EnergyInAbsLength, by = "whaleName") %>%
  mutate(quartgain25 = EnPerLunge_mean-(0.5*iqr),
         quartgain75 = EnPerLunge_mean+(0.5*iqr))

EnergyInMSLength <- LungeKinematics::AllWhalesAvgs %>%
  select(Species, whaleName, meanTotLength) %>%
  left_join(AllWhalesEnergyInMSSummary, EnergyInMSLength, by = "whaleName") %>%
  mutate(quartgain25 = EnPerLungeMS_mean-(0.5*iqr),
         quartgain75 = EnPerLungeMS_mean+(0.5*iqr))

EnergyRatioLungeLength <- LungeKinematics::AllWhalesAvgs %>%
  select(Species, whaleName, meanTotLength) %>%
  left_join(AllWhalesEnergyRatioLungeSummary, EnergyRatioLungeLength, by = "whaleName") %>%
  mutate(quartratio25 = EnRatio_mean-(0.5*iqr),
         quartratio75 = EnRatio_mean+(0.5*iqr))

EnergyRatioDiveLength <- LungeKinematics::AllWhalesAvgs %>%
  select(Species, whaleName, meanTotLength) %>%
  left_join(AllWhalesEnergyRatioDiveSummary, EnergyRatioDiveLength, by = "whaleName") %>%
  mutate(quartratio25 = EnRatioDive_mean-(0.5*iqr),
         quartratio75 = EnRatioDive_mean+(0.5*iqr))

EnergyRatioDayLength <- LungeKinematics::AllWhalesAvgs %>%
  select(Species, whaleName, meanTotLength) %>%
  left_join(AllWhalesEnergyRatioDaySummary, EnergyRatioDayLength, by = "whaleName") %>%
  mutate(quartratio25 = EnRatioDay_mean-(0.5*iqr),
         quartratio75 = EnRatioDay_mean+(0.5*iqr))

usethis::use_data(EnergyOutAbsLength, overwrite = TRUE)
usethis::use_data(EnergyOutMSLength, overwrite = TRUE)
usethis::use_data(EnergyInAbsLength, overwrite = TRUE)
usethis::use_data(EnergyInMSLength, overwrite = TRUE)
usethis::use_data(EnergyRatioLungeLength, overwrite = TRUE)
usethis::use_data(EnergyRatioDiveLength, overwrite = TRUE)
usethis::use_data(EnergyRatioDayLength, overwrite = TRUE)


