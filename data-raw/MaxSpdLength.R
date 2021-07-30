## code to prepare `MaxSpdLength` dataset goes here
library(tidyverse)

AllWhalesMaxSpdSummary <- summarySE(LungeKinematics::AllWhalesLungeTableTrunc, measurevar="SpdBegDecel", groupvars=c("whaleName"))

MaxSpdLength <- LungeKinematics::AllWhalesAvgs %>%
  select(Species, whaleName, meanTotLength) %>%
  left_join(AllWhalesMaxSpdSummary, MaxSpdLength, by = "whaleName") %>%
  mutate(quartspd25 = SpdBegDecel_mean-(0.5*iqr),
         quartspd75 = SpdBegDecel_mean+(0.5*iqr))

usethis::use_data(MaxSpdLength, overwrite = TRUE)

