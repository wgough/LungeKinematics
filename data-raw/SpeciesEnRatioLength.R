## code to prepare `SpeciesEnRatioLength` dataset goes here
library(tidyverse)

AllSpeciesEnRatioAvgs <- summarySE(AllWhalesAvgs, measurevar="meanEnRatioDirectAdjust", groupvars=c("Species"))

SpeciesEnRatioLength <- LungeKinematics::AllWhalesAvgs %>%
  group_by(Species) %>%
  summarize(meanMass = mean(meanMassSKR)) %>%
  left_join(AllSpeciesEnRatioAvgs, "Species") %>%
  mutate(quartratio25 = meanEnRatioDirectAdjust_mean-(0.5*iqr),
         quartratio75 = meanEnRatioDirectAdjust_mean+(0.5*iqr))

usethis::use_data(SpeciesEnRatioLength, overwrite = TRUE)


