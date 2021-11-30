## code to prepare `SpeciesEnRatioDiveLength` dataset goes here
library(tidyverse)

AllSpeciesEnRatioDiveAvgs <- summarySE(AllWhalesAvgs, measurevar="meanEnRatioDive", groupvars=c("Species"))

SpeciesEnRatioDiveLength <- LungeKinematics::AllWhalesAvgs %>%
  group_by(Species) %>%
  summarize(meanMass = mean(meanMassSKR)) %>%
  left_join(AllSpeciesEnRatioDiveAvgs, "Species") %>%
  mutate(quartratio25 = meanEnRatioDive_mean-(0.5*iqr),
         quartratio75 = meanEnRatioDive_mean+(0.5*iqr))

usethis::use_data(SpeciesEnRatioDiveLength, overwrite = TRUE)


