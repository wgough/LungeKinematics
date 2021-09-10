## code to prepare species-level summarized datasets for manuscript table 1 (do not get used anywhere else)
library(tidyverse)

SpeciesTL <- summarySE(AllWhalesAvgs, measurevar="meanTotLength", groupvars=c("Species"))
SpeciesMass <- summarySE(AllWhalesAvgs, measurevar="meanMassSKR", groupvars=c("Species"))

SpeciesAccelLength <- summarySE(AllWhalesAvgs, measurevar="meanLengthAcc", groupvars=c("Species"))
SpeciesDecelLength <- summarySE(AllWhalesAvgs, measurevar="meanLengthDecel", groupvars=c("Species"))
SpeciesDecelStartSpeed <- summarySE(AllWhalesAvgs, measurevar="meanSpdBegDecel", groupvars=c("Species"))

SpeciesWaterEngulfed <- summarySE(AllWhalesAvgs, measurevar="meanModelWaterEngulfFullDirectAdjust", groupvars=c("Species"))
SpeciesEnergeticCost <- summarySE(AllWhalesAvgs, measurevar="meanEnCostkJ", groupvars=c("Species"))
SpeciesEnergeticGain <- summarySE(AllWhalesAvgs, measurevar="meanEnPerLungeDirectAdjust", groupvars=c("Species"))
SpeciesEnergeticRatio <- summarySE(AllWhalesAvgs, measurevar="meanEnRatioDirectAdjust", groupvars=c("Species"))
