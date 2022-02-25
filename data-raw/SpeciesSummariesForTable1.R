## code to prepare species-level summarized datasets for manuscript table 1 (do not get used anywhere else)
library(tidyverse)

SpeciesTL <- summarySE(AllWhalesAvgs, measurevar="meanTotLength", groupvars=c("Species"))
SpeciesMass <- summarySE(AllWhalesAvgs, measurevar="meanMassSKR", groupvars=c("Species"))

write.table(SpeciesTL, file = "C:/Users/wgoug/Documents/Academic Materials/GitHub Repositories/Chapter 3 - Lunge Kinematics/Figures/Outputs For Tables/SpeciesTL.csv", sep = ",")
write.table(SpeciesMass, file = "C:/Users/wgoug/Documents/Academic Materials/GitHub Repositories/Chapter 3 - Lunge Kinematics/Figures/Outputs For Tables/SpeciesMass.csv", sep = ",")

SpeciesLungeLength <- summarySE(AllWhalesAvgs, measurevar="meanLengthLunge", groupvars=c("Species"))
SpeciesAccelLength <- summarySE(AllWhalesAvgs, measurevar="meanLengthAcc", groupvars=c("Species"))
SpeciesDecelLength <- summarySE(AllWhalesAvgs, measurevar="meanLengthDecel", groupvars=c("Species"))
SpeciesDecelStartSpeed <- summarySE(AllWhalesAvgs, measurevar="meanSpdBegDecel", groupvars=c("Species"))

write.table(SpeciesLungeLength, file = "C:/Users/wgoug/Documents/Academic Materials/GitHub Repositories/Chapter 3 - Lunge Kinematics/Figures/Outputs For Tables/SpeciesLungeLength.csv", sep = ",")
write.table(SpeciesAccelLength, file = "C:/Users/wgoug/Documents/Academic Materials/GitHub Repositories/Chapter 3 - Lunge Kinematics/Figures/Outputs For Tables/SpeciesAccelLength.csv", sep = ",")
write.table(SpeciesDecelLength, file = "C:/Users/wgoug/Documents/Academic Materials/GitHub Repositories/Chapter 3 - Lunge Kinematics/Figures/Outputs For Tables/SpeciesDecelLength.csv", sep = ",")
write.table(SpeciesDecelStartSpeed, file = "C:/Users/wgoug/Documents/Academic Materials/GitHub Repositories/Chapter 3 - Lunge Kinematics/Figures/Outputs For Tables/SpeciesDecelStartSpeed.csv", sep = ",")

SpeciesWaterEngulfed <- summarySE(AllWhalesAvgs, measurevar="meanModelWaterEngulfFull", groupvars=c("Species"))
SpeciesEnergeticCost <- summarySE(AllWhalesAvgs, measurevar="meanEnCostkJ", groupvars=c("Species"))
SpeciesEnergeticGain <- summarySE(AllWhalesAvgs, measurevar="meanEnPerLunge", groupvars=c("Species"))
SpeciesEnergeticRatio <- summarySE(AllWhalesAvgs, measurevar="meanEnRatio", groupvars=c("Species"))

write.table(SpeciesWaterEngulfed, file = "C:/Users/wgoug/Documents/Academic Materials/GitHub Repositories/Chapter 3 - Lunge Kinematics/Figures/Outputs For Tables/SpeciesWaterEngulfed.csv", sep = ",")
write.table(SpeciesEnergeticCost, file = "C:/Users/wgoug/Documents/Academic Materials/GitHub Repositories/Chapter 3 - Lunge Kinematics/Figures/Outputs For Tables/SpeciesEnergeticCost.csv", sep = ",")
write.table(SpeciesEnergeticGain, file = "C:/Users/wgoug/Documents/Academic Materials/GitHub Repositories/Chapter 3 - Lunge Kinematics/Figures/Outputs For Tables/SpeciesEnergeticGain.csv", sep = ",")
write.table(SpeciesEnergeticRatio, file = "C:/Users/wgoug/Documents/Academic Materials/GitHub Repositories/Chapter 3 - Lunge Kinematics/Figures/Outputs For Tables/SpeciesEnergeticRatio.csv", sep = ",")

SpeciesVmin <- summarySE(AllWhalesAvgs, measurevar="meanVminPotvin", groupvar=c("Species"))

write.table(SpeciesVmin, file = "C:/Users/wgoug/Documents/Academic Materials/GitHub Repositories/Chapter 3 - Lunge Kinematics/Figures/Outputs For Tables/SpeciesVmin.csv", sep = ",")

SpeciesEngulfmentDrag <- summarySE(AllWhalesAvgs, measurevar="meanEngulfmentDragPotvinFormula", groupvars=c("Species"))
SpeciesShapeDragMinusThrust <- summarySE(AllWhalesAvgs, measurevar="meanShapeDragMinusThrustPotvinFormula", groupvars=c("Species"))
SpeciesShapeMinusThrustOverEngulfment <- summarySE(AllWhalesAvgs, measurevar="meanShapeMinusThrustOverEngulfment", groupvars=c("Species"))

write.table(SpeciesEngulfmentDrag, file = "C:/Users/wgoug/Documents/Academic Materials/GitHub Repositories/Chapter 3 - Lunge Kinematics/Figures/Outputs For Tables/SpeciesEngulfmentDrag.csv", sep = ",")
write.table(SpeciesShapeDragMinusThrust, file = "C:/Users/wgoug/Documents/Academic Materials/GitHub Repositories/Chapter 3 - Lunge Kinematics/Figures/Outputs For Tables/SpeciesShapeDragMinusThrust.csv", sep = ",")
write.table(SpeciesShapeMinusThrustOverEngulfment, file = "C:/Users/wgoug/Documents/Academic Materials/GitHub Repositories/Chapter 3 - Lunge Kinematics/Figures/Outputs For Tables/SpeciesShapeMinusThrustOverEngulfment.csv", sep = ",")

SpeciesFinalOscillation <- summarySE(AllWhalesAvgs, measurevar="meanFinalOscillation", groupvars=c("Species"), na.rm = TRUE)

write.table(SpeciesFinalOscillation, file = "C:/Users/wgoug/Documents/Academic Materials/GitHub Repositories/Chapter 3 - Lunge Kinematics/Figures/Outputs For Tables/SpeciesFinalOscillation.csv", sep = ",")

SpeciesPosteriorWaterComparePercent <- summarySE(AllWhalesAvgs, measurevar="meanPosteriorWaterComparePercent", groupvars=c("Species"))
SpeciesAnteriorWaterComparePercent <- summarySE(AllWhalesAvgs, measurevar="meanAnteriorWaterComparePercent", groupvars=c("Species"))
SpeciesFullWaterComparePercent <- summarySE(AllWhalesAvgs, measurevar="meanFullWaterComparePercent", groupvars=c("Species"))

write.table(SpeciesPosteriorWaterComparePercent, file = "C:/Users/wgoug/Documents/Academic Materials/GitHub Repositories/Chapter 3 - Lunge Kinematics/Figures/Outputs For Tables/SpeciesPosteriorWaterComparePercent.csv", sep = ",")
write.table(SpeciesAnteriorWaterComparePercent, file = "C:/Users/wgoug/Documents/Academic Materials/GitHub Repositories/Chapter 3 - Lunge Kinematics/Figures/Outputs For Tables/SpeciesAnteriorWaterComparePercent.csv", sep = ",")
write.table(SpeciesFullWaterComparePercent, file = "C:/Users/wgoug/Documents/Academic Materials/GitHub Repositories/Chapter 3 - Lunge Kinematics/Figures/Outputs For Tables/SpeciesFullWaterComparePercent.csv", sep = ",")
