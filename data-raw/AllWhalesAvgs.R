## code to prepare `AllWhalesAvgs` dataset goes here
library(tidyverse)

AllWhalesAvgs <- AllWhalesLungeTableTrunc %>%
  group_by(Species,whaleName) %>%
  summarize(meanDepth = mean(depth),
            meanLengthAcc = mean(LengthAcc),
            meanLengthDecel = mean(LengthDecel),
            meanLengthLunge = mean(LengthLunge),
            meanMassSKR = mean(MassSKR),
            meanTotLength = mean(TotLength),
            meanLengthVGB = mean(LengthVGB),
            meanE2E = mean(E2E),
            meanR2BH = mean(R2BH),
            meanJawAreaDirectMeasure = mean(JawAreaDirectMeasure),
            meanJawAreaTriangleApprox = mean(JawAreaTriangleApprox),
            meanPitchBegDecel = mean(PitchBegDecel),
            meanSpdBegDecel = mean(SpdBegDecel),
            meanSpdEndDecel = mean(SpdEndDecel),
            meanSpdEndDecelPotvin = mean(SpdEndDecel),
            meanDecelSpdLoss = mean(DecelSpdLoss),
            meanSlpDecel = mean(SlpDecel),
            meanEnCostMS = mean(EnCostMS),
            meanEnCostkJ = mean(EnCostkJ),
            meanEnPerLunge = mean(EnPerLunge),
            meanEnPerLungeMS = mean(EnPerLungeMS),
            meanEnPerVpos = mean(EnPerVpos),
            meanEnPerVant = mean(EnPerVant),
            meanModelWaterEngulfOpening = mean(ModelWaterEngulfOpening),
            meanModelWaterEngulfClosing = mean(ModelWaterEngulfClosing),
            meanModelWaterEngulfFull = mean(ModelWaterEngulfFull),
            meanVposShirel = mean(VposShirel),
            meanVantShirel = mean(VantShirel),
            meanVtotShirelDroneMorphs = mean(VtotShirelDroneMorphs),
            meanModelWaterEngulfFullShirel = mean(ModelWaterEngulfFullShirel),
            meanPosteriorWaterComparePercent = (mean(ModelWaterEngulfOpening) - mean(VposShirel)) / mean(ModelWaterEngulfOpening) * 100,
            meanAnteriorWaterComparePercent = (mean(ModelWaterEngulfClosing) - mean(VantShirel)) / mean(ModelWaterEngulfClosing) * 100,
            meanFullWaterComparePercent = (mean(ModelWaterEngulfFull) - mean(ModelWaterEngulfFullShirel)) / mean(ModelWaterEngulfFull) * 100,
            meanEngulfDistanceTravelled = mean(EngulfDistanceTravelled),
            meanDistancePerVGBLength = mean(DistancePerVGBLength),
            meanDeceleration = mean(Deceleration),
            meanEngulfmentDragPotvinFormula = mean(EngulfmentDrag),
            meanShapeDragMinusThrustPotvinFormula = mean(ShapeDragMinusThrust),
            meanShapeMinusThrustOverEngulfment = (mean(ShapeDragMinusThrust)/mean(EngulfmentDrag)),
            meanEnRatio = mean(EnRatio),
            meanEnRatioDive = mean(EnRatioDive),
            meanEnRatioDay = mean(EnRatioDay),
            meanVminPotvin = mean(Vmin),
            meanEnCostMSByUSquared = mean(EnCostMSByUSquared),
            meanFinalOscillation = mean(FinalGyroOsFreq, na.rm = TRUE))

write.table(AllWhalesAvgs, file = "C:/Users/wgoug/Documents/Academic Materials/GitHub Repositories/Chapter 3 - Lunge Kinematics/Figures/Outputs For Tables/AllWhalesAvgs.csv", sep = ",")

usethis::use_data(AllWhalesAvgs, overwrite = TRUE)

