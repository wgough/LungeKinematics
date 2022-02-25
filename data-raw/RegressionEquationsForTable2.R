## code to prepare regression equation lines for manuscript table 1 (do not get used anywhere else)
library(tidyverse)
library(lme4)

m1 <- lmList(log10(EnCostkJ) ~ log10(SpdBegDecel) | Species, data = AllWhalesLungeTableTrunc)
summary(m1)
sapply(m1,function(x) summary(x)$r.squared)

m2 <- lmList(log10(EnCostMS) ~ log10(SpdBegDecel) | Species, data = AllWhalesLungeTableTrunc)
summary(m2)
sapply(m2,function(x) summary(x)$r.squared)

m3 <- lmList(log10(EnPerLunge) ~ log10(SpdBegDecel) | Species, data = AllWhalesLungeTableTrunc)
summary(m3)
sapply(m3,function(x) summary(x)$r.squared)

m4 <- lmList(log10(EnPerLungeMS) ~ log10(SpdBegDecel) | Species, data = AllWhalesLungeTableTrunc)
summary(m4)
sapply(m4,function(x) summary(x)$r.squared)

m5 <- lmList(log10(ModelWaterEngulfFull) ~ log10(SpdBegDecel) | Species, data = AllWhalesLungeTableTrunc)
summary(m5)
sapply(m5,function(x) summary(x)$r.squared)

m6 <- lmList(log10(EnRatio) ~ log10(SpdBegDecel) | Species, data = AllWhalesLungeTableTrunc)
summary(m6)
sapply(m6,function(x) summary(x)$r.squared)

m7 <- lmList(log10(ModelWaterEngulfFull) ~ log10(TotLength) | Species, data = AllWhalesLungeTableTrunc)
m7
sapply(m7,function(x) summary(x)$r.squared)

m7b <- lmList(log10(VtotShirelDroneMorphs) ~ log10(TotLength) | Species, data = AllWhalesLungeTableTrunc)
m7b
sapply(m7b,function(x) summary(x)$r.squared)

m7c <- lmList(log10(ModelWaterEngulfFullShirel) ~ log10(TotLength) | Species, data = AllWhalesLungeTableTrunc)
m7c
sapply(m7c,function(x) summary(x)$r.squared)

m8 <- lm(formula = log10(EnCostkJ) ~ log10(TotLength), data = AllWhalesLungeTableTrunc)
summary(m8)

m9 <- lm(formula = log10(EnPerLunge) ~ log10(TotLength), data = AllWhalesLungeTableTrunc)
summary(m9)

m10 <- lm(formula = log10(EnRatio) ~ log10(TotLength), data = AllWhalesLungeTableTrunc)
summary(m10)

m11 <- lm(formula = log10(EnRatioDive) ~ log10(TotLength), data = AllWhalesLungeTableTrunc)
summary(m11)

m12 <- lm(formula = log10(EnRatioDay) ~ log10(TotLength), data = AllWhalesLungeTableTrunc)
summary(m12)

AllWhalesAvgsNoFin <- filter(LungeKinematics::AllWhalesAvgs,Species!="Fin")

m13a <- lmList(log10(meanE2E) ~ log10(meanTotLength) | Species, data = AllWhalesAvgsNoFin)
summary(m13a)
sapply(m13a,function(x) summary(x)$r.squared)

m13b <- lmList(log10(BZigDR) ~ log10(TL) | Species, data = CleanedJawDataDR)
summary(m13b)
sapply(m13b,function(x) summary(x)$r.squared)

m14a <- lmList(log10(meanR2BH) ~ log10(meanTotLength) | Species, data = AllWhalesAvgsNoFin)
summary(m14a)
sapply(m14a,function(x) summary(x)$r.squared)

m14b <- lmList(log10(LjawDR) ~ log10(TL) | Species, data = CleanedJawDataDR)
summary(m14b)
sapply(m14b,function(x) summary(x)$r.squared)

m15a <- lmList(log10(meanJawAreaTriangleApprox) ~ log10(meanTotLength) | Species, data = AllWhalesAvgsNoFin)
summary(m15a)
sapply(m15a,function(x) summary(x)$r.squared)

m15b <- lmList(log10(meanJawAreaDirectMeasure) ~ log10(meanTotLength) | Species, data = AllWhalesAvgsNoFin)
summary(m15b)
sapply(m15b,function(x) summary(x)$r.squared)

m15c <- lmList(log10(JawAreaDR) ~ log10(TL) | Species, data = CleanedJawDataDR)
summary(m15c)
sapply(m15c,function(x) summary(x)$r.squared)

#Testing Out Other Options
mm1 <- lme(log(EnCostkJ) ~ log10(SpdBegDecel), random = ~ 1|Species/whaleName, data = AllWhalesLungeTableTrunc)
summary(mm1)

m7Shirel <- lm(formula = log10(ModelWaterEngulfFullShirel) ~ log10(TotLength), data = AllWhalesLungeTableTrunc)
summary(m7Shirel)
