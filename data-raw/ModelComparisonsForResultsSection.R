## Hypothesis testing to compare regression slopes for Gough et al. 2022
WaterEngulfModelsMN <- filter(LungeKinematics::AllWhalesLungeTableTrunc,Species=="Humpback") %>%
  transmute(Species = Species,
            GoughModel = ModelWaterEngulfFull,
            ShirelModel = VtotShirelDroneMorphs,
            TotLength = TotLength) %>%
  pivot_longer(GoughModel:ShirelModel, names_to = "Models", values_to = "Volume")

m1MN <- lm(formula = Volume ~ TotLength + Models + TotLength*Models, data = WaterEngulfModelsMN)
summary(m1MN)

WaterEngulfModelsBB <- filter(LungeKinematics::AllWhalesLungeTableTrunc,Species=="Minke") %>%
  transmute(Species = Species,
            GoughModel = ModelWaterEngulfFull,
            ShirelModel = ModelWaterEngulfFullShirel,
            TotLength = TotLength) %>%
  pivot_longer(GoughModel:ShirelModel, names_to = "Models", values_to = "Volume")

m1BB <- lm(formula = Volume ~ TotLength + Models + TotLength*Models, data = WaterEngulfModelsBB)
summary(m1BB)

WaterEngulfModelsBW <- filter(LungeKinematics::AllWhalesLungeTableTrunc,Species=="Blue") %>%
  transmute(Species = Species,
            GoughModel = ModelWaterEngulfFull,
            ShirelModel = ModelWaterEngulfFullShirel,
            TotLength = TotLength) %>%
  pivot_longer(GoughModel:ShirelModel, names_to = "Models", values_to = "Volume")

m1BW <- lm(formula = Volume ~ TotLength + Models + TotLength*Models, data = WaterEngulfModelsBW)
summary(m1BW)

EnergyInOut <- LungeKinematics::AllWhalesLungeTableTrunc %>%
  transmute(Species = Species,
            EnergyCost = EnCostkJ,
            EnergyGain = EnPerLunge,
            TotLength = TotLength) %>%
  pivot_longer(EnergyCost:EnergyGain, names_to = "InOut", values_to = "Energy")

m2 <- lm(formula = Energy ~ TotLength + InOut + TotLength*InOut, data = EnergyInOut)
summary(m2)

LungeDive <- LungeKinematics::AllWhalesLungeTableTrunc %>%
  transmute(Species = Species,
            LungeRatio = EnRatio,
            DiveRatio = EnRatioDive,
            TotLength = TotLength) %>%
  pivot_longer(LungeRatio:DiveRatio, names_to = "LungeDive", values_to = "Ratio")

m3LUDI <- lm(formula = Ratio ~ TotLength + LungeDive + TotLength*LungeDive, data = LungeDive)
summary(m3LUDI)

DiveDay <- LungeKinematics::AllWhalesLungeTableTrunc %>%
  transmute(Species = Species,
            DiveRatio = EnRatioDive,
            DayRatio = EnRatioDay,
            TotLength = TotLength) %>%
  pivot_longer(DiveRatio:DayRatio, names_to = "DiveDay", values_to = "Ratio")

m3DIDA <- lm(formula = Ratio ~ TotLength + DiveDay + TotLength*DiveDay, data = DiveDay)
summary(m3DIDA)

LungeDay <- LungeKinematics::AllWhalesLungeTableTrunc %>%
  transmute(Species = Species,
            LungeRatio = EnRatio,
            DayRatio = EnRatioDay,
            TotLength = TotLength) %>%
  pivot_longer(LungeRatio:DayRatio, names_to = "LungeDay", values_to = "Ratio")

m3LUDA <- lm(formula = Ratio ~ TotLength + LungeDay + TotLength*LungeDay, data = LungeDay)
summary(m3LUDA)
