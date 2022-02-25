## Test Figures To Figure Out Energy In Flatline Relationship

pal <- c("Minke" = "#009E73",  "Humpback" = "#D55E00",  "Blue" = "#0072B2", "Fin" = "#9100B2", "Antarctic" = "Black", "Monterey" = "Gray")

p1 <- ggplot(filter(LungeKinematics::AllWhalesAvgs,Species=="Blue")) +
  geom_smooth(method = lm,
              aes(log10(meanTotLength), log10(meanEnCostkJ)),
              color = "black",
              se = TRUE) +
  geom_smooth(method = lm,
              aes(log10(meanTotLength), log10(meanEnPerLunge)),
              color = "black",
              linetype = 2,
              se = TRUE) +
  geom_point(aes(log10(meanTotLength), log10(meanEnCostkJ), fill = meanJawAreaDirectMeasure),
             color = "black",
             shape = 21,
             size = 10) +
  geom_point(aes(log10(meanTotLength), log10(meanEnPerLunge), fill = meanJawAreaDirectMeasure),
             color = "black",
             shape = 22,
             size = 10) +
  ggtitle("Jaw Area") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = bquote("Log"^10*" Body Length (m)"),
       y = bquote("Log"^10*" Lunge Energy (kJ)")) +
  theme_classic(base_size = 8) +
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 36),
        legend.position = "right",
        legend.title = element_blank(),
        panel.grid.minor = element_blank())

p2 <- ggplot(filter(LungeKinematics::AllWhalesAvgs,Species=="Blue")) +
  geom_smooth(method = lm,
              aes(log10(meanTotLength), log10(meanEnCostkJ)),
              color = "black",
              se = TRUE) +
  geom_smooth(method = lm,
              aes(log10(meanTotLength), log10(meanEnPerLunge)),
              color = "black",
              linetype = 2,
              se = TRUE) +
  geom_point(aes(log10(meanTotLength), log10(meanEnCostkJ), fill = meanLengthDecel),
             color = "black",
             shape = 21,
             size = 10) +
  geom_point(aes(log10(meanTotLength), log10(meanEnPerLunge), fill = meanLengthDecel),
             color = "black",
             shape = 22,
             size = 10) +
  ggtitle("Duration of Deceleration") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = bquote("Log"^10*" Body Length (m)"),
       y = bquote("Log"^10*" Lunge Energy (kJ)")) +
  theme_classic(base_size = 8) +
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 36),
        legend.position = "right",
        legend.title = element_blank(),
        panel.grid.minor = element_blank())

p3 <- ggplot(filter(LungeKinematics::AllWhalesAvgs,Species=="Blue")) +
  geom_smooth(method = lm,
              aes(log10(meanTotLength), log10(meanEnCostkJ)),
              color = "black",
              se = TRUE) +
  geom_smooth(method = lm,
              aes(log10(meanTotLength), log10(meanEnPerLunge)),
              color = "black",
              linetype = 2,
              se = TRUE) +
  geom_point(aes(log10(meanTotLength), log10(meanEnCostkJ), fill = meanSpdBegDecel),
             color = "black",
             shape = 21,
             size = 10) +
  geom_point(aes(log10(meanTotLength), log10(meanEnPerLunge), fill = meanSpdBegDecel),
             color = "black",
             shape = 22,
             size = 10) +
  ggtitle("U Mouth Open") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = bquote("Log"^10*" Body Length (m)"),
       y = bquote("Log"^10*" Lunge Energy (kJ)")) +
  theme_classic(base_size = 8) +
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 36),
        legend.position = "right",
        legend.title = element_blank(),
        panel.grid.minor = element_blank())

p4 <- ggplot(filter(LungeKinematics::AllWhalesAvgs,Species=="Blue")) +
  geom_smooth(method = lm,
              aes(log10(meanTotLength), log10(meanEnCostkJ)),
              color = "black",
              se = TRUE) +
  geom_smooth(method = lm,
              aes(log10(meanTotLength), log10(meanEnPerLunge)),
              color = "black",
              linetype = 2,
              se = TRUE) +
  geom_point(aes(log10(meanTotLength), log10(meanEnCostkJ), fill = meanModelWaterEngulfFull),
             color = "black",
             shape = 21,
             size = 10) +
  geom_point(aes(log10(meanTotLength), log10(meanEnPerLunge), fill = meanModelWaterEngulfFull),
             color = "black",
             shape = 22,
             size = 10) +
  ggtitle("Water Engulfment Volume") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = bquote("Log"^10*" Body Length (m)"),
       y = bquote("Log"^10*" Lunge Energy (kJ)")) +
  theme_classic(base_size = 8) +
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 36),
        legend.position = "right",
        legend.title = element_blank(),
        panel.grid.minor = element_blank())

p5 <- ggplot(filter(LungeKinematics::AllWhalesAvgs,Species=="Blue")) +
  geom_smooth(method = lm,
              aes(log10(meanTotLength), log10(meanEnCostkJ)),
              color = "black",
              se = TRUE) +
  geom_smooth(method = lm,
              aes(log10(meanTotLength), log10(meanEnPerLunge)),
              color = "black",
              linetype = 2,
              se = TRUE) +
  geom_point(aes(log10(meanTotLength), log10(meanEnCostkJ), fill = meanDecelSpdLoss),
             color = "black",
             shape = 21,
             size = 10) +
  geom_point(aes(log10(meanTotLength), log10(meanEnPerLunge), fill = meanDecelSpdLoss),
             color = "black",
             shape = 22,
             size = 10) +
  ggtitle("Deceleration Speed Loss") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = bquote("Log"^10*" Body Length (m)"),
       y = bquote("Log"^10*" Lunge Energy (kJ)")) +
  theme_classic(base_size = 8) +
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 36),
        legend.position = "right",
        legend.title = element_blank(),
        panel.grid.minor = element_blank())

p6 <- ggplot(filter(LungeKinematics::AllWhalesAvgs,Species=="Blue")) +
  geom_smooth(method = lm,
              aes(log10(meanTotLength), log10(meanEnCostkJ)),
              color = "black",
              se = TRUE) +
  geom_smooth(method = lm,
              aes(log10(meanTotLength), log10(meanEnPerLunge)),
              color = "black",
              linetype = 2,
              se = TRUE) +
  geom_point(aes(log10(meanTotLength), log10(meanEnCostkJ), fill = meanSlpDecel),
             color = "black",
             shape = 21,
             size = 10) +
  geom_point(aes(log10(meanTotLength), log10(meanEnPerLunge), fill = meanSlpDecel),
             color = "black",
             shape = 22,
             size = 10) +
  ggtitle("Slope of Deceleration") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = bquote("Log"^10*" Body Length (m)"),
       y = bquote("Log"^10*" Lunge Energy (kJ)")) +
  theme_classic(base_size = 8) +
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 36),
        legend.position = "right",
        legend.title = element_blank(),
        panel.grid.minor = element_blank())

p7 <- cowplot::plot_grid(p1, p2, p3, p4, p5, p6,
                         nrow = 2,
                         ncol = 3,
                         align = "h",
                         axis = "bl",
                         labels = NULL)

ggsave("figs/Figure_Test.pdf", height = 960, width = 960, units = "mm", dpi = 300)

SpeedSeriesBlue <- filter(LungeKinematics::SpeedSeries,Species=="Blue")

BlueOnly <- ggplot(SpeedSeriesBlue, aes(secs, Speed)) +
  stat_smooth(geom = "line", aes(group = IndNum, color = meanModelWaterEngulfFull),
              method = "loess", span = 0.08, size = 2, se = FALSE) +
  geom_vline(xintercept = 0) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_x_continuous(breaks = seq(-10,15,5), limits = c(-10,15)) +
  labs(x = "Time (s)",
       y = bquote("Swimming Speed (m s"^-1*")")) +
  theme_classic(base_size = 8) +
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 36),
        legend.position = "none",
        panel.grid.minor = element_blank())

ggsave("figs/Figure_Test_2.pdf", height = 480, width = 960, units = "mm", dpi = 300)

ptest3 <- ggplot(LungeKinematics::AllWhalesAvgs) +
  geom_point(aes(log10(meanMassSKR), log10(meanEnPerVpos)),
             color = "black",
             fill = "red",
             shape = 21,
             size = 10) +
  geom_point(aes(log10(meanMassSKR), log10(meanEnPerVant)),
             color = "black",
             fill = "blue",
             shape = 22,
             size = 10) +
  geom_smooth(method = lm,
              aes(log10(meanMassSKR), log10(meanEnPerVpos), group = Species),
              color = "red",
              se = TRUE) +
  geom_smooth(method = lm,
              aes(log10(meanMassSKR), log10(meanEnPerVant), group = Species),
              color = "blue",
              linetype = 2,
              se = TRUE) +
  scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
  labs(x = bquote("Log"^10*" Body Length (m)"),
       y = bquote("Log"^10*" Lunge Energy (kJ)")) +
  theme_classic(base_size = 8) +
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 36),
        legend.position = "none",
        panel.grid.minor = element_blank())
ptest3

pDepthSpd <- ggplot(LungeKinematics::AllWhalesAvgs) +
  geom_smooth(method = lm,
              aes(log10(meanDepth), log10(meanSpdBegDecel)),
              color = "black",
              se = TRUE) +
  geom_point(aes(log10(meanDepth), log10(meanSpdBegDecel), fill = Species),
             color = "black",
             shape = 21,
             size = 10) +
  labs(x = bquote("Log"^10*" Lunge Depth (m)"),
       y = bquote("Log"^10*" Speed Begin Deceleration (m s-1)")) +
  theme_classic(base_size = 8) +
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 36),
        legend.position = "right",
        legend.title = element_blank(),
        panel.grid.minor = element_blank())
pDepthSpd
