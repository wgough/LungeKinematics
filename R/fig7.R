#' Create Figure 7 for Gough et al. 2022
#'
#' @return
#' @export
#'
#' @examples
#' fig7()
fig7 <- function() {
  pal <- c("Minke" = "#009E73",  "Humpback" = "#D55E00",  "Blue" = "#0072B2", "Fin" = "#9100B2", "Antarctic" = "Black", "Monterey" = "Gray")

  p1 <- ggplot(LungeKinematics::AllWhalesAvgs) +
    geom_pointrange(data = LungeKinematics::MWaterLength,
                    aes(log10(meanTotLength), log10(ModelWaterEngulfFull_mean),
                        color = Species,
                        ymin = log10(quartwater25),
                        ymax = log10(quartwater75))) +
    geom_point(aes(log10(meanTotLength), log10(meanModelWaterEngulfFull), fill = Species),
               color = "black",
               shape = 21,
               size = 10) +
    geom_smooth(method = lm,
                aes(log10(meanTotLength),log10(meanModelWaterEngulfFull), group = Species),
                color = "black",
                se = TRUE) +
    geom_smooth(method = lm,
                aes(log10(meanTotLength),log10(meanModelWaterEngulfFullShirel), group = Species),
                color = "black",
                linetype = 2,
                se = TRUE) +
    scale_color_manual(values = pal,
                       aesthetics = c("fill","colour")) +
    labs(x = bquote("Log"^10*" Body Length (m)"),
         y = bquote("Log"^10*" Water Engulfed (m"^3*")")) +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p2 <- ggplot(LungeKinematics::AllWhalesAvgs) +
    geom_pointrange(data = LungeKinematics::EnergyOutAbsLength,
                    aes(log10(meanTotLength), log10(EnCostkJ_mean),
                        color = Species,
                        ymin = log10(quartcost25),
                        ymax = log10(quartcost75))) +
    geom_point(aes(log10(meanTotLength), log10(meanEnCostkJ), fill = Species),
               color = "black",
               shape = 21,
               size = 10) +
    geom_pointrange(data = LungeKinematics::EnergyInAbsLength,
                    aes(log10(meanTotLength), log10(EnPerLunge_mean),
                        color = Species,
                        ymin = log10(quartgain25),
                        ymax = log10(quartgain75))) +
    geom_point(aes(log10(meanTotLength), log10(meanEnPerLunge), fill = Species),
               color = "black",
               shape = 22,
               size = 10) +
    geom_smooth(method = lm,
                aes(log10(meanTotLength), log10(meanEnCostkJ)),
                color = "black",
                se = TRUE) +
    geom_smooth(method = lm,
                aes(log10(meanTotLength), log10(meanEnPerLunge)),
                color = "black",
                linetype = 2,
                se = TRUE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = bquote("Log"^10*" Body Length (m)"),
         y = bquote("Log"^10*" Lunge Energy (kJ)")) +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
#          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p3 <- ggplot(LungeKinematics::AllWhalesAvgs) +
    geom_pointrange(data = LungeKinematics::EnergyRatioLungeLength,
                    aes(log10(meanTotLength), log10(EnRatio_mean),
                        color = Species,
                        ymin = log10(quartratio25),
                        ymax = log10(quartratio75))) +
    geom_point(aes(log10(meanTotLength), log10(meanEnRatio), fill = Species),
               color = "black",
               shape = 21,
               size = 10) +
    geom_smooth(method = lm,
                aes(log10(meanTotLength), log10(meanEnRatio)),
                color = "black",
                se = TRUE) +
    geom_pointrange(data = LungeKinematics::EnergyRatioDiveLength,
                    aes(log10(meanTotLength), log10(EnRatioDive_mean),
                        color = Species,
                        ymin = log10(quartratio25),
                        ymax = log10(quartratio75))) +
    geom_point(aes(log10(meanTotLength), log10(meanEnRatioDive), fill = Species),
               color = "black",
               shape = 22,
               size = 10) +
    geom_smooth(method = lm,
                aes(log10(meanTotLength), log10(meanEnRatioDive)),
                color = "black",
                linetype = 2,
                se = TRUE) +
    geom_pointrange(data = LungeKinematics::EnergyRatioDayLength,
                    aes(log10(meanTotLength), log10(EnRatioDay_mean),
                        color = Species,
                        ymin = log10(quartratio25),
                        ymax = log10(quartratio75))) +
    geom_point(aes(log10(meanTotLength), log10(meanEnRatioDay), fill = Species),
               color = "black",
               shape = 23,
               size = 10) +
    geom_smooth(method = lm,
                aes(log10(meanTotLength), log10(meanEnRatioDay)),
                color = "black",
                linetype = 3,
                se = TRUE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = bquote("Log"^10*" Body Length (m)"),
         y = bquote("Log"^10*" Lunge Energetic Ratio")) +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p4 <- cowplot::plot_grid(p1, p2,
                           nrow = 2,
                           ncol = 1,
                           align = "v",
                           axis = "bl",
                           labels = NULL)

  p5 <- cowplot::plot_grid(p4, p3,
                           nrow = 1,
                           ncol = 2,
                           align = "none",
                           axis = "none",
                           labels = NULL)

  ggsave("figs/Figure_7.pdf", height = 480, width = 960, units = "mm", dpi = 300)
#  ggsave("figs/figwatersquare.pdf", height = 480, width = 480, units = "mm", dpi = 300)
#  ggsave("figs/figenergysquare.pdf", height = 480, width = 480, units = "mm", dpi = 300)
#  ggsave("figs/figratiosquare.pdf", height = 480, width = 480, units = "mm", dpi = 300)

}

## Unused Portions
# p5 <- ggplot(LungeKinematics::SpeciesEnRatioDiveLength) +
#   geom_pointrange(aes(log10(meanMass), log10(meanEnRatioDive_mean),
#                       color = Species,
#                       ymin = log10(quartratio25),
#                       ymax = log10(quartratio75))) +
#   geom_point(aes(log10(meanMass), log10(meanEnRatioDive_mean), fill = Species),
#              color = "black",
#              shape = 21,
#              size = 10) +
#   geom_point(data = LungeKinematics::LitEnRatios,
#              aes(log10(Mass), log10(EnRatio), fill = Group),
#              color = "black",
#              shape = 23,
#              size = 10) +
#   #scale_color_manual(values = pal, aesthetics = c("fill","color")) +
#   geom_text(data = LungeKinematics::LitEnRatios,
#             aes(x = log10(Mass), y = log10(EnRatio), label=Spec_Abbrev, color = Group),
#             hjust = -0.5,
#             vjust = 1.4,
#             size = 12) +
#   labs(x = "Estimated Body Mass (kg)",
#        y = "Hunting Efficiency (Ratio of Energy Acquired to Energy Spent)") +
#   theme_classic(base_size = 8) +
#   theme(axis.text = element_text(size = 30),
#         axis.title = element_text(size = 36),
#         legend.position = "none",
#         panel.grid.minor = element_blank())
#
# p3 <- ggplot(LungeKinematics::AllWhalesAvgs) +
#   # geom_pointrange(data = LungeKinematics::EnergyOutMSLength,
#   #                 aes(log10(meanTotLength), log10(EnCostMS_mean),
#   #                     color = Species,
#   #                     ymin = log10(quartcost25),
#   #                     ymax = log10(quartcost75))) +
#   geom_point(aes(log10(meanTotLength), log10(meanPowerOutputMSiso), fill = Species),
#              color = "black",
#              shape = 21,
#              size = 10) +
#   # geom_pointrange(data = LungeKinematics::EnergyInMSLength,
#   #                 aes(log10(meanTotLength), log10(EnPerLungeMS_mean),
#   #                     color = Species,
#   #                     ymin = log10(quartgain25),
#   #                     ymax = log10(quartgain75))) +
#   geom_point(aes(log10(meanTotLength), log10(meanRateOfEnergyInMSiso), fill = Species),
#              color = "black",
#              shape = 22,
#              size = 10) +
#   geom_smooth(method = lm,
#               aes(log10(meanTotLength), log10(meanPowerOutputMSiso)),
#               color = "black",
#               se = TRUE) +
#   geom_smooth(method = lm,
#               aes(log10(meanTotLength), log10(meanRateOfEnergyInMSiso)),
#               color = "black",
#               linetype = 2,
#               se = TRUE) +
#   scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
#   labs(x = bquote("Log"^10*" Body Length (m)"),
#        y = bquote("Log"^10*" Mass-Specific Lunge Energy Per Unit Time (kJ/kg/s)")) +
#   theme_classic(base_size = 8) +
#   theme(axis.text = element_text(size = 20),
#         axis.title = element_text(size = 24),
#         axis.title.x = element_blank(),
#         legend.position = "none",
#         panel.grid.minor = element_blank())
#
# p1 <- ggplot(LungeKinematics::AllWhalesAvgs) +
#   geom_pointrange(data = LungeKinematics::MWaterLength, aes(log10(meanTotLength), log10(ModelWaterEngulfFull_mean), color = Species, ymin = log10(quartwater25), ymax = log10(quartwater75))) +
#   geom_point(aes(log10(meanTotLength), log10(meanModelWaterEngulfFull), fill = Species), color = "black", shape = 21, size = 10) +
#   geom_smooth(method = lm, aes(log10(meanTotLength),log10(meanModelWaterEngulfFull)), color = "black", se = TRUE) +
#   geom_smooth(method = lm, aes(log10(meanTotLength),log10(meanModelWaterEngulfFullShirel)), color = "black", linetype = 3, se = TRUE) +
#   scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
#   labs(x = bquote("Log"^10*" Body Length (m)"),
#        y = bquote("Log"^10*" Water Engulfed (m"^3*")")) +
#   theme_classic(base_size = 8) +
#   theme(axis.text = element_text(size = 20),
#         axis.title = element_text(size = 24),
#         legend.position = "none",
#         panel.grid.minor = element_blank())

# p2 <- ggplot(LungeKinematics::AllWhalesAvgs) +
#   geom_point(aes(log10(meanTotLength), log10(meanEnCostMSByUSquared), fill = Species),
#              color = "black",
#              shape = 21,
#              size = 10) +
#   geom_smooth(method = lm,
#               aes(log10(meanTotLength), log10(meanEnCostMSByUSquared)),
#               color = "black",
#               se = TRUE) +
#   scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
#   ylim(-2.8,-1.4) +
#   labs(x = bquote("Log"^10*" Body Length (m)"),
#        y = bquote("Log"^10*" Mass-Specific Energetic Cost Over U^2 (kJ/kg)")) +
#   theme_classic(base_size = 8) +
#   theme(axis.text = element_text(size = 30),
#         axis.title = element_text(size = 36),
#         legend.position = "none",
#         panel.grid.minor = element_blank())
#
# ggsave("figs/EnEnCostMSByUSquared_Test.pdf", height = 480, width = 480, units = "mm", dpi = 300)
