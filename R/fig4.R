#' Create Figure 4
#'
#' @return
#' @export
#'
#' @examples
#' fig4()
fig4 <- function() {
  pal <- c("Minke" = "#009E73",  "Humpback" = "#D55E00",  "Blue" = "#0072B2", "Fin" = "#9100B2", "Antarctic" = "Black", "Monterey" = "Gray")

  p1 <- ggplot(LungeKinematics::AllWhalesAvgs) +
    geom_pointrange(data = LungeKinematics::MWaterLength,
                    aes(log10(meanTotLength), log10(ModelWaterEngulfFullDirectAdjust_mean),
                        color = Species,
                        ymin = log10(quartwater25),
                        ymax = log10(quartwater75))) +
    geom_point(aes(log10(meanTotLength), log10(meanModelWaterEngulfFullDirectAdjust), fill = Species),
               color = "black",
               shape = 21,
               size = 10) +
    geom_smooth(method = lm,
                aes(log10(meanTotLength),log10(meanModelWaterEngulfFullDirectAdjust)),
                color = "black",
                se = TRUE) +
    geom_smooth(method = lm,
                aes(log10(meanTotLength),log10(meanModelWaterEngulfFullShirel)),
                color = "black",
                linetype = 3,
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
                    aes(log10(meanTotLength), log10(EnPerLungeDirectAdjust_mean),
                        color = Species,
                        ymin = log10(quartgain25),
                        ymax = log10(quartgain75))) +
    geom_point(aes(log10(meanTotLength), log10(meanEnPerLungeDirectAdjust), fill = Species),
               color = "black",
               shape = 22,
               size = 10) +
    geom_smooth(method = lm,
                aes(log10(meanTotLength), log10(meanEnCostkJ)),
                color = "black",
                se = TRUE) +
    geom_smooth(method = lm,
                aes(log10(meanTotLength), log10(meanEnPerLungeDirectAdjust)),
                color = "black",
                linetype = 2,
                se = TRUE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = bquote("Log"^10*" Body Length (m)"),
         y = bquote("Log"^10*" Lunge Energy (kJ)")) +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p3 <- ggplot(LungeKinematics::AllWhalesAvgs) +
    geom_pointrange(data = LungeKinematics::EnergyOutMSLength,
                    aes(log10(meanTotLength), log10(EnCostMS_mean),
                        color = Species,
                        ymin = log10(quartcost25),
                        ymax = log10(quartcost75))) +
    geom_point(aes(log10(meanTotLength), log10(meanEnCostMS), fill = Species),
               color = "black",
               shape = 21,
               size = 10) +
    geom_pointrange(data = LungeKinematics::EnergyInMSLength,
                    aes(log10(meanTotLength), log10(EnPerLungeDirectAdjustMS_mean),
                        color = Species,
                        ymin = log10(quartgain25),
                        ymax = log10(quartgain75))) +
    geom_point(aes(log10(meanTotLength), log10(meanEnPerLungeDirectAdjustMS), fill = Species),
               color = "black",
               shape = 22,
               size = 10) +
    geom_smooth(method = lm,
                aes(log10(meanTotLength), log10(meanEnCostMS)),
                color = "black",
                se = TRUE) +
    geom_smooth(method = lm,
                aes(log10(meanTotLength), log10(meanEnPerLungeDirectAdjustMS)),
                color = "black",
                linetype = 2,
                se = TRUE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = bquote("Log"^10*" Body Length (m)"),
         y = bquote("Log"^10*" Mass-Specific Lunge Energy (kJ/kg)")) +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p4 <- ggplot(LungeKinematics::AllWhalesAvgs) +
    geom_pointrange(data = LungeKinematics::EnergyRatioLungeLength,
                    aes(log10(meanTotLength), log10(EnRatioDirectAdjust_mean),
                        color = Species,
                        ymin = log10(quartratio25),
                        ymax = log10(quartratio75))) +
    geom_point(aes(log10(meanTotLength), log10(meanEnRatioDirectAdjust), fill = Species),
               color = "black",
               shape = 21,
               size = 10) +
    geom_smooth(method = lm,
                aes(log10(meanTotLength), log10(meanEnRatioDirectAdjust)),
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
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          legend.position = "none",
          panel.grid.minor = element_blank())



  p5 <- cowplot::plot_grid(p1, p2, p3,
                           nrow = 3,
                           ncol = 1,
                           align = "v",
                           axis = "bl",
                           labels = NULL)

  p6 <- cowplot::plot_grid(p5, p4,
                           nrow = 1,
                           ncol = 2,
                           align = "none",
                           axis = "none",
                           labels = NULL)

  ggsave("figs/fig4.pdf", height = 480, width = 960, units = "mm", dpi = 300)

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
