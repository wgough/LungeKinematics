#' Create Figure 2
#'
#' @return
#' @export
#'
#' @examples
#' fig2()
fig2 <- function() {
  pal <- c("Minke" = "#009E73",  "Humpback" = "#D55E00",  "Blue" = "#0072B2", "Fin" = "#9100B2", "Antarctic" = "Black", "Monterey" = "Gray")

  p1 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(SpdBegDecel,LengthAcc)) +
    geom_point(aes(fill = Species), color = "black", shape = 21, alpha = 0.5) +
    geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = "Speed At Mouth Opening (m s-1)",
         y = "Length of Acceleration Period (s)") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p2 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(SpdBegDecel,LengthDecel)) +
    geom_point(aes(fill = Species), color = "black", shape = 21, alpha = 0.5) +
    geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = "Speed At Mouth Opening (m s-1)",
         y = "Length of Mouth Open Period (s)") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p3 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(SpdBegDecel,AccelSpdGain)) +
    geom_point(aes(fill = Species), color = "black", shape = 21, alpha = 0.5) +
    geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = "Speed At Mouth Opening (m s-1)",
         y = "Acceleration Period Speed Gain (m s-1)") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p4 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(SpdBegDecel,DecelSpdLoss)) +
    geom_point(aes(fill = Species), color = "black", shape = 21, alpha = 0.5) +
    geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = "Speed At Mouth Opening (m s-1)",
         y = "Mouth Open Period Speed Loss (m s-1)") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p5 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(SpdBegDecel,SlpAccel)) +
    geom_point(aes(fill = Species), color = "black", shape = 21, alpha = 0.5) +
    geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = "Speed At Mouth Opening (m s-1)",
         y = "Slope of Acceleration Period") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p6 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(SpdBegDecel,SlpDecel)) +
    geom_point(aes(fill = Species), color = "black", shape = 21, alpha = 0.5) +
    geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = "Speed At Mouth Opening (m s-1)",
         y = "Slope of Mouth Open Period") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p7 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(SpdBegDecel,SpeedIntegrDist)) +
    geom_point(aes(fill = Species), color = "black", shape = 21, alpha = 0.5) +
    geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = "Speed At Mouth Opening (m s-1)",
         y = "Mouth Open Period Distance Traveled (m)") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p8 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(log10(SpdBegDecel),log10(ModelWaterEngulfFullDirectAdjust))) +
    geom_point(aes(fill = Species), color = "black", shape = 21, alpha = 0.5) +
    geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = "Log10 Speed At Mouth Opening (m s-1)",
         y = "Log10 Water Engulfed (m^3)") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p9 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(log10(SpdBegDecel),log10(EnCostkJ))) +
    geom_point(aes(fill = Species), color = "black", shape = 21, alpha = 0.5) +
    geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = "Log10 Speed At Mouth Opening (m s-1)",
         y = "Log10 Lunge Energetic Cost (Watts)") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p10 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(log10(SpdBegDecel),log10(EnCostMS))) +
    geom_point(aes(fill = Species), color = "black", shape = 21, alpha = 0.5) +
    geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = "Log10 Speed At Mouth Opening (m s-1)",
         y = "Log10 Mass-Specific Lunge Energetic Cost (Watts kg-1)") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p11 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(log10(SpdBegDecel),log10(EnPerLungeDirectAdjust))) +
    geom_point(aes(fill = Species), color = "black", shape = 21, alpha = 0.5) +
    geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = "Log10 Speed At Mouth Opening (m s-1)",
         y = "Log10 Lunge Energetic Gain (kJ)") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p12 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(log10(SpdBegDecel),log10(EnRatioDirectAdjust))) +
    geom_point(aes(fill = Species), color = "black", shape = 21, alpha = 0.5) +
    geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = "Log10 Speed At Mouth Opening (m s-1)",
         y = "Log10 Lunge Energetic Cost/Gain Ratio") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p13 <- cowplot::plot_grid(p1, p3, p5, p7, p9, p11, p2, p4, p6, p8, p10, p12,
                           nrow = 2,
                           ncol = 6,
                           align = "v",
                           axis = "bl",
                           labels = NULL)

  ggsave("figs/fig2.pdf", height = 480, width = 960, units = "mm", dpi = 300)
}

#LengthAcc, LengthDecel, AccelSpdGain, DecelSpdLoss, SlpAccel, SlpDecel, SpeedIntegrDist, ModelWaterEngulfFullShirel, EnCostkJ, EnCostMS, EnPerLungeShirel, EnRatioShirel
