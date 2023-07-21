#' Create Figure 6 in Gough et al. 2022 - Defense Talk Version
#'
#' @return
#' @export
#'
#' @examples
#' fig6ForTalk()
fig6ForTalk <- function() {
  pal <- c("Minke" = "#009E73",  "Humpback" = "#D55E00",  "Blue" = "#0072B2", "Fin" = "#9100B2", "Antarctic" = "Black", "Monterey" = "Gray")

  p1 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(log10(SpdBegDecel),log10(EnPerLunge))) +
    geom_point(aes(fill = Species, size = TotLength), color = "black", shape = 21, alpha = 0.5) +
    geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = bquote("Log"[10]*" Speed At Mouth Opening (m s"^-1*")"),
         y = bquote("Log"[10]*" Lunge Energetic Gain (kJ)")) +
    scale_size(range = c(4,10)) +
    ylim(1,6) +
    theme_classic(base_size = 16) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p2 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(log10(SpdBegDecel),log10(EnCostkJ))) +
    geom_point(aes(fill = Species, size = TotLength), color = "black", shape = 21, alpha = 0.5) +
    geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = bquote("Log"[10]*" Speed At Mouth Opening (m s"^-1*")"),
         y = bquote("Log"[10]*" Lunge Energetic Cost (kJ)")) +
    scale_size(range = c(4,10)) +
    ylim(1,6) +
    theme_classic(base_size = 16) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p3 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(log10(SpdBegDecel),log10(EnRatio))) +
    geom_point(aes(fill = Species, size = TotLength), color = "black", shape = 21, alpha = 0.5) +
    geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = bquote("Log"[10]*" Speed At Mouth Opening (m s"^-1*")"),
         y = bquote("Log"[10]*" Lunge Energetic Gain/Cost Ratio")) +
    scale_size(range = c(4,10)) +
    ylim(0,3) +
    theme_classic(base_size = 16) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p4 <- cowplot::plot_grid(p1, p2, p3,
                           nrow = 1,
                           ncol = 3,
                           align = "v",
                           axis = "bl",
                           labels = NULL)

  ggsave("figs/Figure_6_Three_Switched.pdf", height = 480, width = 960, units = "mm", dpi = 300)
}

#Extra Stuff - Unused

# p1 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(log10(SpdBegDecel),SlpDecel)) +
#   geom_point(aes(fill = Species), color = "black", shape = 21, alpha = 0.5, size = 3) +
#   geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
#   scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
#   labs(x = bquote("Log"^10*" Speed At Mouth Opening (m s"^-1*")"),
#        y = "Slope of Mouth Open Period") +
#   theme_classic(base_size = 8) +
#   theme(axis.text = element_text(size = 20),
#         axis.title = element_text(size = 24),
#         axis.title.x = element_blank(),
#         legend.position = "none",
#         panel.grid.minor = element_blank())

# p1 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(SpdBegDecel,LengthAcc)) +
#   geom_point(aes(fill = Species), color = "black", shape = 21, alpha = 0.5, size = 3) +
#   #geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
#   scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
#   labs(x = "Speed At Mouth Opening (m s-1)",
#        y = "Length of Acceleration Period (s)") +
#   theme_classic(base_size = 8) +
#   theme(axis.text = element_text(size = 20),
#         axis.title = element_text(size = 24),
#         axis.title.x = element_blank(),
#         legend.position = "none",
#         panel.grid.minor = element_blank())

# p2 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(SpdBegDecel,LengthDecel)) +
#   geom_point(aes(fill = Species), color = "black", shape = 21, alpha = 0.5, size = 3) +
#   #geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
#   scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
#   labs(x = "Speed At Mouth Opening (m s-1)",
#        y = "Length of Mouth Open Period (s)") +
#   theme_classic(base_size = 8) +
#   theme(axis.text = element_text(size = 20),
#         axis.title = element_text(size = 24),
#         axis.title.x = element_blank(),
#         legend.position = "none",
#         panel.grid.minor = element_blank())

# p3 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(SpdBegDecel,AccelSpdGain)) +
#   geom_point(aes(fill = Species), color = "black", shape = 21, alpha = 0.5, size = 3) +
#   geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
#   scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
#   labs(x = "Speed At Mouth Opening (m s-1)",
#        y = "Acceleration Period Speed Gain (m s-1)") +
#   theme_classic(base_size = 8) +
#   theme(axis.text = element_text(size = 20),
#         axis.title = element_text(size = 24),
#         axis.title.x = element_blank(),
#         legend.position = "none",
#         panel.grid.minor = element_blank())

# p4 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(SpdBegDecel,DecelSpdLoss)) +
#   geom_point(aes(fill = Species), color = "black", shape = 21, alpha = 0.5, size = 3) +
#   geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
#   scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
#   labs(x = "Speed At Mouth Opening (m s-1)",
#        y = "Mouth Open Period Speed Loss (m s-1)") +
#   theme_classic(base_size = 8) +
#   theme(axis.text = element_text(size = 20),
#         axis.title = element_text(size = 24),
#         axis.title.x = element_blank(),
#         legend.position = "none",
#         panel.grid.minor = element_blank())

# p8 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc, aes(log10(SpdBegDecel),log10(EnRatioDirectAdjustAvgPrey))) +
#   geom_point(aes(fill = Species), color = "black", shape = 21, alpha = 0.5, size = 3) +
#   geom_smooth(method = lm, aes(group = whaleName, colour = Species, fill = Species), se = FALSE) +
#   scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
#   labs(x = bquote("Log"^10*" Speed At Mouth Opening (m s"^-1*")"),
#        y = bquote("Log"^10*" Averaged Prey Lunge Energetic Gain/Cost Ratio")) +
#   ylim(0,3) +
#   theme_classic(base_size = 8) +
#   theme(axis.text = element_text(size = 20),
#         axis.title = element_text(size = 24),
#         axis.title.x = element_blank(),
#         legend.position = "none",
#         panel.grid.minor = element_blank())
