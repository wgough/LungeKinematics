#' Create Figure 1
#'
#' @return
#' @export
#'
#' @examples
#' fig1()
fig1 <- function() {
  pal <- c("Minke" = "#009E73",  "Humpback" = "#D55E00",  "Blue" = "#0072B2", "Fin" = "#9100B2", "Antarctic" = "Black", "Monterey" = "Gray")
  light_pal <- colorspace::lighten(pal, amount = 0.2)
  names(light_pal) <- names(pal)

  p1 <- ggplot(LungeKinematics::AllWhalesAvgs, aes(meanTotLength,meanSpdBegDecel)) +
    geom_pointrange(data = LungeKinematics::MaxSpdLength, aes(meanTotLength,SpdBegDecel_mean, color = Species, ymin = quartspd25, ymax = quartspd75)) +
    geom_point(aes(fill = Species, size = meanDepth), color = "black", shape = 21) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    scale_x_continuous(breaks = seq(0,30,5), limits = c(0,30)) +
    scale_size(range = c(4,10)) +
    ylim(0,8) +
    labs(x = "Body Length (m)",
         y = "Speed At Mouth Opening (m s-1)") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  dragReg <- function(x) 6.1389 * x^(-0.954)

  p2 <- ggplot(LungeKinematics::SpeedSeries, aes(secs, Speed, group = Species)) +
    stat_smooth(geom = "line", aes(group = IndNum, color = Species),
                method = "loess", alpha = 0.2, span = 0.08, size = 2, se = FALSE) +
    geom_smooth(aes(color = Species),
                method = "loess", span = 0.08, size = 3, se = FALSE) +
    geom_vline(xintercept = 0) +
    scale_color_manual(values = pal) +
    scale_x_continuous(breaks = seq(-25,15,5), limits = c(-25,15)) +
    ylim(0,8) +
    labs(x = "Time (s)",
         y = "Swimming Speed (m s-1)") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p3 <- AllWhalesLungeTableTrunc %>%
    transmute(Species = factor(Species, levels = c("Minke", "Humpback", "Fin", "Blue")),
              "Acceleration Start Speed (m s-1)" = SpdAtInc,
              "Maximum Speed (m s-1)" = MaxSpd,
              "Speed At Mouth Opening (m s-1)" = SpdBegDecel,
              "Speed At Mouth Closing (m s-1)" = SpdEndDecel) %>%
    pivot_longer("Acceleration Start Speed (m s-1)":"Speed At Mouth Closing (m s-1)", names_to = "Timings", values_to = "Speed") %>%
    mutate(Timings = factor(Timings, levels = c("Acceleration Start Speed (m s-1)", "Maximum Speed (m s-1)", "Speed At Mouth Opening (m s-1)", "Speed At Mouth Closing (m s-1)"))) %>%
    ggplot(aes(Species, Speed, color = Species, fill = Species,
               group = fct_cross(Timings, Species))) +
    geom_boxplot_pattern(size = 1, aes(pattern = Timings, pattern_density = Timings),
                         pattern_spacing = 0.01,
                         pattern_size = 0.01,
                         pattern_colour = "black",
                         pattern_fill = "white") +
    scale_pattern_manual(values = c("Acceleration Start Speed (m s-1)" = "stripe", "Maximum Speed (m s-1)" = "circle", "Speed At Mouth Opening (m s-1)" = "none", "Speed At Mouth Closing (m s-1)" = "crosshatch")) +
    scale_pattern_density_manual(values = c("Acceleration Start Speed (m s-1)" = 0.3, "Maximum Speed (m s-1)" = 0.4, "Speed At Mouth Opening (m s-1)" = 0.2, "Speed At Mouth Closing (m s-1)" = 0.25)) +
    scale_pattern_angle_manual(values = c(45, 45, 0, -45)) +
    scale_color_manual(values = c("Black", "Black", "Black", "Black")) +
    scale_fill_manual(values = pal) +
    ylim(0,8) +
    labs(x = "Species",
         y = "Energetically Important Speeds (m s-1)") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
          axis.title.x = element_blank(),
          legend.position = "none")

  p4 <- ggplot() +
    stat_function(fun = dragReg, aes(linetype = "dragReg"), size = 2) +
    scale_linetype_manual(values = "dashed", guide = FALSE) +
    scale_x_continuous(breaks = seq(0,30,5), limits = c(0,30)) +
    ylim(0,2) +
    labs(x = "Body Length (m)",
         y = "Shape Drag / Engulfment Drag") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p5 <- AllWhalesLungeTableTrunc %>%
    transmute(Species = factor(Species, levels = c("Blue", "Fin", "Humpback", "Minke")),
              AccStart = (PosSpdInc - PosBegDecel) / 10,
              DecEnd = (PosEndDecel - PosBegDecel) / 10) %>%
    pivot_longer(AccStart:DecEnd, names_to = "kinematic", values_to = "time") %>%
    ggplot(aes(time, Species, color = Species, fill = Species,
               group = fct_cross(kinematic, Species))) +
    geom_boxplot(position = position_identity(), size = 1) +
    geom_vline(xintercept = 0) +
    scale_color_manual(values = c("Black", "Black", "Black", "Black")) +
    scale_fill_manual(values = pal) +
    scale_x_continuous(breaks = seq(-25,15,5), limits = c(-25,15)) +
    labs(x = "Time Before/After Mouth Opening (s)",
         y = "Swimming Speed (m s-1)") +
    theme_classic(base_size = 8) +
    theme(axis.line.y = element_blank(),
          axis.text.x = element_text(size = 30),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title = element_text(size = 36),
          axis.title.y = element_blank(),
          legend.position = "none")

  p6 <- cowplot::plot_grid(p1, p2, p3, p4, p5,
            nrow = 2,
            ncol = 3,
            align = "hv",
            axis = "bl",
            rel_heights = c(1,0.50),
            labels = NULL)

  ggsave("figs/fig1.pdf", height = 480, width = 960, units = "mm", dpi = 300)
}

#Extra Stuff - Unused

#p5 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc) +
#  geom_flat_violin(aes(x = 0.04,LengthAcc, fill = factor(Species)),
#                   position = position_dodge(2.0),
#                   width = 5, adjust = 0.8, trim = FALSE, alpha = 0.5, colour = NA) +
#  geom_boxplot(aes(x = 0, y = LengthAcc, fill = factor(Species)),
#               position = position_dodge(2.0),
#               width = 0.25, outlier.shape = NA, alpha = 0.3, width = .1, colour = "Black") +
#  scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
#  coord_flip() +
#  labs(y = "Length of Acceleration Phase (s)") +
#  theme_classic(base_size = 8) +
#  theme(axis.text = element_text(size = 20),
#        axis.text.y = element_blank(),
#        axis.title = element_text(size = 24),
#        axis.title.y = element_blank(),
#        legend.position = "none",
#        panel.grid.minor = element_blank())

#p6 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc) +
#  geom_flat_violin(aes(x = 0.04,LengthDecel, fill = factor(Species)),
#                   position = position_dodge(2.0),
#                   width = 5, adjust = 0.8, trim = FALSE, alpha = 0.5, colour = NA) +
#  geom_boxplot(aes(x = 0, y = LengthDecel, fill = factor(Species)),
#               position = position_dodge(2.0),
#               width = 0.25, outlier.shape = NA, alpha = 0.3, width = .1, colour = "Black") +
#  scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
#  coord_flip() +
#  labs(y = "Length of Deceleration Phase (s)") +
#  theme_classic(base_size = 8) +
#  theme(axis.text = element_text(size = 20),
#        axis.text.y = element_blank(),
#        axis.title = element_text(size = 24),
#        axis.title.y = element_blank(),
#        legend.position = "none",
#        panel.grid.minor = element_blank())

#p6 <- cowplot::plot_grid(p4, p5,
#                         ncol = 2,
#                         align = "h",
#                         labels = NULL)
