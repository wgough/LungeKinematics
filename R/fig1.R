#' Create Figure 1
#'
#' @return
#' @export
#'
#' @examples
#' fig1()
fig1 <- function() {
  pal <- c("Minke" = "#009E73",  "Humpback" = "#D55E00",  "Blue" = "#0072B2", "Fin" = "#9100B2", "Antarctic" = "Black", "Monterey" = "Gray")

  p1 <- ggplot(LungeKinematics::AllWhalesAvgs, aes(meanTotLength,meanSpdBegDecel)) +
    geom_pointrange(data = LungeKinematics::MaxSpdLength, aes(meanTotLength,SpdBegDecel_mean, color = Species, ymin = quartspd25, ymax = quartspd75)) +
    geom_point(aes(fill = Species), color = "black", size = 7, shape = 21) +
    geom_point(data = LungeKinematics::LitMaxSpeeds, aes(EstimatedSize,MaxSpeed, fill = Species), size = 7, shape = 22) +
    geom_flat_violin(data = LungeKinematics::AllWhalesLungeTableTrunc, aes(x = 0,SpdBegDecel, fill = factor(Location)),
                     position = position_nudge(x = 0, y = 0),
                     width = 8, adjust = 0.8, trim = FALSE, alpha = 0.5, colour = NA) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    ylim(0,8) +
    xlim(0,28) +
    labs(x = "Body Length (m)",
         y = "Speed At Mouth Opening (m s-1)") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          legend.position = "none",
          panel.grid.minor = element_blank())

  dragReg <- function(x) 6.1389 * x^(-0.954)

  p2 <- ggplot(LungeKinematics::SpeedSeries, aes(secs, Speed, group = Species)) +
    #geom_path(aes(group = interaction(IndNum,lunge), color = Species, alpha = 0.05)) +
    stat_smooth(geom='line', aes(group = IndNum, color = Species), method = "loess", alpha = 0.2, span = 0.08, se = FALSE) +
    geom_smooth(aes(color = Species), method = "loess", span = 0.08, se = FALSE) +
    scale_color_manual(values = pal) +
    ylim(0,8) +
    xlim(-15,15) +
    labs(x = "Time (s)",
         y = "Swimming Speed (m s-1)") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p3 <- ggplot() +
    stat_function(fun = dragReg, aes(linetype = "dragReg")) +
    scale_linetype_manual(values = "dashed", guide = FALSE) +
    ylim(0,2) +
    xlim(0,28) +
    labs(x = "Body Length (m)",
         y = "Shape Drag / Engulfment Drag") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p4 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc) +
    geom_flat_violin(aes(x = 0.04,LengthAcc, fill = factor(Species)),
                     position = position_dodge(2.0),
                     width = 5, adjust = 0.8, trim = FALSE, alpha = 0.5, colour = NA) +
    geom_boxplot(aes(x = 0, y = LengthAcc, fill = factor(Species)),
                 position = position_dodge(2.0),
                 width = 0.25, outlier.shape = NA, alpha = 0.3, width = .1, colour = "Black") +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    coord_flip() +
    labs(y = "Length of Acceleration Phase (s)") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.text.y = element_blank(),
          axis.title = element_text(size = 24),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p5 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc) +
    geom_flat_violin(aes(x = 0.04,LengthDecel, fill = factor(Species)),
                 position = position_dodge(2.0),
                 width = 5, adjust = 0.8, trim = FALSE, alpha = 0.5, colour = NA) +
    geom_boxplot(aes(x = 0, y = LengthDecel, fill = factor(Species)),
                 position = position_dodge(2.0),
                 width = 0.25, outlier.shape = NA, alpha = 0.3, width = .1, colour = "Black") +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    coord_flip() +
    labs(y = "Length of Deceleration Phase (s)") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.text.y = element_blank(),
          axis.title = element_text(size = 24),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p6 <- cowplot::plot_grid(p4, p5,
            ncol = 2,
            align = "h",
            labels = NULL)

  p7 <- cowplot::plot_grid(p1, p2, p3, p6,
            nrow = 2,
            ncol = 2,
            align = "hv",
            axis = "tl",
            rel_heights = c(2,1),
            labels = NULL)

  ggsave("figs/fig1.pdf", height = 480, width = 960, units = "mm", dpi = 300)
}
