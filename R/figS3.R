#' Create Supplementary Figure 3 for Gough et al. 2022
#'
#' @return
#' @export
#'
#' @examples
#' figS3()
figS3 <- function() {
  pal <- c("Minke" = "#009E73",  "Humpback" = "#D55E00",  "Blue" = "#0072B2", "Fin" = "#9100B2", "Antarctic" = "Black", "Monterey" = "Gray")

  p1 <- ggplot(LungeKinematics::AllWhalesAvgs, aes(meanTotLength,meanDistancePerVGBLength)) +
    geom_pointrange(data = LungeKinematics::DistancePerVGBLength, aes(meanTotLength,DistancePerVGBLength_mean, color = Species, ymin = quartdist25, ymax = quartdist75)) +
    geom_point(aes(fill = Species, size = 10), color = "black", shape = 21) +
    geom_smooth(method = loess, color = "black", se = TRUE) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    scale_x_continuous(breaks = seq(0,30,5), limits = c(0,30)) +
    labs(x = "Body Length (m)",
         y = "Distance Travelled Per VGB Length") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
          legend.position = "none",
          panel.grid.minor = element_blank())

  ggsave("figs/Figure_Supp_3.pdf", height = 480, width = 480, units = "mm", dpi = 300)
}


