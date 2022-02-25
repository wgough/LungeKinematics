#' Create Supplementary Figure 1 for Gough et al. 2022
#'
#' @return
#' @export
#'
#' @examples
#' figS1()
figS1 <- function() {
  pal <- c("Minke" = "#009E73",  "Humpback" = "#D55E00",  "Blue" = "#0072B2", "Fin" = "#9100B2", "Antarctic" = "Black", "Monterey" = "Gray")

  p1 <- ggplot(LungeKinematics::AllWhalesAvgs, aes(meanTotLength,meanDeceleration)) +
    geom_pointrange(data = LungeKinematics::DecelerationLength, aes(meanTotLength,Deceleration_mean, color = Species, ymin = quartdecel25, ymax = quartdecel75)) +
    geom_point(aes(fill = Species, size = 10), color = "black", shape = 21) +
    geom_smooth(method = loess, color = "black", se = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = "Body Length (m)",
         y = bquote("Deceleration During Engulfment (m s"^-2*")")) +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
          legend.position = "none",
          panel.grid.minor = element_blank())

  ggsave("figs/Figure_Supp_1.pdf", height = 480, width = 480, units = "mm", dpi = 300)
}

