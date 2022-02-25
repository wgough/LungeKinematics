#' Create Supplementary Figure 4 for Gough et al. 2022
#'
#' @return
#' @export
#'
#' @examples
#' figS4()
figS4 <- function() {
  pal <- c("Minke" = "#009E73",  "Humpback" = "#D55E00",  "Blue" = "#0072B2", "Fin" = "#9100B2", "Antarctic" = "Black", "Monterey" = "Gray")

  p1 <- ggplot(LungeKinematics::AllWhalesAvgs) +
    geom_point(aes(meanTotLength,meanJawAreaDirectMeasure, fill = Species, size = 10), color = "black", shape = 21) +
    #geom_point(aes(meanTotLength,meanJawAreaTriangleApprox, fill = Species, size = 10), color = "black", shape = 22) +
    geom_smooth(aes(meanTotLength,meanJawAreaDirectMeasure, group = Species), method = lm, color = "black", se = TRUE) +
    #geom_smooth(aes(meanTotLength,meanJawAreaTriangleApprox), method = loess, color = "black", se = TRUE, linetype = "dashed") +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    scale_x_continuous(breaks = seq(0,30,5), limits = c(0,30)) +
    labs(x = "Body Length (m)",
         y = bquote("Jaw Area (m s"^2*")")) +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
          legend.position = "none",
          panel.grid.minor = element_blank())

  ggsave("figs/Figure_Supp_4.pdf", height = 480, width = 480, units = "mm", dpi = 300)
}


