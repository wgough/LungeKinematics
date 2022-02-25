#' Create 4-Panel Portion within Figure 3 for Gough et al. 2022
#'
#' @return
#' @export
#'
#' @examples
#' fig3()
fig3 <- function() {
  pal <- c("Minke" = "#009E73",  "Humpback" = "#D55E00",  "Blue" = "#0072B2", "Fin" = "#9100B2", "Antarctic" = "Black", "Monterey" = "Gray")

  p1 <- ggplot(LungeKinematics::AllWhalesAvgs) +
    geom_point(data = LungeKinematics::CleanedJawDataDR, aes(log10(TL), log10(JawAreaDR), fill = Species),
               alpha = 0.5,
               color = "black",
               shape = 22,
               size = 10) +
    geom_point(aes(log10(meanTotLength), log10(meanJawAreaTriangleApprox), fill = Species),
               color = "black",
               shape = 21,
               size = 10) +
    geom_point(aes(log10(meanTotLength), log10(meanJawAreaDirectMeasure), fill = Species),
               color = "black",
               shape = 23,
               size = 10) +
    geom_smooth(method = lm,
                aes(log10(meanTotLength), log10(meanJawAreaTriangleApprox), group = Species),
                color = "black",
                linetype = 3,
                se = TRUE) +
    geom_smooth(method = lm,
                aes(log10(meanTotLength), log10(meanJawAreaDirectMeasure), group = Species),
                color = "black",
                se = TRUE) +
    geom_smooth(data = LungeKinematics::CleanedJawDataDR, method = lm,
                aes(log10(TL), log10(JawAreaDR), group = Species),
                color = "black",
                linetype = 2,
                se = TRUE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = bquote("Log"^10*" Body Length (m)"),
         y = bquote("log"^10*" Jaw Area (m"^2*")")) +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
          legend.position = "none",
          panel.grid.minor = element_blank())

  ggsave("figs/Figure_3.pdf", height = 360, width = 720, units = "mm", dpi = 300)
}
