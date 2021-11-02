#' Create Figure 3
#'
#' @return
#' @export
#'
#' @examples
#' fig3()
fig3 <- function() {
  pal <- c("Minke" = "#009E73",  "Humpback" = "#D55E00",  "Blue" = "#0072B2", "Fin" = "#9100B2", "Antarctic" = "Black", "Monterey" = "Gray")

  p1 <- ggplot(LungeKinematics::AllWhalesAvgs) +
    geom_pointrange(data = LungeKinematics::MWaterLength, aes(log10(meanTotLength), log10(ModelWaterEngulfFullDirectAdjust_mean), color = Species, ymin = log10(quartwater25), ymax = log10(quartwater75))) +
    geom_point(aes(log10(meanTotLength), log10(meanModelWaterEngulfFullDirectAdjust), fill = Species), color = "black", shape = 21, size = 10) +
    geom_smooth(method = lm, aes(log10(meanTotLength),log10(meanModelWaterEngulfFullDirectAdjust)), color = "black", se = TRUE) +
    geom_smooth(method = lm, aes(log10(meanTotLength),log10(meanModelWaterEngulfFullEllipse)), color = "black", linetype = 2, se = TRUE) +
    geom_smooth(method = lm, aes(log10(meanTotLength),log10(meanModelWaterEngulfFullShirel)), color = "black", linetype = 3, se = TRUE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = bquote("Log"^10*" Body Length (m)"),
         y = bquote("Log"^10*" Water Engulfed (m"^3*")")) +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          legend.position = "none",
          panel.grid.minor = element_blank())

  ggsave("figs/fig3.pdf", height = 480, width = 480, units = "mm", dpi = 300)

}
