#' Create Figure 5 for Gough et al. 2022
#'
#' @return
#' @export
#'
#' @examples
#' fig5()
fig5 <- function() {
  pal <- c("Minke" = "#009E73",  "Humpback" = "#D55E00",  "Blue" = "#0072B2", "Fin" = "#9100B2", "Antarctic" = "Black", "Monterey" = "Gray")

  p1 <- ggplot(LungeKinematics::AllWhalesLungeTableTrunc) +
    geom_density(aes(x=(ShapeDragMinusThrust/EngulfmentDrag), fill = Species, alpha = 0.5), size = 0.5) +
    geom_vline(xintercept = 0, size = 1, linetype = "dashed") +
    scale_color_manual(values = pal, aesthetics = c("fill")) +
    labs(x = "(Shape Drag - Thrust) / Engulfment Drag",
         y = "Density") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
          legend.position = "none",
          panel.grid.minor = element_blank())

  ggsave("figs/Figure_5.pdf", height = 480, width = 720, units = "mm", dpi = 300)

}
