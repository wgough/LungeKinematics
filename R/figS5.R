#' Create Supplementary Figure 5 for Gough et al. 2022
#'
#' @return
#' @export
#'
#' @examples
#' figS5()
figS5 <- function() {
  pal <- c("Minke" = "#009E73",  "Humpback" = "#D55E00",  "Blue" = "#0072B2", "Fin" = "#9100B2", "Antarctic" = "Black", "Monterey" = "Gray")

  p1 <- ggplot(LungeKinematics::AllWhalesAvgs) +
    geom_point(data = LungeKinematics::CleanedJawDataDR, aes(log10(TL), log10(BZigDR), fill = Species),
               alpha = 0.5,
               color = "black",
               shape = 22,
               size = 10) +
    geom_point(aes(log10(meanTotLength), log10(meanE2E), fill = Species),
               color = "black",
               shape = 21,
               size = 10) +
    geom_smooth(method = lm,
                aes(log10(meanTotLength), log10(meanE2E), group = Species),
                color = "black",
                se = TRUE) +
    geom_smooth(data = LungeKinematics::CleanedJawDataDR, method = lm,
                aes(log10(TL), log10(BZigDR), group = Species),
                color = "black",
                linetype = 2,
                se = TRUE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = bquote("Log"[10]*" Body Length (m)"),
         y = bquote("Log"[10]*" Bi-Zygomatic Width (m)")) +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p2 <- ggplot(LungeKinematics::AllWhalesAvgs) +
    geom_point(data = LungeKinematics::CleanedJawDataDR, aes(log10(TL), log10(LjawDR), fill = Species),
               alpha = 0.5,
               color = "black",
               shape = 22,
               size = 10) +
    geom_point(aes(log10(meanTotLength), log10(meanR2BH), fill = Species),
               color = "black",
               shape = 21,
               size = 10) +
    geom_smooth(method = lm,
                aes(log10(meanTotLength), log10(meanR2BH), group = Species),
                color = "black",
                se = TRUE) +
    geom_smooth(data = LungeKinematics::CleanedJawDataDR, method = lm,
                aes(log10(TL), log10(LjawDR), group = Species),
                color = "black",
                linetype = 2,
                se = TRUE) +
    scale_color_manual(values = pal, aesthetics = c("fill","colour")) +
    labs(x = bquote("Log"[10]*" Body Length (m)"),
         y = bquote("Log"[10]*" Rostrum-to-Blowhole Length (m)")) +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p3 <- cowplot::plot_grid(p1, p2,
                           nrow = 1,
                           ncol = 2,
                           align = "vh",
                           axis = "bl",
                           labels = NULL)

  ggsave("figs/Figure_Supp_5.pdf", height = 480, width = 960, units = "mm", dpi = 300)
}
