#' Create Figure 6
#'
#' @return
#' @export
#'
#' @examples
#' fig6()
fig6 <- function() {
  pal <- c("Minke" = "#009E73",  "Humpback" = "#D55E00",  "Blue" = "#0072B2", "Fin" = "#9100B2", "Antarctic" = "Black", "Monterey" = "Gray")

  LongLungeBeats <- LungeKinematics::LungeBeatsTable %>%
    pivot_longer(cols = c(FinalGyroOsFreq:FinalGyrMinusSix),
                 names_to = "Gyrobeats",
                 values_to = "OsFreq",
                 values_drop_na = TRUE) %>%
    filter(Species != "Fin")
  LongLungeBeats$Gyrobeats <- as.character(LongLungeBeats$Gyrobeats)
  LongLungeBeats$Gyrobeats <- factor(LongLungeBeats$Gyrobeats, levels=unique(LongLungeBeats$Gyrobeats))

  p1 <- ggplot(LongLungeBeats, aes(x = Gyrobeats, y = OsFreq, fill = Species,
                                   group = fct_cross(Gyrobeats, Species))) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(position=position_jitterdodge(jitter.width = 0.1),alpha=0.2, shape = 21, size = 6) +
    scale_x_discrete(limits = rev(levels(LongLungeBeats$Gyrobeats)),
                     labels=c("6","5","4","3","2","1","0")) +
    scale_fill_manual(values = pal) +
    labs(x = "Tailbeats Prior To Lunge",
         y = "Oscillatory Frequency of Tailbeat (Hz)") +
    theme_classic(base_size = 10) +
    theme(axis.text = element_text(size = 40),
          axis.title = element_text(size = 48),
          legend.position = "none",
          panel.grid.minor = element_blank())

  ggsave("figs/fig6.pdf", height = 480, width = 960, units = "mm", dpi = 300)
}


