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
    mutate(Lunge = row_number()) %>%
    drop_na(FinalGyroOsFreq) %>%
    pivot_longer(cols = c(FinalGyroOsFreq:FinalGyrMinusSix),
                 names_to = "Gyrobeats",
                 values_to = "OsFreq",
                 values_drop_na = TRUE) %>%
    group_by(Lunge) %>%
    filter(Species != "Fin")
  LongLungeBeats$Gyrobeats <- as.character(LongLungeBeats$Gyrobeats)
  LongLungeBeats$Gyrobeats <- factor(LongLungeBeats$Gyrobeats, levels=unique(LongLungeBeats$Gyrobeats))
  LongLungeBeats$Individual <- as.character(LongLungeBeats$Individual)
  LongLungeBeats$Individual <- factor(LongLungeBeats$Individual, levels=unique(LongLungeBeats$Individual))

  p1 <- ggplot(LongLungeBeats, aes(x = Gyrobeats, y = OsFreq, fill = Species,
                                   group = fct_cross(Gyrobeats, Species))) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(position=position_jitterdodge(jitter.width = 0.1),alpha=0.2, shape = 21, size = 6) +
    scale_x_discrete(limits = rev(levels(LongLungeBeats$Gyrobeats)),
                     labels=c("6","5","4","3","2","1","0")) +
    scale_fill_manual(values = pal) +
    labs(x = "Tailbeats Prior To Lunge",
         y = "Oscillatory Period of Tailbeat (s)") +
    theme_classic(base_size = 10) +
    theme(axis.text = element_text(size = 40),
          axis.title = element_text(size = 48),
          legend.position = "none",
          panel.grid.minor = element_blank())

  ggsave("figs/fig6.pdf", height = 720, width = 960, units = "mm", dpi = 300)

  p2 <- LongLungeBeats %>%
    filter(Species == "Blue") %>%
    ggplot(aes(x = Gyrobeats, y = OsFreq, fill = Species, group = Gyrobeats)) +
    geom_smooth(aes(color = Individual, group = Lunge), se = FALSE) +
    geom_boxplot() +
    scale_shape_manual(values = c(21,22,23,24,25,26,27)) +
    scale_x_discrete(limits = rev(levels(LongLungeBeats$Gyrobeats)),
                     labels=c("6","5","4","3","2","1","0")) +
    scale_fill_manual(values = pal) +
    labs(x = "Tailbeats Prior To Lunge",
         y = "Oscillatory Period of Tailbeat (s)") +
    theme_classic(base_size = 10) +
    theme(axis.text = element_text(size = 40),
          axis.title = element_text(size = 48),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p3 <- LongLungeBeats %>%
    filter(Species == "Humpback") %>%
    ggplot(aes(x = Gyrobeats, y = OsFreq, fill = Species, group = Gyrobeats)) +
    geom_smooth(aes(color = Individual, group = Lunge), se = FALSE) +
    geom_boxplot() +
    scale_shape_manual(values = c(21,22,23,24,25,26,27)) +
    scale_x_discrete(limits = rev(levels(LongLungeBeats$Gyrobeats)),
                     labels=c("6","5","4","3","2","1","0")) +
    scale_fill_manual(values = pal) +
    labs(x = "Tailbeats Prior To Lunge",
         y = "Oscillatory Period of Tailbeat (s)") +
    theme_classic(base_size = 10) +
    theme(axis.text = element_text(size = 40),
          axis.title = element_text(size = 48),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p4 <- LongLungeBeats %>%
    filter(Species == "Minke") %>%
    ggplot(aes(x = Gyrobeats, y = OsFreq, fill = Species, group = Gyrobeats)) +
    geom_smooth(aes(color = Individual, group = Lunge), se = FALSE) +
    geom_boxplot() +
    scale_shape_manual(values = c(21,22,23,24,25,26,27)) +
    scale_x_discrete(limits = rev(levels(LongLungeBeats$Gyrobeats)),
                     labels=c("6","5","4","3","2","1","0")) +
    scale_fill_manual(values = pal) +
    labs(x = "Tailbeats Prior To Lunge",
         y = "Oscillatory Period of Tailbeat (s)") +
    theme_classic(base_size = 10) +
    theme(axis.text = element_text(size = 40),
          axis.title = element_text(size = 48),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p5 <- cowplot::plot_grid(p2, p3, p4,
                           nrow = 3,
                           ncol = 1,
                           align = "v",
                           axis = "bl",
                           labels = NULL)

  ggsave("figs/fig6test1.pdf", height = 960, width = 960, units = "mm", dpi = 300)


  FinalGyroSummary <- summarySE(LungeKinematics::LungeBeatsTable, measurevar = "FinalGyroOsFreq", groupvar = c("whaleName"), na.rm = TRUE)

  FinalGyroLength <- LungeKinematics::AllWhalesAvgs %>%
    select(Species, whaleName, meanTotLength) %>%
    left_join(FinalGyroSummary, FinalGyroLength, by = "whaleName") %>%
    mutate(quart25 = FinalGyroOsFreq_mean-(0.5*iqr),
           quart75 = FinalGyroOsFreq_mean+(0.5*iqr))

  FinalMinusOneGyroSummary <- summarySE(LungeKinematics::LungeBeatsTable, measurevar = "FinalGyrMinusOne", groupvar = c("whaleName"), na.rm = TRUE)

  FinalMinusOneGyroLength <- LungeKinematics::AllWhalesAvgs %>%
    select(Species, whaleName, meanTotLength) %>%
    left_join(FinalMinusOneGyroSummary, FinalMinusOneGyroLength, by = "whaleName") %>%
    mutate(quart25 = FinalGyrMinusOne_mean-(0.5*iqr),
           quart75 = FinalGyrMinusOne_mean+(0.5*iqr))

  FinalMinusTwoGyroSummary <- summarySE(LungeKinematics::LungeBeatsTable, measurevar = "FinalGyrMinusTwo", groupvar = c("whaleName"), na.rm = TRUE)

  FinalMinusTwoGyroLength <- LungeKinematics::AllWhalesAvgs %>%
    select(Species, whaleName, meanTotLength) %>%
    left_join(FinalMinusTwoGyroSummary, FinalMinusTwoGyroLength, by = "whaleName") %>%
    mutate(quart25 = FinalGyrMinusTwo_mean-(0.5*iqr),
           quart75 = FinalGyrMinusTwo_mean+(0.5*iqr))

  FinalMinusThreeGyroSummary <- summarySE(LungeKinematics::LungeBeatsTable, measurevar = "FinalGyrMinusThree", groupvar = c("whaleName"), na.rm = TRUE)

  FinalMinusThreeGyroLength <- LungeKinematics::AllWhalesAvgs %>%
    select(Species, whaleName, meanTotLength) %>%
    left_join(FinalMinusThreeGyroSummary, FinalMinusThreeGyroLength, by = "whaleName") %>%
    mutate(quart25 = FinalGyrMinusThree_mean-(0.5*iqr),
           quart75 = FinalGyrMinusThree_mean+(0.5*iqr))

  p6 <- LongLungeBeats %>%
    filter(Gyrobeats == c("FinalGyroOsFreq","FinalGyrMinusOne","FinalGyrMinusTwo","FinalGyrMinusThree")) %>%
    ggplot(aes(x = TotLength, y = OsFreq, fill = Species)) +
    geom_smooth(method = loess, aes(group = Gyrobeats, linetype = Gyrobeats), color = "black") +
    geom_smooth(method = loess, aes(y = LengthDecel, fill = NULL), color = "red") +
    geom_pointrange(data = FinalGyroLength, aes(meanTotLength, FinalGyroOsFreq_mean, color = Species, ymin = quart25, ymax = quart75), shape = 21) +
    geom_pointrange(data = FinalMinusOneGyroLength, aes(meanTotLength, FinalGyrMinusOne_mean, color = Species, ymin = quart25, ymax = quart75), shape = 22) +
    geom_pointrange(data = FinalMinusTwoGyroLength, aes(meanTotLength, FinalGyrMinusTwo_mean, color = Species, ymin = quart25, ymax = quart75), shape = 23) +
    geom_pointrange(data = FinalMinusThreeGyroLength, aes(meanTotLength, FinalGyrMinusThree_mean, color = Species, ymin = quart25, ymax = quart75), shape = 24) +
    geom_point(data = FinalGyroLength, aes(meanTotLength, FinalGyroOsFreq_mean, fill = Species), color = "black", size = 8, shape = 21) +
    geom_point(data = FinalMinusOneGyroLength, aes(meanTotLength, FinalGyrMinusOne_mean, fill = Species), color = "black", size = 8, shape = 22) +
    geom_point(data = FinalMinusTwoGyroLength, aes(meanTotLength, FinalGyrMinusTwo_mean, fill = Species), color = "black", size = 8, shape = 23) +
    geom_point(data = FinalMinusThreeGyroLength, aes(meanTotLength, FinalGyrMinusThree_mean, fill = Species), color = "black", size = 8, shape = 24) +
    scale_fill_manual(values = pal) +
    scale_color_manual(values = pal) +
    labs(x = "Body Length (m)",
         y = "Oscillatory Period of Tailbeat (s)") +
    theme_classic(base_size = 10) +
    theme(axis.text = element_text(size = 40),
          axis.title = element_text(size = 48),
          legend.position = "none",
          panel.grid.minor = element_blank())

  # p6Log10 <- LongLungeBeats %>%
  #   filter(Gyrobeats == c("FinalGyroOsFreq","FinalGyrMinusOne","FinalGyrMinusTwo","FinalGyrMinusThree")) %>%
  #   ggplot(aes(x = log10(TotLength), y = log10(OsFreq), fill = Species)) +
  #   geom_smooth(method = loess, aes(group = Gyrobeats, linetype = Gyrobeats), color = "black") +
  #   geom_smooth(method = loess, aes(y = log10(LengthDecel), fill = NULL), color = "red") +
  #   geom_pointrange(data = FinalGyroLength, aes(log10(meanTotLength), log10(FinalGyroOsFreq_mean), color = Species, ymin = log10(quart25), ymax = log10(quart75)), shape = 21) +
  #   geom_pointrange(data = FinalMinusOneGyroLength, aes(log10(meanTotLength), log10(FinalGyrMinusOne_mean), color = Species, ymin = log10(quart25), ymax = log10(quart75)), shape = 22) +
  #   geom_pointrange(data = FinalMinusTwoGyroLength, aes(log10(meanTotLength), log10(FinalGyrMinusTwo_mean), color = Species, ymin = log10(quart25), ymax = log10(quart75)), shape = 23) +
  #   geom_pointrange(data = FinalMinusThreeGyroLength, aes(log10(meanTotLength), log10(FinalGyrMinusThree_mean), color = Species, ymin = log10(quart25), ymax = log10(quart75)), shape = 24) +
  #   geom_point(data = FinalGyroLength, aes(log10(meanTotLength), log10(FinalGyroOsFreq_mean), fill = Species), color = "black", size = 8, shape = 21) +
  #   geom_point(data = FinalMinusOneGyroLength, aes(log10(meanTotLength), log10(FinalGyrMinusOne_mean), fill = Species), color = "black", size = 8, shape = 22) +
  #   geom_point(data = FinalMinusTwoGyroLength, aes(log10(meanTotLength), log10(FinalGyrMinusTwo_mean), fill = Species), color = "black", size = 8, shape = 23) +
  #   geom_point(data = FinalMinusThreeGyroLength, aes(log10(meanTotLength), log10(FinalGyrMinusThree_mean), fill = Species), color = "black", size = 8, shape = 24) +
  #   scale_fill_manual(values = pal) +
  #   scale_color_manual(values = pal) +
  #   labs(x = "Tailbeats Prior To Lunge",
  #        y = "Oscillatory Period of Tailbeat (s)") +
  #   theme_classic(base_size = 10) +
  #   theme(axis.text = element_text(size = 40),
  #         axis.title = element_text(size = 48),
  #         legend.position = "none",
  #         panel.grid.minor = element_blank())
  # p6Log10

  ggsave("figs/fig6test2.pdf", height = 480, width = 960, units = "mm", dpi = 300)
}


