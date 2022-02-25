#' Create Supplementary Figure 2 for Gough et al. 2022
#'
#' @return
#' @export
#'
#' @examples
#' figS2()
figS2 <- function() {
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

  ggsave("figs/Figure_Supp_2.pdf", height = 720, width = 960, units = "mm", dpi = 300)
}

#Extra Stuff
#
#   p2 <- LongLungeBeats %>%
#     filter(Species == "Blue") %>%
#     ggplot(aes(x = Gyrobeats, y = OsFreq, fill = Species, group = Gyrobeats)) +
#     geom_smooth(aes(color = Individual, group = Lunge), se = FALSE) +
#     geom_boxplot() +
#     scale_shape_manual(values = c(21,22,23,24,25,26,27)) +
#     scale_x_discrete(limits = rev(levels(LongLungeBeats$Gyrobeats)),
#                      labels=c("6","5","4","3","2","1","0")) +
#     scale_fill_manual(values = pal) +
#     labs(x = "Tailbeats Prior To Lunge",
#          y = "Oscillatory Period of Tailbeat (s)") +
#     theme_classic(base_size = 10) +
#     theme(axis.text = element_text(size = 40),
#           axis.title = element_text(size = 48),
#           legend.position = "none",
#           panel.grid.minor = element_blank())
#
#   p3 <- LongLungeBeats %>%
#     filter(Species == "Humpback") %>%
#     ggplot(aes(x = Gyrobeats, y = OsFreq, fill = Species, group = Gyrobeats)) +
#     geom_smooth(aes(color = Individual, group = Lunge), se = FALSE) +
#     geom_boxplot() +
#     scale_shape_manual(values = c(21,22,23,24,25,26,27)) +
#     scale_x_discrete(limits = rev(levels(LongLungeBeats$Gyrobeats)),
#                      labels=c("6","5","4","3","2","1","0")) +
#     scale_fill_manual(values = pal) +
#     labs(x = "Tailbeats Prior To Lunge",
#          y = "Oscillatory Period of Tailbeat (s)") +
#     theme_classic(base_size = 10) +
#     theme(axis.text = element_text(size = 40),
#           axis.title = element_text(size = 48),
#           legend.position = "none",
#           panel.grid.minor = element_blank())
#
#   p4 <- LongLungeBeats %>%
#     filter(Species == "Minke") %>%
#     ggplot(aes(x = Gyrobeats, y = OsFreq, fill = Species, group = Gyrobeats)) +
#     geom_smooth(aes(color = Individual, group = Lunge), se = FALSE) +
#     geom_boxplot() +
#     scale_shape_manual(values = c(21,22,23,24,25,26,27)) +
#     scale_x_discrete(limits = rev(levels(LongLungeBeats$Gyrobeats)),
#                      labels=c("6","5","4","3","2","1","0")) +
#     scale_fill_manual(values = pal) +
#     labs(x = "Tailbeats Prior To Lunge",
#          y = "Oscillatory Period of Tailbeat (s)") +
#     theme_classic(base_size = 10) +
#     theme(axis.text = element_text(size = 40),
#           axis.title = element_text(size = 48),
#           legend.position = "none",
#           panel.grid.minor = element_blank())
#
#   p5 <- cowplot::plot_grid(p2, p3, p4,
#                            nrow = 3,
#                            ncol = 1,
#                            align = "v",
#                            axis = "bl",
#                            labels = NULL)
#
#   ggsave("figs/fig6test1.pdf", height = 960, width = 960, units = "mm", dpi = 300)
#
#
#   FinalGyroSummary <- summarySE(LungeKinematics::LungeBeatsTable, measurevar = "FinalGyroOsFreq", groupvar = c("whaleName"), na.rm = TRUE)
#
#   FinalGyroLength <- LungeKinematics::AllWhalesAvgs %>%
#     select(Species, whaleName, meanTotLength) %>%
#     left_join(FinalGyroSummary, FinalGyroLength, by = "whaleName") %>%
#     mutate(quart25 = FinalGyroOsFreq_mean-(0.5*iqr),
#            quart75 = FinalGyroOsFreq_mean+(0.5*iqr))
#
#   FinalMinusOneGyroSummary <- summarySE(LungeKinematics::LungeBeatsTable, measurevar = "FinalGyrMinusOne", groupvar = c("whaleName"), na.rm = TRUE)
#
#   FinalMinusOneGyroLength <- LungeKinematics::AllWhalesAvgs %>%
#     select(Species, whaleName, meanTotLength) %>%
#     left_join(FinalMinusOneGyroSummary, FinalMinusOneGyroLength, by = "whaleName") %>%
#     mutate(quart25 = FinalGyrMinusOne_mean-(0.5*iqr),
#            quart75 = FinalGyrMinusOne_mean+(0.5*iqr))
#
#   FinalMinusTwoGyroSummary <- summarySE(LungeKinematics::LungeBeatsTable, measurevar = "FinalGyrMinusTwo", groupvar = c("whaleName"), na.rm = TRUE)
#
#   FinalMinusTwoGyroLength <- LungeKinematics::AllWhalesAvgs %>%
#     select(Species, whaleName, meanTotLength) %>%
#     left_join(FinalMinusTwoGyroSummary, FinalMinusTwoGyroLength, by = "whaleName") %>%
#     mutate(quart25 = FinalGyrMinusTwo_mean-(0.5*iqr),
#            quart75 = FinalGyrMinusTwo_mean+(0.5*iqr))
#
#   FinalMinusThreeGyroSummary <- summarySE(LungeKinematics::LungeBeatsTable, measurevar = "FinalGyrMinusThree", groupvar = c("whaleName"), na.rm = TRUE)
#
#   FinalMinusThreeGyroLength <- LungeKinematics::AllWhalesAvgs %>%
#     select(Species, whaleName, meanTotLength) %>%
#     left_join(FinalMinusThreeGyroSummary, FinalMinusThreeGyroLength, by = "whaleName") %>%
#     mutate(quart25 = FinalGyrMinusThree_mean-(0.5*iqr),
#            quart75 = FinalGyrMinusThree_mean+(0.5*iqr))
#
#   p6 <- LongLungeBeats %>%
#     filter(Gyrobeats == c("FinalGyroOsFreq","FinalGyrMinusOne","FinalGyrMinusTwo","FinalGyrMinusThree")) %>%
#     ggplot(aes(x = TotLength, y = OsFreq, fill = Species)) +
#     geom_smooth(method = loess, aes(group = Gyrobeats, linetype = Gyrobeats), color = "black") +
#     geom_smooth(method = loess, aes(y = LengthDecel, fill = NULL), color = "red") +
#     geom_pointrange(data = FinalGyroLength, aes(meanTotLength, FinalGyroOsFreq_mean, color = Species, ymin = quart25, ymax = quart75), shape = 21) +
#     geom_pointrange(data = FinalMinusOneGyroLength, aes(meanTotLength, FinalGyrMinusOne_mean, color = Species, ymin = quart25, ymax = quart75), shape = 22) +
#     geom_pointrange(data = FinalMinusTwoGyroLength, aes(meanTotLength, FinalGyrMinusTwo_mean, color = Species, ymin = quart25, ymax = quart75), shape = 23) +
#     geom_pointrange(data = FinalMinusThreeGyroLength, aes(meanTotLength, FinalGyrMinusThree_mean, color = Species, ymin = quart25, ymax = quart75), shape = 24) +
#     geom_point(data = FinalGyroLength, aes(meanTotLength, FinalGyroOsFreq_mean, fill = Species), color = "black", size = 8, shape = 21) +
#     geom_point(data = FinalMinusOneGyroLength, aes(meanTotLength, FinalGyrMinusOne_mean, fill = Species), color = "black", size = 8, shape = 22) +
#     geom_point(data = FinalMinusTwoGyroLength, aes(meanTotLength, FinalGyrMinusTwo_mean, fill = Species), color = "black", size = 8, shape = 23) +
#     geom_point(data = FinalMinusThreeGyroLength, aes(meanTotLength, FinalGyrMinusThree_mean, fill = Species), color = "black", size = 8, shape = 24) +
#     scale_fill_manual(values = pal) +
#     scale_color_manual(values = pal) +
#     labs(x = "Body Length (m)",
#          y = "Oscillatory Period of Tailbeat (s)") +
#     theme_classic(base_size = 10) +
#     theme(axis.text = element_text(size = 40),
#           axis.title = element_text(size = 48),
#           legend.position = "none",
#           panel.grid.minor = element_blank())
#
#   ggsave("figs/fig6test2.pdf", height = 480, width = 960, units = "mm", dpi = 300)
#
#   p7 <- ggplot(LungeBeatsTable, aes(x = TotLength, y = FinalGyroOsFreq, fill = Species)) +
#     geom_point(shape = 21, size = 6) +
#     geom_smooth(method = lm, color = "black") +
#     #geom_abline(intercept = 0, slope = 1) +
#     scale_fill_manual(values = pal) +
#     labs(x = "Total Body Length (m))",
#          y = "Final Body Oscillation Period (s)") +
#     theme_classic(base_size = 10) +
#     theme(axis.text = element_text(size = 40),
#           axis.title = element_text(size = 48),
#           legend.position = "none",
#           panel.grid.minor = element_blank())
#   p7
#
#   p8 <- ggplot(LungeBeatsTable, aes(x = TotLength, y = LengthDecel, fill = Species)) +
#     geom_point(shape = 21, size = 6) +
#     geom_smooth(method = lm, color = "black") +
#     #geom_abline(intercept = 0, slope = 1) +
#     scale_fill_manual(values = pal) +
#     labs(x = "Total Body Length (m)",
#          y = "Length of Deceleration Period (s)") +
#     theme_classic(base_size = 10) +
#     theme(axis.text = element_text(size = 40),
#           axis.title = element_text(size = 48),
#           legend.position = "none",
#           panel.grid.minor = element_blank())
#   p8
#
#   p9 <- ggplot(LungeBeatsTable, aes(x = FinalGyroOsFreq, y = LengthDecel, fill = Species)) +
#     geom_point(shape = 21, size = 6) +
#     geom_smooth(method = lm, color = "black") +
#     geom_abline(intercept = 0, slope = 1) +
#     scale_fill_manual(values = pal) +
#     labs(x = "Final Body Oscillation Period (s)",
#          y = "Length of Deceleration Period (s)") +
#     theme_classic(base_size = 10) +
#     theme(axis.text = element_text(size = 40),
#           axis.title = element_text(size = 48),
#           legend.position = "none",
#           panel.grid.minor = element_blank())
#   p9
#
#   p10 <- ggplot(LungeBeatsTable, aes(x = SpdBegDecel, y = FinalGyroOsFreq, fill = Species)) +
#     geom_point(shape = 21, size = 6, na.rm= TRUE) +
#     geom_smooth(method = lm, color = "black") +
#     scale_fill_manual(values = pal) +
#     labs(x = "Speed at Mouth Opening (m s-1)",
#          y = "Final Body Oscillation Period (s)") +
#     theme_classic(base_size = 10) +
#     theme(axis.text = element_text(size = 40),
#           axis.title = element_text(size = 48),
#           legend.position = "none",
#           panel.grid.minor = element_blank())
#   p10
#
#   p11 <- ggplot(LungeBeatsTable, aes(x = SpdBegDecel, y = LengthDecel, fill = Species)) +
#     geom_point(shape = 21, size = 6, na.rm= TRUE) +
#     geom_smooth(method = lm, color = "black") +
#     scale_fill_manual(values = pal) +
#     labs(x = "Speed at Mouth Opening (m s-1)",
#          y = "Length of Deceleration Period (s)") +
#     theme_classic(base_size = 10) +
#     theme(axis.text = element_text(size = 40),
#           axis.title = element_text(size = 48),
#           legend.position = "none",
#           panel.grid.minor = element_blank())
#   p11
#
#   ggsave("figs/fig6test3.pdf", height = 480, width = 960, units = "mm", dpi = 300)
