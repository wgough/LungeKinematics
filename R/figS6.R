#' Create Supplementary Figure 6 for Gough et al. 2022 - Variance Testing Mouth Open Speed
#' # "Minke" = "#009E73"
#' # "Humpback" = "#D55E00"
#' # "Blue" = "#0072B2"
#'
#' @return
#' @export
#'
#' @examples
#' figS6()
figS6 <- function() {
  # read in minke whale data
  VarianceMinke <- read_csv("data-raw/VarianceMinke.csv")

  # create a function that takes a sample from the mouth open speed for minke whale
  sample.n = function(n){sample(VarianceMinke$SpdBegDecel, n, replace=FALSE)}

  # create and empty dataframe to store results
  df.sample.var = data.frame()

  # for loop to fill in df.sample.var with variances using the var function
  for(i in 2:length(VarianceMinke$SpdBegDecel)){
    tempDF <- data.frame(sample.variance=replicate(1000,var(sample.n(i))), n=i)
    if(i==2) df.sample.var <- tempDF else df.sample.var <-rbind(df.sample.var, tempDF)
  }

  # create a summary datafile for all of the variances per sample size and finds the added variance with one extra n
  MinkeVarSummary <-  df.sample.var %>%
    group_by(n) %>%
    summarize(mean = mean(sample.variance)) %>%
    mutate(addedvar = ifelse(row_number()==1,NA, mean-lag(mean)))

  # create plot of average variances vs the sample size
  p1 <- ggplot(df.sample.var, aes(x=n, y=sample.variance))+
    geom_point(color="#009E73") +
    geom_vline(xintercept = 30) +
    ylim(0,3) +
    labs(x = "n",
         y = "Sample Variance - Speed of Mouth Opening") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
          legend.position = "none",
          panel.grid.minor = element_blank())

  # read in humpback whale data
  VarianceHumpback <- read_csv("data-raw/VarianceHumpback.csv")

  # create a function that takes a sample from the mouth open speed for humpback whale
  sample.n = function(n){sample(VarianceHumpback$SpdBegDecel, n, replace=FALSE)}

  # create and empty dataframe to store results
  df.sample.var = data.frame()

  # for loop to fill in df.sample.var with variances using the var function
  for(i in 2:length(VarianceHumpback$SpdBegDecel)){
    tempDF <- data.frame(sample.variance=replicate(1000,var(sample.n(i))), n=i)
    if(i==2) df.sample.var <- tempDF else df.sample.var <-rbind(df.sample.var, tempDF)
  }

  # create a summary datafile for all of the variances per sample size and finds the added variance with one extra n
  HumpbackVarSummary <-  df.sample.var %>%
    group_by(n) %>%
    summarize(mean = mean(sample.variance)) %>%
    mutate(addedvar = ifelse(row_number()==1,NA, mean-lag(mean)))

  # create plot of average variances vs the sample size
  p2 <- ggplot(df.sample.var, aes(x=n, y=sample.variance))+
    geom_point(color="#D55E00") +
    geom_vline(xintercept = 30) +
    ylim(0,3) +
    labs(x = "n",
         y = "Sample Variance - Speed of Mouth Opening") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  # read in blue whale variance data
  VarianceBlue <- read_csv("data-raw/VarianceBlue.csv")

  # create a function that takes a sample from the mouth open speed for blue whale
  sample.n = function(n){sample(VarianceBlue$SpdBegDecel, n, replace=FALSE)}

  # create and empty dataframe to store results
  df.sample.var = data.frame()

  # for loop to fill in df.sample.var with variances using the var function
  for(i in 2:length(VarianceBlue$SpdBegDecel)){
    tempDF <- data.frame(sample.variance=replicate(1000,var(sample.n(i))), n=i)
    if(i==2) df.sample.var <- tempDF else df.sample.var <-rbind(df.sample.var, tempDF)
  }

  # create a summary datafile for all of the variances per sample size and finds the added variance with one extra n
  BlueVarSummary <-  df.sample.var %>%
    group_by(n) %>%
    summarize(mean = mean(sample.variance)) %>%
    mutate(addedvar = ifelse(row_number()==1,NA, mean-lag(mean)))

  # create plot of average variances vs the sample size
  p3 <- ggplot(df.sample.var, aes(x=n, y=sample.variance))+
    geom_point(color="#0072B2") +
    geom_vline(xintercept = 30) +
    ylim(0,3) +
    labs(x = "n",
         y = "Sample Variance - Speed of Mouth Opening") +
    theme_classic(base_size = 8) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank())

  p4 <- cowplot::plot_grid(p1, p2, p3,
                           nrow = 1,
                           ncol = 3,
                           align = "vh",
                           axis = "bl",
                           labels = NULL)

  ggsave("figs/Figure_Supp_6.pdf", height = 480, width = 960, units = "mm", dpi = 300)
}
