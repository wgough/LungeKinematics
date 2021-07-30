## code to prepare `AllWhalesLungeTableTrunc` dataset goes here
library(tidyverse)

AllOsFreqs <- read_csv("data-raw/AllOsFreqs.csv")

AllWhalesLungeTableTrunc <- read_csv("data-raw/AllWhalesLungeTableTrunc.csv") %>%
  left_join(AllOsFreqs, by = "whaleName") %>%
  mutate(Period = 1/AvgFreq,
         halfPeriod = Period/2) # %>%
  # select(-LungeI,-timeoflunge)

usethis::use_data(AllWhalesLungeTableTrunc, overwrite = TRUE)
