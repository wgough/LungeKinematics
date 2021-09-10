## code to prepare `SpeedSeries` dataset goes here
library(tidyverse)

AllWhalesMetaData <- read_csv("data-raw/AllWhalesMetadata.csv")

SpeedSeries <- read_csv("data-raw/SpdSeriesMaxSpdCenter.csv",
                        col_names = as.character(1:1001)) %>%
  mutate(IndNum = 1 + floor((row_number() - 1) / 30),
         lunge = 1 + (row_number() - 1) %% 30) %>%
  pivot_longer(1:1001, names_to = "Frame", values_to = "Speed") %>%
  mutate(Frame = as.numeric(Frame),
         secs = (Frame/10)-50) %>%
  filter(!is.nan(Speed)) %>%
  left_join(AllWhalesMetaData, by = "IndNum")

usethis::use_data(SpeedSeries, overwrite = TRUE)
