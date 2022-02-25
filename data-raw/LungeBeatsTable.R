## code to prepare `LungeBeatsTable` dataset goes here
library(tidyverse)

AllWhalesLungeTableTrunc <- read_csv("data-raw/AllWhalesLungeTableTrunc.csv")

QuickUMO <- AllWhalesLungeTableTrunc %>%
  select(whaleName,SpdBegDecel)

LungeBeatsTable <- read_csv("data-raw/LungeBeatsTable.csv") %>%
  left_join(QuickUMO, by = "whaleName")

usethis::use_data(LungeBeatsTable, overwrite = TRUE)
