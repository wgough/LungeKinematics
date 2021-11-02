## code to prepare `LungeBeatsTable` dataset goes here
library(tidyverse)

LungeBeatsTable <- read_csv("data-raw/LungeBeatsTable.csv")

usethis::use_data(LungeBeatsTable, overwrite = TRUE)
