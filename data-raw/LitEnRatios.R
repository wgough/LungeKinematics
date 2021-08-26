## code to prepare `LitEnRatios` dataset goes here
library(tidyverse)

LitEnRatios <- read_csv("data-raw/LitEnRatios.csv")

usethis::use_data(LitEnRatios, overwrite = TRUE)

