## code to prepare `LitMaxSpeeds` dataset goes here
library(tidyverse)

LitMaxSpeeds <- read_csv("data-raw/LitMaxSpeeds.csv")

usethis::use_data(LitMaxSpeeds, overwrite = TRUE)

