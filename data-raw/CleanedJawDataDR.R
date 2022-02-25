## code to prepare `CleanedJawDataDR` dataset (from Discovery Reports and digitized by Jeremy Goldbogen) goes here
library(tidyverse)

CleanedJawDataDR <- read_csv("data-raw/CleanedJawDataDR.csv")

usethis::use_data(CleanedJawDataDR, overwrite = TRUE)
