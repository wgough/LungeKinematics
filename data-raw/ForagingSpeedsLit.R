## code to prepare `ForagingSpeedsLit` dataset goes here
library(tidyverse)

ForagingSpeedsLit <- read_csv("data-raw/ForagingSpeedsLit.csv")

usethis::use_data(ForagingSpeedsLit, overwrite = TRUE)

