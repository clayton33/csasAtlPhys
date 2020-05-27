library(usethis)
file <- 'data-raw/prince5/P5_Data.csv'
prince5monthlyData <- read.csv(file, skip = 1)
usethis::use_data(prince5monthlyData, compress = "xz", overwrite = T)
