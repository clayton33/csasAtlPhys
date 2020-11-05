library(usethis)
file <- 'data-raw/summerGroundfish/cilGrid.csv'
summerGroundfishCilGrid <- na.omit(read.csv(file))
usethis::use_data(summerGroundfishCilGrid, compress = 'xz', overwrite = TRUE)
