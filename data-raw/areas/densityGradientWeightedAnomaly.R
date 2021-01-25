library(usethis)
path <- 'data-raw/areas/weightedAnomalies'
files <- list.files(path = path)
densityGradientWeightedAnomaly <- read.csv(paste(path, files, sep = '/'))
usethis::use_data(densityGradientWeightedAnomaly, compress = "xz", overwrite = T)

