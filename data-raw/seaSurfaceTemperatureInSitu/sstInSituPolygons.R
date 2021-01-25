rm(list=ls())
library(usethis)

sstInSituPolygons <- vector(mode = 'list', length = 2)
sstInSituPolygons[[1]][['stationName']] <- 'Halifax'
sstInSituPolygons[[1]][['longitude']] <- c(-63.62420, -63.73768, -63.58007, -63.43506)
sstInSituPolygons[[1]][['latitude']] <- c(44.79200, 44.70215, 44.57611, 44.68866)

sstInSituPolygons[[2]][['stationName']] <- 'St. Andrews'
sstInSituPolygons[[2]][['longitude']] <- c(-67.09121, -67.61493, -67.10431, -66.53694)
sstInSituPolygons[[2]][['latitude']] <- c(44.74649, 45.17409, 45.42973, 45.13704)

usethis::use_data(sstInSituPolygons, compress = 'xz', overwrite = TRUE)
