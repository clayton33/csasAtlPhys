library(usethis)

stn2lat <- c(44.5, 44.2, 43.95, 44.25)
stn2lon <- c(-63.0, -63.0, -63.6, -63.6)

station2Polygon <- list(longitude = stn2lon,
                          latitude = stn2lat)

usethis::use_data(station2Polygon, compress = 'xz', overwrite = TRUE)
