rm(list=ls())
library(rgdal)
library(usethis)
nafoPath <- 'data-raw/nafo/Divisions'
shpfiles <- list.files(path = nafoPath, pattern = '*.shp$')
file <- scan(text = paste(nafoPath, shpfiles, sep = '/'),
              what = character())
shp <- readOGR(dsn = nafoPath)
# we want 4x, 4v, 4w, 4Vs, 4Vn
azmpzones <- c('4X', '4W', '4Vs', '4Vn', '5Ze', '5Zw', '5Y')
okzone <- shp$ZONE %in% azmpzones

zonepoly <- shp@polygons[okzone]

nafoZones <- vector(mode = 'list', length = length(azmpzones))
for (i in 1:length(azmpzones)){
  zone <- shp$ZONE[okzone][i]
  longitude <- zonepoly[[i]]@Polygons[[1]]@coords[,1]
  latitude <- zonepoly[[i]]@Polygons[[1]]@coords[,2]
  nafoZones[[i]]$zoneName <- as.character(zone)
  nafoZones[[i]]$longitude <- longitude
  nafoZones[[i]]$latitude <- latitude
}

usethis::use_data(nafoZones, compress = "xz", overwrite = T)
