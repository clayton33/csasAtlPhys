rm(list = ls())
library(oce)
library(usethis)

# make new gridFile
llgridFile <- 'data-raw/winterGroundfish/oegrid_ll.txt'
llgrid <- read.table(llgridFile)
utmcoord <- lonlat2utm(longitude = llgrid[,2],
                       latitude = llgrid[,1],
                       zone = 19,
                       km = TRUE)
utmgrid <- cbind(utmcoord$easting, utmcoord$northing, llgrid[,3:dim(llgrid)[2]])

winterGroundfishLonLatGrid <- llgrid
winterGroundfishUtmGrid <- utmgrid

usethis::use_data(winterGroundfishLonLatGrid, compress = "xz", overwrite = T)
usethis::use_data(winterGroundfishUtmGrid, compress = "xz", overwrite = T)
