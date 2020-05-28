rm(list = ls())
library(usethis)
library(oce)
# make new gridFile
llgridFile <- 'data-raw/snowCrab/newllgrid.txt'
llgrid <- read.table(llgridFile)

utmcoord <- lonlat2utm(longitude = llgrid[,2],
                       latitude = llgrid[,1],
                       zone = 20, # based on test using first grid point
                       km = TRUE)

snowCrabUtmGrid <- cbind(utmcoord$easting, utmcoord$northing, llgrid[,3:dim(llgrid)[2]])
snowCrabLonLatGrid <- llgrid

usethis::use_data(snowCrabLonLatGrid, compress = 'xz', overwrite = TRUE)
usethis::use_data(snowCrabUtmGrid, compress = 'xz', overwrite = TRUE)
