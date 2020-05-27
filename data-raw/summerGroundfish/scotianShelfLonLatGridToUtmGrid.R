library(oce)
library(usethis)
# make new gridFile
llgridFile <- 'data-raw/summerGroundfish/summerGroundfishLonLatGrid.txt'
summerGroundfishLonLatGrid <- read.table(llgridFile)

utmcoord <- lonlat2utm(longitude = scotianShelfLonLatGrid[,2],
                       latitude = scotianShelfLonLatGrid[,1],
                       zone = 19,
                       km = TRUE)

summerGroundfishUtmGrid <- cbind(utmcoord$easting,
                             utmcoord$northing,
                             scotianShelfLonLatGrid[,3:dim(scotianShelfLonLatGrid)[2]])

write.table(scotianShelfUtmGrid, file = 'data-raw/summerGroundfish/summerGroundfishUtmGrid.txt', sep = " ",
            col.names = FALSE, row.names = FALSE)
usethis::use_data(summerGroundfishLonLatGrid, compress = "xz", overwrite = T)
usethis::use_data(summerGroundfishUtmGrid, compress = "xz", overwrite = T)
