library(oce)
library(usethis)
# make new gridFile
llgridFile <- 'data-raw/summerGroundfish/summerGroundfishLonLatGrid.txt'
summerGroundfishLonLatGrid <- read.table(llgridFile)

utmcoord <- lonlat2utm(longitude = summerGroundfishLonLatGrid[,2],
                       latitude = summerGroundfishLonLatGrid[,1],
                       zone = 19,
                       km = TRUE)

summerGroundfishUtmGrid <- cbind(utmcoord$easting,
                             utmcoord$northing,
                             summerGroundfishLonLatGrid[,3:dim(summerGroundfishLonLatGrid)[2]])

write.table(summerGroundfishUtmGrid, file = 'data-raw/summerGroundfish/summerGroundfishUtmGrid.txt', sep = " ",
            col.names = FALSE, row.names = FALSE)
usethis::use_data(summerGroundfishLonLatGrid, compress = "xz", overwrite = T)
usethis::use_data(summerGroundfishUtmGrid, compress = "xz", overwrite = T)
