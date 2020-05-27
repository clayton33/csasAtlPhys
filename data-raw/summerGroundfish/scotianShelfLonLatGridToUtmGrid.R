library(oce)
library(usethis)
# make new gridFile
llgridFile <- 'data-raw/scotianShelfLonLatGrid.txt'
scotianShelfLonLatGrid <- read.table(llgridFile)

utmcoord <- lonlat2utm(longitude = scotianShelfLonLatGrid[,2],
                       latitude = scotianShelfLonLatGrid[,1],
                       zone = 19,
                       km = TRUE)

scotianShelfUtmGrid <- cbind(utmcoord$easting,
                             utmcoord$northing,
                             scotianShelfLonLatGrid[,3:dim(scotianShelfLonLatGrid)[2]])

write.table(scotianShelfUtmGrid, file = 'data-raw/scotianShelfUtmGrid.txt', sep = " ",
            col.names = FALSE, row.names = FALSE)
usethis::use_data(scotianShelfLonLatGrid, compress = "xz", overwrite = T)
usethis::use_data(scotianShelfUtmGrid, compress = "xz", overwrite = T)
