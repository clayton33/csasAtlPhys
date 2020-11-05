library(usethis)
library(oce)
inputDataDir <- './data-raw/summerGroundfish/inputData/'
years <- 1970:2018

ctd <- vector(mode = 'list')
cnt <- 1
for(year in years){
  strangeYears <- 2007:2012
  {if(!(year %in% strangeYears)){
    oaxinput <- read.table(paste0(inputDataDir, paste0('data_', year, '.txt')))
    oaxinput <- oaxinput[,1:5]
    names(oaxinput) <- c('latitude', 'longitude', 'pressure', 'temperature', 'salinity')
  } else {
    oaxinputT <- read.table(paste0(inputDataDir, paste0('data_', year, '_Temp.txt')))
    oaxinputT <- oaxinputT[,1:4]
    names(oaxinputT) <- c('latitude', 'longitude', 'pressure', 'temperature')
    oaxinputS <- read.table(paste0(inputDataDir, paste0('data_', year, '_Salinity.txt')))
    oaxinputS <- oaxinputS[,1:4]
    names(oaxinputS) <- c('latitude', 'longitude', 'pressure', 'salinity')
    oaxinput <- merge(x = oaxinputT, y = oaxinputS , by = c('latitude', 'longitude', 'pressure'),
                    all = TRUE)
  }}
  ulonlatctd <- unique(data.frame(longitude = oaxinput$longitude, latitude = oaxinput$latitude))
  for (j in 1:dim(ulonlatctd)[1]){
    lonlook <- ulonlatctd$longitude[j]
    latlook <- ulonlatctd$latitude[j]
    ok <- which(oaxinput$longitude == lonlook & oaxinput$latitude == latlook)
    {if(length(ok) < 5){
      next
    } else {
      data <- oaxinput[ok, ]
      op <- order(data$pressure)
      data <- data[op, ]
      ctd[[cnt]] <- as.ctd(salinity = oaxinput$salinity[ok],
                           temperature = oaxinput$temperature[ok],
                           pressure = oaxinput$pressure[ok],
                           longitude = lonlook,
                           latitude = latlook)
      ctd[[cnt]] <- oceSetMetadata(ctd[[cnt]],
                     name = 'year',
                     value = year)
      cnt <- cnt + 1
  }}
  }
}

summerGroundfishOaxCtdInput <- ctd
usethis::use_data(summerGroundfishOaxCtdInput, compress = 'xz', overwrite = TRUE)
