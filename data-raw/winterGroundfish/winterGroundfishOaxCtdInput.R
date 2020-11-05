library(usethis)
library(oce)

# 3. Read in the data that was supplied to the oax
#     and create ctd profiles then extract the bottom temperature.
#    Require that there be at least 10 data points for each CTD
#     as some of the file have what looks like to be single points
#     and I don't know where these are coming from, are they bottom ? mid depth ?
#    For years 2007 to 2012, temperature and salinity fields were separated for some reason
#     this will be reflected in the code.
#    There are no headers in these files, but the column names are as follows
#    longitude, latitude, pressure, temperature, salinity, ... (other that we aren't concerned with)
inputDataDir <- 'data-raw/winterGroundfish/inputData'
inputDirs <- list.dirs(path = inputDataDir, recursive = FALSE)
# x number names of input data with longitude/latitude
# [Combined_Data]_year.txt
# [DFO_Data]_year.txt
# [Data]_year.txt
cnt <- 1
ctd <- vector(mode = 'list')
for(id in 1:length(inputDirs)){
  year <- as.numeric(tail(strsplit(inputDirs[id], split = '/',)[[1]], 1))
  oaxInputFile <- list.files(inputDirs[id],
                             pattern = '^(Combined_Data)?(DFO_Data)?(Data)?\\_[0-9]{4}\\.txt$',
                             full.names = TRUE)
  oaxinput <- read.table(oaxInputFile)
  names(oaxinput) <- c('latitude', 'longitude',
                       'yearday', 'pressure',
                       'temperature', 'salinity',
                       'weight')
  ulonlatctd <- unique(data.frame(longitude = oaxinput$longitude, latitude = oaxinput$latitude))

  for (j in 1:dim(ulonlatctd)[1]){
    lonlook <- ulonlatctd$longitude[j]
    latlook <- ulonlatctd$latitude[j]
    ok <- which(oaxinput$longitude == lonlook & oaxinput$latitude == latlook)
    # deciding to keep all data.
    #{if(length(ok) < 5){
    #  next
    #} else {
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
                                   year)
      cnt <- cnt + 1
  }
}


winterGroundfishOaxCtdInput <- ctd
usethis::use_data(winterGroundfishOaxCtdInput, compress = 'xz', overwrite = TRUE)


# # DFO and US station locations
# dfoStnFile <- 'dfo_stations.txt'
# okdfo <- file.exists(paste(inputDataDir, dfoStnFile, sep = '/'))
# if(okdfo){
#   dfoStn <- read.table(paste(inputDataDir, dfoStnFile, sep = '/'))
#   names(dfoStn) <- c('latitude', 'longitude')
# }
# usStnFile <- 'us_stations.txt'
# okus <- file.exists(paste(inputDataDir, usStnFile, sep = '/'))
# if(okus){
#   usStn <- read.table(paste(inputDataDir, usStnFile, sep = '/'))
#   names(usStn) <- c('latitude', 'longitude')
# }
