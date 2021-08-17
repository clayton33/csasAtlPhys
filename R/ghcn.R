#' @title Download temperature GHCN data files
#'
#' @description This function downloads the two files associated with the Global Historical
#' Climate Network (GHNC) mean temperature data.
#'
#' @details Download the GHCN mean temperature data. As of now, it only obtains the quality
#' controlled adjusted data, below is a table summarizing the other types of products
#' \tabular{ll}{
#' \strong{product code} \tab \strong{product description} \cr
#' qcu \tab quality controlled \cr
#' qcf \tab quality controlled, adjusted using pairewise homogeneity algorithm \cr
#' qfe \tab quality controlled, adjusted using pairewise homogenetiy algorithm, 1961-2010
#' }
#'
#' @param destdir Optional string indicating the directory in which to store downloaded files.
#' If not supplied, "." is used, i.e. the data file is stored in the present working directory.
#'
#' @author Chantelle Layton
#'
#' @importFrom utils download.file
#'
#' @export
#'

download.ghcn <- function(destdir = '.') {

  ftp <- 'ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v4'  # define two files associated with temperature.
  ftpFile <- 'ghcnm.tavg.latest.qcf.tar.gz'

  if(!dir.exists(destdir)){
    dir.create(destdir, recursive = TRUE)
  }
  download.file(url = paste(ftp, ftpFile, sep = '/'),
                destfile = paste(destdir, ftpFile, sep = '/'))
}

#' @title Read in ghcn data
#' @author Chantelle Layton
#'
#' @description This function reads in Global Historical Climatology Network monthly (GHCNm)
#' data that is downloaded using `download.ghcn`.
#'
#' @param dataFile a character string giving the name of the file to read, this will be the `.dat` file
#' @param metadataFile a character string giving the name of the file to read, this will be the `.inv` file
#' @param stationId a character string of the station ID to extract
#'
#' @return a list that is in similar nature to the ahccd format, it contains some metadata,
#'  `stationId`, `stationName`, `longitude`, `latitude`, and `elevation`, as well as a
#'  data that contains `year`, `month`, `temperature`, and `flag`.
#'
#' @export

read.ghcn <- function(dataFile, metadataFile, stationId = NULL){
  if(is.null(stationId)){
    stop('Please provide a stationId, code not developed yet to give all data due to size.')
  }
  # parse the meta data file
  rlm <- readLines(metadataFile)
  stnidm <- substr(rlm, start = 1, stop = 11)
  okstn <- stnidm %in% stationId
  rlm <- rlm[okstn]
  stnidm <- stnidm[okstn]
  latitude <- as.numeric(substr(rlm, start = 13, stop = 20))
  longitude <- as.numeric(substr(rlm, start = 22, stop = 30))
  stnelev <- as.numeric(substr(rlm, start = 32, stop = 37))
  stnname <- trimws(substr(rlm, start = 39, stop = 68), which = 'both')
  #dfmeta <- data.frame(stationId = stnidm, longitude = longitude, latitude = latitude, stationElevation = stnelev, stationName = stnname)
  #tibmeta <- as_tibble(dfmeta)
  # parse the data file
  rl <- readLines(dataFile)
  stnid <- substr(rl, start = 1, stop = 11)
  okdstn <- stnid %in% stationId
  rl <- rl[okdstn]
  stnid <- stnid[okdstn]
  year <- as.numeric(substr(rl, start = 12, stop = 15))
  element <- substr(rl, start = 16, stop = 19)
  # get the data and flags for each months data
  months <- tolower(month.abb)
  flagnames <- c('dm', 'qc', 'ds')
  #alldata <- data.frame(stationId = stnid, year = year, element = element)
  #value <- dmflag <- qcflag <- dsflag <- vector(mode = 'list', length = 12)
  alldata <- NULL
  for (i in 1:12){
    #cat(paste0('Getting data for month : ',i), sep = '\n')
    idx1 <- (i-1)*8 + 20
    #cat(paste0('idx1 : ',idx1), sep = '\n')
    idx2 <- (i-1)*8 + 24 + cumsum(c(0, 1, 1, 1))
    #cat(paste0('idx2 : ',idx2[1]), sep = '\n')
    data <- as.numeric(substr(rl, start = idx1, idx2[1]))/100
    data[data == -99.99] <- NA
    #dmflag <- substr(rl, start = idx2[2], idx2[2])
    qcflag <- substr(rl, start = idx2[3], idx2[3])
    #dsflag <- substr(rl, start = idx2[4], idx2[4])
    #headernames <- c(months[i], paste0(months[i], flagnames, 'flags'))
    mdf <- data.frame(year = as.numeric(year), month = rep(i, length(year)), temperature = as.numeric(data), flag = qcflag, stringsAsFactors = FALSE)
    alldata <- rbind(alldata, mdf)
  }
  alldata <- alldata[with(alldata, order(year,month)), ]
  #tiballdata <- as_tibble(alldata)
  #alldatajoin <- left_join(tiballdata, tibmeta, by = 'stationId')
  #df <- as.data.frame(alldatajoin)
  # if(!is.null(stationId)){
  #   okstn <- which(alldata$stationId == stationId)
  #   if(!length(okstn)){
  #     message('Station_id not found.')
  #   } else{
  #     df[okstn, ]
  #   }
  # } else {
  #   df
  # }
  list(stationId = stationId,
       stationName = stnname,
       province = NA,
       latitude = latitude,
       longitude = longitude,
       elevation = stnelev,
       updatedTo = NA,
       data = alldata)
}

#' @title Join GHCN station data
#'
#' @description This function will join the data of two sites that might need to be combined
#' due to temporal coverage.
#'
#' @param ghcn1 a list that was output in using `read.ghcn`, this is the primary station
#' @param ghcn2 a list that was output using `read.ghcn`, this is the secondary station
#' @param minYear a numeric value indicating the minimum year to subset the data, default is `NULL`
#' to retain the entire time series.
#' @param meanData a logical value indicating if (`TRUE`) the data should be averaged in the event there
#' are multiple values for a single year and month.
#'
#' @author Chantelle Layton
#'
#' @importFrom oce geodDist
#' @export
#'

join.ghcn <- function(ghcn1, ghcn2, minYear = NULL, meanData = TRUE){
  # just do a quick check that the stations provided are close together
  # here we've defined 25 km
  ok <- geodDist(longitude1 = as.numeric(ghcn1[['longitude']]),
                 latitude1 = as.numeric(ghcn1[['latitude']]),
                 longitude2 = as.numeric(ghcn2[['longitude']]),
                 latitude2 = as.numeric(ghcn2[['latitude']])) < 25

  if(ok){
    data1 <- ghcn1[['data']]
    data2 <- ghcn2[['data']]
    # since data1 is primary, from data2, we only want what is NOT in data1
    okdata2 <- apply(data2, 1, function(k) {!(k[['year']] %in% data1[['year']] & k[['month']] %in% data1[['month']])})
    data <- rbind(data1, data2[okdata2, ])
    data <- data[with(data, order(year, month)), ]
    if(!is.null(minYear)){
      data <- data[data$year > minYear, ]
    }
    if(meanData){
      yearmonth <- data.frame(year = data[['year']], month = data[['month']])
      uym <- unique(yearmonth)
      davg <- apply(uym, 1, function(k) mean(data[['temperature']][(data[['year']] == k[['year']] & data[['month']] == k[['month']])], na.rm = TRUE))
      data <- data.frame(uym, temperature = davg)
    }

    list(stationId = ghcn1[['stationId']],
         stationId2 = ghcn2[['stationId']],
         stationName = ghcn1[['stationName']],
         stationName2 = ghcn2[['stationName']],
         province = ghcn1[['province']],
         province2 = ghcn2[['province']],
         longitude = ghcn1[['longitude']],
         longitude2 = ghcn2[['longitude']],
         latitude = ghcn1[['latitude']],
         latitude2 = ghcn2[['latitude']],
         elevation = ghcn1[['elevation']],
         elevation2 = ghcn2[['elevation']],
         data = data)
  }

}
