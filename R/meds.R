#' @title Download MEDS data
#'
#' @description Download data hosted by Fisheries and Oceans Canada
#' (DFO) Marine Environmental Data Section (MEDS).
#'
#' @details This function downloads data hosted by Fisheries and Oceans Canada
#' (DFO) Marine Environmental Data Section (MEDS) via an ftp connection.
#' Users have the option of supplying the year and or month. Note that the year
#' and month specified does not necessarily mean that the data contained in
#' those files was aquired during the year and month in question. The data file
#' will contain all the data received by MEDS during that year and month. For each
#' requested year and or month will download two files, a `.hdr`and a `.prf`
#' file. Both are required when reading the data as the `.hdr` file contains
#' some basic metadata, and `.prf` contains all of the data.
#'
#' @param year Number giving the year of interest
#' @param month Optional number indicating the month
#' @param destdir Optional string indicating the directory in which to store
#' downloaded files. If not supplied, `"."` is used, i.e. the data file
#' is stored in the present working directory. Also, if the directory will be created
#' if not already done so.
#'
#' @author Chantelle Layton
#' @importFrom utils download.file
#' @importFrom RCurl getURL
#' @export
#'
download.meds <- function(year, month, destdir = '.') {
  if(missing(year)){
    stop('At a minimum, must supply a year.')
  }
  # define ftp site
  ftp <- 'ftp://www.meds-sdmm.dfo-mpo.gc.ca/pub/bio/'
  # get filenames
  files <- getURL(url = ftp,
                  ftp.use.epsv = FALSE, dirlistonly = TRUE)
  filenames <- strsplit(files, '\r*\n')[[1]]
  # there are various files in here, we only want to look at
  #   those with a .hdr or .prf extension
  filenames <- filenames[grepl(pattern = '.*\\.(prf)?(hdr)?$',filenames)]

  years <- as.numeric(substr(filenames, 1, 4))
  months <- as.numeric(substr(filenames, 5, 6))

  if(missing(month)){ # only year provided
    okfiles <- years == year
  } else { # provide both year and ship
    okfiles <- years == year & months == month
  }
  downloadfiles <- filenames[okfiles]

  if(!dir.exists(destdir)){
    dir.create(destdir, recursive = TRUE)
  }

  #existingFiles <- list.files(savedir)
  #filesToDownload <- filesOnServer[!(filesOnServer %in% existingFiles)]

  if(length(downloadfiles) != 0){
    for(file in downloadfiles){
      download.file(url = paste0(ftp, file),
                    destfile = paste(destdir, file, sep = '/'))
    }
  } else {
    message(paste0('No files found for', year, ifelse(!missing(month), paste0('and month', month), ''),'.'))
  }
}


#' @title Read MEDS data
#'
#' @description Read in data that was downloaded, which can be done by [download.meds()],
#' from Fisheries and Oceans Canada (DFO) Marine Environmental Data Section (MEDS). The function
#' requires that the `.hdr` and `.prf` files, which are associated with the same year and month
#' are supplied.
#'
#' @param hdrfile a connection or a character string indicating the header , `.hdr``, file to read
#' @param prffile a connection or a character string indicating the data, `.prf``, file to read
#' @param exclusionLon a supplied vector of longitude coordinates to ignore, default is set to NULL,
#' meaning keep all data.
#' @param exclusionLat a supplied vector of latitude coordinates to ignore, default is set to NULL,
#' meaning keep all data.
#'
#' @return a list of ctd objects
#'
#' @importFrom sp point.in.polygon
#' @importFrom dplyr as_tibble
#' @importFrom dplyr left_join
#' @importFrom oce as.ctd
#' @importFrom oce oceSetMetadata
#'
#'

read.meds <- function(hdrfile, prffile, exclusionLon = NULL, exclusionLat = NULL){
  hdrrl <- readLines(hdrfile)
  cruiseNumber <- gsub(pattern = '^(\\w+)\\s+(\\w+)$', '\\1\\2', substr(hdrrl, start = 1, stop = 10))
  hdrrlsplt <- trimws(unlist(lapply(hdrrl, function(k) substr(k, start = 11, stop = nchar(k)))), which = 'both')
  prfrl <- trimws(readLines(prffile), which = 'both')

  hdrsplt <- strsplit(hdrrlsplt, '\\s+')
  prfsplt <- strsplit(prfrl, '\\s+')

  hdrdf <- data.frame(cruiseNumber = cruiseNumber,
                      stationNumber = as.numeric(unlist(lapply(hdrsplt, function(k) k[1]))),
                      ninput = as.numeric(unlist(lapply(hdrsplt, function(k) k[2]))),
                      dataType = as.character(unlist(lapply(hdrsplt, function(k) k[3]))),
                      latitude = as.numeric(unlist(lapply(hdrsplt, function(k) k[4]))),
                      longitude = as.numeric(unlist(lapply(hdrsplt, function(k) k[5]))) * -1,
                      qcpos = as.numeric(unlist(lapply(hdrsplt, function(k) k[6]))),
                      year = as.numeric(unlist(lapply(hdrsplt, function(k) k[7]))),
                      month = as.numeric(unlist(lapply(hdrsplt, function(k) k[8]))),
                      day = as.numeric(unlist(lapply(hdrsplt, function(k) k[9]))),
                      time = as.numeric(unlist(lapply(hdrsplt, function(k) k[10]))),
                      qcdatetime = as.numeric(unlist(lapply(hdrsplt, function(k) k[11]))),
                      maxDepth = as.numeric(unlist(lapply(hdrsplt, function(k) k[12]))),
                      nrec = as.numeric(unlist(lapply(hdrsplt, function(k) k[13]))),
                      nqcfail = as.numeric(unlist(lapply(hdrsplt, function(k) k[14]))),
                      stringsAsFactors = FALSE)
  if(!is.null(exclusionLon) & !is.null(exclusionLat)){
    good <- point.in.polygon(point.x = hdrdf$longitude, point.y = hdrdf$latitude,
                             pol.x = exclusionLon, pol.y = exclusionLat) == 0
    hdrdf <- hdrdf[good, ]
  }

  prfdf <- data.frame(ninput = as.numeric(unlist(lapply(prfsplt, function(k) k[1]))),
                      depth = as.numeric(unlist(lapply(prfsplt, function(k) k[2]))),
                      temperature = as.numeric(unlist(lapply(prfsplt, function(k) k[3]))),
                      salinity = as.numeric(unlist(lapply(prfsplt, function(k) k[4]))),
                      qctemperature = as.numeric(unlist(lapply(prfsplt, function(k) k[5]))),
                      qcsalinity = as.numeric(unlist(lapply(prfsplt, function(k) k[6]))),
                      stringsAsFactors = FALSE)

  hdrtb <- as_tibble(hdrdf)
  prftb <- as_tibble(prfdf)

  alldata <- left_join(prftb, hdrtb, by = 'ninput')

  hhmm <- unlist(lapply(alldata$time, function(k) ifelse(nchar(k) == 1, paste0('000',k),
                                                         ifelse(nchar(k) == 2, paste0('00', k),
                                                                ifelse(nchar(k) == 3, paste0('0', k), k)))))
  time <- as.POSIXct(paste(paste(alldata$year, alldata$month, alldata$day, sep = '-'),
                           paste(substr(hhmm, start = 1, stop = 2),
                                 substr(hhmm, start = 3, stop = 4),sep = ':'), sep = ' '),
                     format = '%Y-%m-%d %H:%M', tz = 'UTC')
  natemp <- alldata$temperature == -99.00
  nasal <- alldata$salinity == -99.00
  alldata$temperature[natemp] <- NA
  alldata$salinity[nasal] <- NA
  baddata <- alldata$qctemperature > 1 | alldata$qcsalinity > 1 | alldata$qcpos > 1 | alldata$qcdatetime > 1
  data <- alldata[!baddata, ]
  time <- time[!baddata]
  uninput <- unique(data$ninput)
  ctd <- vector(mode = 'list')
  cnt <- 1
  for(idx in 1:length(uninput)){
    ok <- which(data$ninput == uninput[idx])
    if(length(ok) > 3){
      dd <- as.ctd(salinity = data$salinity[ok],
                   temperature = data$temperature[ok],
                   pressure = data$depth[ok],
                   cruise = data$cruiseNumber[ok][1],
                   longitude = data$longitude[ok],
                   latitude = data$latitude[ok],
                   time = time[ok],
                   startTime = time[ok][1])
      dd <- oceSetMetadata(dd, name = 'dataType', value = data$dataType[ok][1])
      ctd[[cnt]] <- dd
      cnt <- cnt + 1
    }
  }
  ctd
}
