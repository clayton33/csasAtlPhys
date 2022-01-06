#' @title Download NODC NEFSC data
#'
#' @description Download data hosted by the National
#' Oceanic and Atmospheric Administration (NOAA) Northeast Fisheries
#' Science Center (NEFSC).
#'
#' @details This function downloads data hosted by the National
#' Oceanic and Atmospheric Administration (NOAA) Northeast Fisheries
#' Science Center (NEFSC) via an ftp connection. Users have the option of supplying
#' either the year and or ship, but one must be supplied.
#'
#' @section Valid ship abbreviations with full name:
#' \tabular{ll}{
#' \strong{abbreviation} \tab \strong{full name} \cr
#' aj   \tab \cr
#' alb  \tab \cr
#' am   \tab \cr
#' arm  \tab RV Neil Armstrong \cr
#' c1   \tab \cr
#' ch   \tab \cr
#' del  \tab FSV Delaware II \cr
#' edl  \tab \cr
#' eg   \tab \cr
#' egg  \tab \cr
#' en   \tab E/V Endeavor \cr
#' end  \tab \cr
#' ex   \tab Okeanos Explorer \cr
#' ey   \tab Eagle Eye II \cr
#' gu   \tab Gordon Gunter \cr
#' hb   \tab FSV Henry B. Bigelow \cr
#' is   \tab \cr
#' kat  \tab \cr
#' nob  \tab \cr
#' np   \tab \cr
#' oce  \tab \cr
#' ore  \tab \cr
#' pc   \tab FSV Pisces \cr
#' pe   \tab \cr
#' rel  \tab \cr
#' s1   \tab R/V Hugh R. Sharp \cr
#' sj   \tab \cr
#' unk  \tab
#' }
#'
#' @param year Optional number giving the year of interest
#' @param ship Optional string indicating the ship of interest
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

download.nefsc <- function(year, ship, destdir = '.') {
  if(missing(year) & missing(ship)){
    stop("Please provide either a year or ship.")
  }
  # define ftp site
  ftp <- 'ftp://ftp.nefsc.noaa.gov/pub/hydro/nodc_files/'
  # obtain file names
  files <- getURL(url = ftp,
                  ftp.use.epsv = FALSE, dirlistonly = TRUE)
  filenames <- strsplit(files, '\r*\n')[[1]]
  # pull out some information from file names
  ships <- gsub(pattern = '^(\\w+)[0-9]{4}\\.dat$', '\\1', filenames)
  bogusFiles <- nchar(ships) > 3 # might be some strange random files
  filenames <- filenames[!bogusFiles]
  ships <- ships[!bogusFiles]
  years <- gsub(pattern = '^\\w+([0-9]{2})[0-9]{2}\\.dat$', '\\1', filenames)
  cruiseNumber <- gsub(pattern = '^\\w+[0-9]{2}([0-9]{2})\\.dat$', '\\1', filenames)
  fullYear <- as.numeric(unlist(lapply(years, function(k) ifelse(k < 50, paste0('20',k), paste0('19',k)))))

  if(!missing(year) & missing(ship)){ # only year provided
    okfiles <- fullYear == year
  } else if (missing(year) & !missing(ship)){ # only ship provided
    okfiles <- ships == ship
  } else { # provide both year and ship
    okfiles <- fullYear == year & ships == ship
  }

  downloadfiles <- filenames[okfiles]

  if(!dir.exists(destdir)){
    dir.create(destdir, recursive = TRUE)
  }

  if(length(downloadfiles) != 0){
    for(file in downloadfiles){
      download.file(url = paste0(ftp, file),
                    destfile = paste(destdir, file, sep = '/'))
    }
  } else {
    message(paste0('No files found for', year, '.'))
  }

}

#' @title Read NEFSC hydrographic data
#'
#' @description Read in Northeast Fisheries
#' Science Center (NEFSC) hydrographic
#' data that can be downloaded using [download.nefsc()].
#'
#' @param file a connection or a character string giving the name of the file to read.
#'
#' @return a list of ctd objects
#'
#' @importFrom oce as.ctd
#' @export

read.nefsc <- function(file){
  rl <- readLines(file)
  # find header lines
  # these are indicated by a 1 in the 80th character of the string
  flag <- as.numeric(unlist(lapply(rl, substr, 80, 80)))
  header <- which(flag == 1)
  ctd <- vector(mode = 'list', length = length(header))
  for(i in 1:length(header)){
    hl <- header[i]
    # parse header line
    countryCode <- substr(rl[hl], 1, 2) # t1, i2
    ship <- substr(rl[hl], 3, 3 + 1) # t3, A2
    latitudeUnformat <- substr(rl[hl], 5, 5 + 4) # t5, i5
    longitudeUnformat <- substr(rl[hl], 10, 10 + 5) # t10, i6
    year <- substr(rl[hl], 17, 17 + 3) # t17, i4
    month <- substr(rl[hl], 21, 21 + 1) # t21, i2
    day <- substr(rl[hl], 23, 23 + 1) # t23, i2
    timeUnformat <- substr(rl[hl], 25, 25 + 3) # t25, i4
    cruiseCode <- substr(rl[hl], 29, 29 + 1) # t29, i2
    castNumber <- substr(rl[hl], 31, 31 + 2) # t21, i3
    bottomDepth <- substr(rl[hl], 34, 34 + 3) # t34, i4
    deploymentMethod <- substr(rl[hl], 39, 39) # t39, i1
    ctdSerialNumber <- substr(rl[hl], 42, 42 + 3) # t42, i4
    stationNumber <- substr(rl[hl], 63, 63 + 9) # t63, i10
    # get data
    {if(i == length(header)){
      dl <- (hl + 1):(length(rl))
    } else {
      dl <- (hl + 1):(header[i+1] - 1)
    }}
    p <- T <- S <- sigT <- oxy <- chl <- fluor <- par <- vector(length = length(dl))
    for(j in 1:length(dl)){
      line <- dl[j]
      dataline <- rl[line]
      p[j] <- substr(dataline, 28, 28 + 3) # t28, i4
      T[j] <- substr(dataline, 33, 33 + 3) # t33, i4
      S[j] <- substr(dataline, 38, 38 + 4) # t38, i5
      sigT[j] <- substr(dataline, 43, 43 + 3) # t43, i4
      oxy[j] <- substr(dataline, 51, 51 + 2) # t51, i3
      chl[j] <- substr(dataline, 60, 60 + 3) # t60, i4
      # fluor[j] <- substr(dataline, 60, 60 + 3) #t60, i4 #
      par[j] <- substr(dataline, 65, 65 + 3)
    }

    # format the metadata and data
    latitude <- as.numeric(substr(latitudeUnformat, 1,2)) +
      as.numeric(substr(latitudeUnformat, 3,4))/60 +
      as.numeric(paste0('.', substr(latitudeUnformat, 5,5))) * (60/3600)
    longitude <- -1* (as.numeric(substr(longitudeUnformat, 1,3)) +
                        as.numeric(substr(longitudeUnformat, 4,5))/60 +
                        as.numeric(paste0('.',substr(longitudeUnformat, 6, 6))) * (60 / 3600))
    time <- paste(substr(timeUnformat, 1, 2), substr(timeUnformat, 3, 4), sep = ":")
    # header in files before year 2000 appear to have '19' missing from year
    startTime <- as.POSIXct(paste(paste(ifelse(nchar(as.numeric(year)) == 2, as.numeric(year) + 1900, as.numeric(year)),
                                        as.numeric(month),
                                        as.numeric(day), sep = '-'),
                                  time, sep = " "), tz = 'UTC')
    temperature <- as.numeric(paste(substr(T, 1, 2), substr(T, 3, 4), sep ='.'))
    temperature[temperature == 99.999] <- NA
    salinity <- as.numeric(paste(substr(S, 1, 2), substr(S, 3, 5), sep = '.'))
    salinity[salinity == 99.999] <- NA
    sigmaTheta <- as.numeric(paste(substr(sigT, 1, 2), substr(sigT, 3, 4), sep= '.'))
    sigmaTheta[salinity == 99.999 | temperature == 99.999] <- NA
    # likley not to have any oxy, chl, fluor, and par
    # implement other logic
    ## 20190926 : not concerned with these other variable so don't use them
    ##            for now. Below is some example code if needed in the future
    # oxygen <- as.numeric(unlist(lapply(oxy, function(k) ifelse(grepl('^\\s*$', k),
    #                                                            NA,
    #                                                            paste(substr(k, 1, 2), substr(k, 3,3), sep = ".")))))
    # chlorophyll <- as.numeric(unlist(lapply(chl, function(k) ifelse(grepl('^\\s*$', k),
    #                                                                 NA,
    #                                                                 paste(substr(k, 1, 2), substr(k, 3, 4), sep = ".")))))

    # construct a cruiseNumber that looks like ours
    cruiseNumber <- paste0(ship, ifelse(nchar(as.numeric(year)) == 2, as.numeric(year) + 1900, as.numeric(year)), paste0('0', cruiseCode))
    ctdtemp <- as.ctd(salinity = salinity,
                       temperature = temperature,
                       pressure = as.numeric(p),
                       serialNumber = as.numeric(ctdSerialNumber),
                       ship = ship,
                       cruise = cruiseCode,
                       station = castNumber,
                       longitude = longitude,
                       latitude = latitude,
                       time = as.POSIXct(rep(startTime, length(temperature)), origin = '1970-01-01', tz = 'UTC'),
                       startTime = startTime)
    ctdtemp <- oceSetMetadata(ctdtemp,
                              'filename',
                              file)
    ctdtemp <- oceSetMetadata(ctdtemp,
                              'cruiseNumber',
                              cruiseNumber)
    ctd[[i]] <- ctdtemp

  }
  ctd
}
