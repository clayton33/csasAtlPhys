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
#' @param overwrite Logical value indicating if data file(s) should be re-downloaded, or overwritten,
#' if they already exist in destdir. Default is set to `FALSE`.
#'
#' @author Chantelle Layton
#' @importFrom utils download.file
#' @importFrom RCurl getURL
#' @export
#'

download.nefsc <- function(year, ship, destdir = '.', overwrite = FALSE) {
  if(missing(year) & missing(ship)){
    stop("Please provide either a year or ship.")
  }
  # check if files are already in destdir
  if(!overwrite){ # chances are user has already downloaded data
    existingFiles <- list.files(path = destdir, pattern = '.*\\.dat')
    if(length(existingFiles) == 0) {
      message(paste('No existing files in', destdir))
    }
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
  message(paste('Found', length(downloadfiles), 'files to download.'))
  # if overwrite = FALSE, check to see which ones have already been downloaded
  if(!overwrite & length(existingFiles) != 0){
    okdownload <- downloadfiles %in% existingFiles
    downloadfiles <- downloadfiles[!okdownload]
    message(paste(length(which(okdownload == TRUE)), 'have already been downloaded.'))
  }

  if(!dir.exists(destdir)){
    dir.create(destdir, recursive = TRUE)
  }

  if(length(downloadfiles) != 0){
    for(file in downloadfiles){
      download.file(url = paste0(ftp, file),
                    destfile = paste(destdir, file, sep = '/'))
    }
  } else {
    message(paste('No files found for', year, '.'))
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
#' @return a list of `ctd` objects.
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

#' @title Download NODC NEFSC data from erddap server
#'
#' @description Download data hosted by the National
#' Oceanic and Atmospheric Administration (NOAA) Northeast Fisheries
#' Science Center (NEFSC).
#'
#' @details This function downloads data hosted by the National
#' Oceanic and Atmospheric Administration (NOAA) Northeast Fisheries
#' Science Center (NEFSC) from the ERDDAP server. Saves a `.nc` file in
#' supplied `destdir`.
#'
#' @param startDate A character string in the format `yyyy-mm-dd` identifying the start of the desired
#' timeseries beginning at 00:00UTC
#' @param endDate A character string in the format `yyyy-mm-dd` identifying the end of the desired
#' timeseries ending at 23:59UTC
#' @param destdir Optional string indicating the directory in which to store
#' downloaded files. If not supplied, `"."` is used, i.e. the data file
#' is stored in the present working directory. Also, the directory will be created
#' if not already done so.
#'
#' @return a character string of the filename.
#' @author Chantelle Layton
#' @export
#'

download.nefsc.erddap <- function(startDate, endDate, destdir = ".") {
  # create output directory if not already done so
  if(!dir.exists(destdir)){
    dir.create(destdir, recursive = TRUE)
  }
  # define the url
  baseurl <- "https://comet.nefsc.noaa.gov/"
  url <- paste0(baseurl, "erddap/tabledap/",
                "ocdbs_v_erddap1.nc",
                "?UTC_DATETIME",
                "%2Clatitude",
                "%2Clongitude",
                "%2Cdepth",
                "%2Cpressure_dbars",
                "%2Csea_water_temperature",
                "%2Csea_water_salinity",
                "%2Cdissolved_oxygen",
                "%2Cfluorescence",
                "%2Cpar_sensor",
                "%2Ccast_number",
                "%2Ccruise_id",
                "%2Cpurpose_code",
                "%2Cbottom_depth",
                "%2CGEAR_TYPE",
                "&UTC_DATETIME%3E=", startDate, #"T00%3A00%3A00Z",
                "&UTC_DATETIME%3C=", endDate#, "T23%3A59%3A59Z"
                )
  # download data
  start <- gsub('-','', startDate)
  end <- gsub('-','', endDate)
  destfile <- paste0(paste('nefscProfileData', start, end, sep = '_'),'.nc')
  destFilename <- paste(destdir, destfile, sep = '/')
  download.file(url = url,
                destfile = destFilename,
                mode = 'wb')
  destFilename
}

#' @title Read NODC NEFSC Profile data
#'
#' @description This function reads in NODC NEFSC Profile data that was downloaded using [download.nefsc.erddap].
#'
#' @param file a connection or a character string giving the name of the file to read.
#'
#' @return A list of `ctd` objects.
#'
#' @author Chantelle Layton
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 ncvar_get
#' @importFrom ncdf4 ncatt_get
#' @importFrom oce as.ctd
#'
#' @export

read.nefsc.erddap.nc <- function(file){
  nc <- nc_open(filename = file)
  # to see which variables are in the file do names(nc$var)
  # no qc flags in data
  # but if they get implemented, have a look at read.neracoos.buoy

  # I don't think there's anything useful here
  globalAtts <- ncatt_get(nc, 0)

  # get data
  # note that I could have just converted 'time' to number of seconds
  # and added it to 'date', but I don't trust that 'date' is actually
  # UTC, so I'm going to convert them separately, then join them together
  date <- ncvar_get(nc = nc, varid = "UTC_DATETIME")
  # # convert to POSIXct
  # datestrf <- strftime(as.POSIXct(date, origin = '1970-01-01', tz = 'UTC'), format = '%Y-%m-%d')
  # # I'm not sure that the date is actually "00:00:00"Z, it's giving me "04:00:00"Z
  # # that's why I strftime'd it
  # time <- ncvar_get(nc = nc, varid = 'UTC_DECIMAL_HOUR')
  # # convert to POSIXct
  # timestrf <- strftime(as.POSIXct(x = time * 60 * 60, # covert to number of seconds
  #                                 origin = '1970-01-01', # could use anything
  #                                 tz = 'UTC'),
  #                      format = '%H:%M:%S')
  #dateTime <- as.POSIXct(x = paste(datestrf, timestrf), origin = '1970-01-01', tz = 'UTC')
  dateTime <- as.POSIXct(x = date, origin = '1970-01-01', tz = 'UTC')
  # get data
  # [1] "UTC_DATE"              "UTC_DECIMAL_HOUR"      "latitude"              "longitude"             "depth"                 "pressure_dbars"
  # [7] "sea_water_temperature" "sea_water_salinity"    "dissolved_oxygen"      "fluorescence"          "par_sensor"            "cast_number"
  # [13] "cruise_id"             "purpose_code"          "bottom_depth"          "GEAR_TYPE"
  # 20241113
  # [1] "UTC_DATETIME"          "latitude"              "longitude"             "depth"
  # [5] "pressure_dbars"        "sea_water_temperature" "sea_water_salinity"    "dissolved_oxygen"
  # [9] "fluorescence"          "par_sensor"            "cast_number"           "cruise_id"
  # [13] "purpose_code"          "bottom_depth"          "GEAR_TYPE"
  lat <- ncvar_get(nc = nc, varid = 'latitude')
  lon <- ncvar_get(nc = nc, varid = 'longitude')
  p <- ncvar_get(nc = nc, varid = 'pressure_dbars')
  T <- ncvar_get(nc = nc, varid = 'sea_water_temperature')
  S <- ncvar_get(nc = nc, varid = 'sea_water_salinity')
  # get metadata items
  castNum <- ncvar_get(nc = nc, varid = 'cast_number')
  cruiseId <- ncvar_get(nc = nc, varid = 'cruise_id')
  purposeCode <- ncvar_get(nc = nc, varid = 'purpose_code')
  sounding <- ncvar_get(nc = nc, varid = 'bottom_depth')
  gearType <- ncvar_get(nc = nc, varid = 'GEAR_TYPE')

  # create a data.frame so the data can be split efficiently
  # (not sure what happens if there's a lot of data for one year)
  df <- data.frame(cruiseId = cruiseId, # metadata
                   castNumber = castNum,
                   purposeCode = purposeCode,
                   gearType = gearType,
                   sounding = sounding,
                   time = dateTime,
                   latitude = lat, # data
                   longitude = lon,
                   pressure = p,
                   temperature = T,
                   salinity = S)
  # split df by cruiseId
  dfs <- split(df, df[['cruiseId']])
  # split dfs by castNumber
  dfss <- lapply(dfs, function(k) split(x = k, f = k[['castNumber']]))
  # split dfss by gearType
  dfsss <- lapply(dfss, function(kk) lapply(kk, function(k) split(x = k, f = k[['gearType']])))
  # now create ctd objects
  profilecnt <- 1
  ctd <- vector(mode = 'list', length = sum(unlist(lapply(dfsss, # cruiseID
                                                          function(kk) lapply(kk, length)))))# castNumber
  #function(k) lapply(k, # gearType
  #                   length))))))
  for(ic in 1:length(dfsss)){ # iterates through length from split by cruiseNumber
    ddd <- dfsss[[ic]]
    for(ip in 1:length(ddd)){ # iterates through length from split by castNumber
      dd <- ddd[[ip]]
      for(ig in 1:length(dd)){
        d <- dd[[ig]]
        ctd[[profilecnt]] <- as.ctd(salinity = d[['salinity']],
                                    temperature = d[['temperature']],
                                    pressure = d[['pressure']],
                                    time = d[['time']],
                                    startTime = d[['time']][1],
                                    longitude = d[['longitude']],
                                    latitude = d[['latitude']],
                                    station = d[['castNumber']][1],
                                    cruise = d[['cruiseId']][1],
                                    type = d[['gearType']][1])
        profilecnt <- profilecnt + 1
      }

    }
  }
  ctd
}
