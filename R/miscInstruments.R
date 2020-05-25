#' @title Read Vemco minilog file
#'
#' @description This function reads in a Vemco minilog file.
#'
#' @param file a connection or character string  of the file to read.
#' @param skipStart an optional numeric value indicating number of hours to skip at the beginning of the file,
#' default is NA, meaning keep all the data points.
#' @param skipEnd an optional numeric value indicating number of hours to skip at the beginning of the file,
#' default is NA, meaning keep all the data points.
#' @param skip an optional integer value indicating the number of lines of the data file to skip before
#' beginning to read the data, default is 8.
#' @param pressure a numerical value of the pressure that the instrument was moored
#'
#' @details Reads a Vemco minilog file assuming just date, time and temperature fields. At this time
#' it is not clear is this a standard formatted file. Saves no information
#' from the header, not the even column names. It assumes that only temperature is present. If for some reason
#' a conductivity or pressure sensor is added, the code for this function will need to be updated accordingly.
#'
#' @author Chantelle Layton
#'
#' @return a ctd object with `deploymentType = "moored"`.
#'
#' @importFrom oce as.ctd
#'

read.minilog <- function(file, skipStart = NA, skipEnd = NA, skip = 8, pressure) {
  csvdata <- read.table(file,
                        header=FALSE, skip = skip, sep=",",
                        col.names = c("DATE","TIME","TEMPERATURE"),stringsAsFactors = FALSE)
  sst <- as.data.frame(DATE = as.POSIXct(paste(csvdata$DATE,csvdata$TIME), tryFormats=c("%d/%m/%Y %H:%M:%S",
                                                                         "%Y-%m-%d %H:%M:%S"),
                          tz="UTC",origin="1970-01-01"),
                       TEMPERATURE = csvdata$TEMPERATURE)
  #colnames(sst) <- c("DATE", "TEMPERATURE")
  #sst <- as.data.frame(sst)
  #sst$DATE <- as.POSIXct(sst$DATE, tz="UTC", origin="1970-01-01")
  if(!is.na(skipStart)){
    start <- sst$DATE[1]
    startSkip <- start + skipStart*3600 # multiply by seconds per hour
    sst <- subset(sst, DATE > startSkip)
  }
  if(!is.na(skipEnd)){
    end <- sst$DATE[length(sst$DATE)]
    endSkip <- end - skipEnd*3600 # multiply by seconds per hour
    sst <- subset(sst, DATE < endSkip)
  }
  names(sst) <- tolower(names(sst))

  as.ctd(pressure = rep(pressure, length(sst$date)),
         conductivity = rep(NA, length(sst$date)),
         temperature = sst$temperature, # in the event there are more than one row of data
         time = sst$date,
         deploymentType = 'moored')
}

#' @title Read non-archived St.Andrews moored temperature data
#'
#' @param file a connection or a character string giving the name of the file to load.
#' @param skip a numerical value indicating the number of rows to skip before reading in data
#' @param header logical value indicating whether or not a header is present in the file
#' @param pressure a numerical value of the pressure that the instrument was moored
#'
#' @details This function attempts to read in a non-archived file that is obtained
#' from a technician at the St.Andrews Biological Station in St.Andrews, New Brunswick.
#' This is in no way a standard formatted file, as they are recieved in various
#' formats year to year. Note that pressure must be indicated as the files recieved have
#' no pressure information.
#'
#' @return a ctd object with `deploymentType = "moored"`.
#'
#' @author Chantelle Layton
#'
#' @importFrom oce as.ctd
#' @importFrom utils read.csv
#'

read.stAndrewsTemperature <- function(file, skip = 0, header = TRUE, pressure){
  ss <- strsplit(x = file, split = '\\.')[[1]]
  ext <- ss[length(ss)]
  if(ext == 'xlsx'){
    message('Excel file formatting for this data is not ideal, save an alternate format.')
  } else if(ext == 'csv'){
    dd <- read.csv(file, header = header, stringsAsFactors = FALSE, skip = skip)
  } else if(ext == 'dat'){
    dd <- read.table(file, header = header)
  } else{
    message('File format not yet supported.')
  }

  ddnames <- tolower(names(dd))
  ddnames <- gsub('\\.', '', ddnames) # remove all '.' from names
  okdatetime <- grepl('date|time', ddnames) # switched back to date|time
  if(length(ddnames[okdatetime]) > 1){
    okdate <- grepl('date', ddnames)
    oktime <- grepl('time$', ddnames)
    # sometimes there is a column with both time and date due to someone manually modifying the
    # data. Omit these rows
    okdt <- oktime & okdate
    okdate[okdt] <- oktime[okdt] <- FALSE
    if(any(oktime)){
      date <- dd[,okdate]
      time <- dd[,oktime]
      datetime <- paste(date, time, sep = ' ')
    } else { # the case where there was only a Date.Time vector, and it's been split up in hour and minute
      okhour <- grepl('^hour', ddnames)
      okminute <- grepl('^minute', ddnames)
      datetime <- paste(dd[,okdate], paste(dd[,okhour], dd[,okminute], '00', sep = ':'), sep = ' ')
    }

  }else if (any(grepl('^month', ddnames)) & !any(okdatetime)){
    okyear <- grepl('^year', ddnames)
    okmonth <- grepl('^month', ddnames)
    okday <- grepl('^day$', ddnames)
    okhour <- grepl('^hour', ddnames)
    okminute <- grepl('^minute', ddnames)
    datetime <- paste(paste(dd[,okyear], dd[,okmonth], dd[,okday], sep = '/'),
                      paste(dd[,okhour], dd[,okminute], '00', sep = ':'),
                      sep = ' ')
  } else if (any(grepl('^hour', ddnames))) {
    okdate <- grepl('^date', ddnames)
    okhour <- grepl('^hour', ddnames)
    okminute <- grepl('^minute', ddnames)
    datetime <- paste(dd[,okdate], paste(dd[,okhour], dd[,okminute], '00', sep = ':'), sep = ' ')
  } else {
    datetime <- dd[,okdatetime]
  }
  emptyns <- datetime == "" # find empty indicies
  empty1s <- datetime == " "
  empty <- emptyns | empty1s
  dd <- dd[!empty,]
  datetime <- gsub('-','/', datetime) # some files have mixed '-' and '/' in the dates, so just use '/
  datetimect <- as.POSIXct(datetime[!empty], tryFormats = c('%m/%d/%y %H:%M',
                                                            '%m/%d/%Y %H:%M',
                                                            '%y/%m/%d %H:%M:%OS',
                                                            '%Y/%m/%d %H:%M:%OS',
                                                            '%d/%b/%y %H:%M:%OS'), tz = 'UTC')

  oktemp <- which(grepl('temperature|temp', ddnames))[1]
  as.ctd(pressure = rep(pressure, length(datetimect)),
         conductivity = rep(NA, length(datetimect)),
         temperature = dd[,oktemp], # in the event there are more than one row of data
         time = datetimect,
         deploymentType = 'moored')
}

#' @title Read sealog-T file
#'
#' @description This function reads in a sealog-T file.
#'
#' @param file a connection or character string  of the file to read.
#' @param skipStart an optional numeric value indicating number of hours to skip at the beginning of the file,
#' default is NA, meaning keep all the data points.
#' @param skipEnd an optional numeric value indicating number of hours to skip at the beginning of the file,
#' default is NA, meaning keep all the data points.
#' @param pressure a numerical value of the pressure that the instrument was moored
#'
#' @details Reads a sealog-T file, calculates the date based on the start, end, and sample period specified
#' in the header lines. Saves no information. It assumes that only temperature is present. This code
#' might be fragile, so use with caution.
#'
#' @author Chantelle Layton
#'
#' @importFrom oce as.ctd
#'
#' @return a ctd object with `deploymentType = "moored"`.
#'

read.sealogT <- function(file, skipStart = NA, skipEnd = NA, pressure){
  rl <- readLines(file)
  # the data starts after * Sample period
  hdrEnd <- grep('\\* Sample period', rl)
  endData <- grep('\\* Exit\\. Serial plug', rl)
  datal <- rl[(hdrEnd+1):(endData-1)]
  # get the dateTime
  st <- gsub('\\* Start time\\s+(\\w+)', '\\1', rl[grep('^\\* Start time', rl)])
  startTime <- as.POSIXct(st, format = '%y-%m-%d %H:%M:%OS', tz = 'UTC')
  et <- gsub('\\* End time\\s+(\\w+)', '\\1', rl[grep('^\\* End time', rl)])
  endTime <- as.POSIXct(et, format = '%y-%m-%d %H:%M:%OS', tz = 'UTC')
  sp <- gsub('\\* Sample period\\s+(\\w+)', '\\1', rl[hdrEnd])
  samplePeriod <- as.difftime(sp, format = '%H:%M:%OS')

  # get temperature
  temp <- as.numeric(gsub('\\s+(\\w+\\.\\w+)\\sDEGC\\s+((\\w+\\W)*\\w+)?', '\\1', datal))
  # for some reason the time vector is going to be longer than temperature
  dateTime <- seq(startTime, endTime, samplePeriod)[1:length(temp)]

  df <- data.frame(dateTime = dateTime, temperature = temp)

  if(!is.na(skipStart)){
    start <- df$dateTime[1]
    startSkip <- start + skipStart*3600 # multiply by seconds per hour
    df <- subset(df, dateTime > startSkip)
  }
  if(!is.na(skipEnd)){
    end <- df$dateTime[length(df$dateTime)]
    endSkip <- end - skipEnd*3600 # multiply by seconds per hour
    df <- subset(df, dateTime < endSkip)
  }

  as.ctd(pressure = rep(pressure, length(df$temperature)),
         conductivity = rep(NA, length(df$temperature)),
         temperature = df$temperature, # in the event there are more than one row of data
         time = df$dateTime,
         deploymentType = 'moored')
}
