#' @title Download temperature AHCCD data files
#'
#' @description This function downloads the two files associated with the Adjusted and Homogenized
#' Canadian Climate Data (AHCCD) mean temperature data.
#'
#' @details Download the AHCCD mean temperature data and its associated station excel file.
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

download.ahccd <- function(destdir = '.') {

  # source location of the data changed somewhere in first half of 2022
  # most likely due to various popular web browsers not supporting ftp sites anymore
  #ftp <- 'ftp://ccrp.tor.ec.gc.ca/pub/AHCCD/'
  site <- 'http://crd-data-donnees-rdc.ec.gc.ca/CDAS/products/AHCCD/'
  # define two files associated with temperature.
  #ftpFile <- 'Homog_monthly_mean_temp.zip'
  file <- 'Homog_monthly_mean_temp_Gen3.zip'
  #ftpStnFile <- 'Temperature_Stations.xls'
  stnFile <- 'Temperature_Stations_Gen3.xls'

  if(!dir.exists(destdir)){
    dir.create(destdir, recursive = TRUE)
  }

  download.file(url = paste0(site, file), destfile = paste(destdir, file, sep = '/'), mode = 'wb')
  download.file(url = paste0(site, stnFile), destfile = paste(destdir, stnFile, sep = '/'), mode = 'wb')
}


#' @title Read AHCCD station data file
#'
#' @description Read in the file which defines all stations included in the Adjusted and Homogenized
#' Canadian Climate Data (AHCCD).
#'
#' @details
#' Read Adjusted and Homogenized Canadian Climate Data station files. Excel station can be downloaded from
#' the ftp site at \url{ftp://ccrp.tor.ec.gc.ca/pub/AHCCD/} . It is normally downloaded
#' along with the associated data file. For example, if the 'Precipitation_Stations.xls' file
#' is downloaded, then one of the associated precipitation files should also be downloaded, in this example case
#' 'Adj_monthly_rain.zip' could be downloaded.
#'
#' @param file xls file containing the station data
#' @importFrom readxl read_xls
#' @author Chantelle Layton
#' @export

read.ahccd.stations <- function(file){
  # should put some checks here

  # Read the Excel spreadsheet of station information
  # can't use readLines to obtain headers due encoding of some characters
  # and keep getting incomplete final line [as of 20190724]
  cnametib <- read_xls(path = file, skip = 2)
  cnames <- names(cnametib)
  columnNames <- mapAhccdStationHeader(cnames)
  s <- read_xls(path = file, skip = 4, col_names = FALSE)
  # make it a data frame, read_xls makes it a tibble, ew.
  stnInfo <- as.data.frame(s)
  names(stnInfo) <- columnNames
  stnInfo
}

#' @title Map AHCCD station column names
#'
#' @description In efforts to attempt to infer column names and put them in a more useable form
#' for useage.
#'
#' @param x a vector of character strings
#'
#' @return a vector of character strings
#'
#' @author Chantelle Layton
#' @export
#'
mapAhccdStationHeader <- function(x){
  x <- gsub('No', 'no', x)
  x <- gsub('StnId', 'stnId', x)
  x <- gsub('Station name', 'stationName', x)
  x <- gsub('Prov', 'province', x)
  x <- gsub('From', 'startYear', x)
  x <- gsub('To', 'endYear', x)
  x <- gsub('%Miss', 'percentMissing', x)
  x <- gsub('Lat\\(deg\\)', 'latitude', x)
  x <- gsub('Long\\(deg\\)', 'longitude', x)
  x <- gsub('Elev\\(m\\)', 'elevation', x)
  x <- gsub('Joined', 'joined', x)
  x
}

#' @title Read AHCCD data
#'
#' @description
#' Read Adjusted and Homogenized Canadian Climate Data files. Data can be downloaded from
#' the ftp site at ftp://ccrp.tor.ec.gc.ca/pub/AHCCD/ . It is required that the data file be downloaded
#' along with the associated stations excel file. For example, if the 'Adj_monthly_rain.zip' file
#' is downloaded, then the 'Precipitation_Stations.xls' file should also be downloaded whether it
#' be using download.file(), or manually from the site.
#'
#' Flag description from 'Temperature_Documentation.doc', which is on the ftp site, last downloaded : 20190723,
#' M = missing value, E = estimated during archiving process, a = value has been adjusted due to homogeneity
#' and no flag indicates original unaltered value.
#'
#' @param file 	a connection or a character string giving the name of the file to load.
#' @param longitude optional numerical value containing longitude in decimal degrees,
#' positive in the eastern hemisphere.
#' @param latitude optional numerical value containing the latitude in decimal degrees,
#' positive in the northern hemisphere.
#' @param elevation optional numerical value containing the elevation of the station in meters.
#'
#' @importFrom utils read.table
#'
#' @return named list of data for each station provided in stns
#' @export

read.ahccd <- function(file, longitude = NULL, latitude = NULL, elevation = NULL){
  lines <- readLines(file, encoding = 'UTF-8')
  headerEng <- grep("Year*", lines)
  headerFre <- grep("Annee*", lines) # this is the last header line
  mm <- read.table(file, sep = ',', skip = headerFre, stringsAsFactors = FALSE)
  # remove NA data
  mm[mm==-9999.9] <- NA
  heading <- gsub("\\s", '', strsplit(lines[headerEng], split = ',')[[1]])
  # add an extra heading if the length doesn't match the number of columns in mm
  if(length(heading) != length(mm)){
    heading <- c(heading, "")
  }
  # need to add heading for flag columns
  # get headers that aren't Year
  okheaders <- heading != 'Year' & heading != ""
  headersFlag <- paste0(heading[okheaders], 'Flag')
  okflags <- heading == ""
  heading[okflags] <- headersFlag
  colnames(mm) <- heading
  # pull data apart and put it into a data.frame
  okcols <- names(mm) %in% month.abb
  okflag <- names(mm) %in% paste0(month.abb, 'Flag')
  data <- as.vector(t(mm[,okcols]))
  flag <- as.vector(t(mm[,okflag]))
  year <- unlist(lapply(mm[,names(mm) %in% 'Year'], rep, times = 12))
  month <- rep(1:12, times = dim(mm)[1])
  df <- data.frame(year = as.numeric(year),
                   month = as.numeric(month),
                   temperature = as.numeric(data), # wondering what to name this, obviously this fn could be used to read other ahccd data...
                   flag = flag,
                   stringsAsFactors = FALSE)
  metaEng <- trimws(strsplit(lines[grep('^\\d+,\\w+\\s+,\\w+\\s+, station( not)? joined', lines)],
                             split = ',')[[1]],
                    which = 'both')
  list(stationId = metaEng[1],
       stationName = gsub('_', ' ', metaEng[2]),
       province = metaEng[3],
       latitude = as.numeric(latitude),
       longitude = as.numeric(longitude),
       elevation = as.numeric(elevation),
       updatedTo = metaEng[7],
       data = df)
}

#' @title Find missing air temperature data
#'
#' @description This function identifies missing monthly data from the last date to
#' the present date
#'
#' @param ahccd a list containing data that is read in using `read.ahcdd`
#'
#' @return a data.frame of year, month, province, stationId, stationName
#'
#' @importFrom utils tail
#'
#' @export
#'

identifyMissingAhccd <- function(ahccd){
  lastDataTime <- as.POSIXct(paste(tail(ahccd[['data']][['year']],1),
                                   tail(ahccd[['data']][['month']],1),
                                   '01',
                                   sep = '-'),
                             format = '%Y-%m-%d', tz = 'UTC') # note that the day doesn't really matter
  currentTime <- checkCurrentTimeForClimateSummary(as.POSIXlt(presentTime()))
  missingTime <- as.POSIXlt(seq(lastDataTime, currentTime, by = 'month')[-1]) # omit the first one since it's the last data time
  n <- length(missingTime)
  data.frame(year = missingTime$year + 1900,
             month = missingTime$mon + 1,
             province = rep(ahccd[['province']], n),
             stationId = rep(ahccd[['stationId']], n),
             stationName = rep(ahccd[['stationName']], n))

}

#' Check current time
#'
#' @description This function checks a provided time, and returns the most recent date
#' available for monthly climate data.
#'
#' @param x a POSIXlt formatted time
#'
#' @return a POSIXct formatted time
#'
#' @author Chantelle Layton
#'
#' @export


checkCurrentTimeForClimateSummary <- function(x) {
  year <- x$year + 1900
  month <- x$mon + 1 # so 1 = jan
  {
    if (month == 1) {
      year <- year - 1
      if (x$mday < 5) {
        month <- 11
      } else {
        month <- 12
      }
    } else if (month == 2) {
      if (x$mday < 5) {
        month <- 12
        year <- year - 1
      } else {
        month <- month - 1
      }
    } else {
      if (x$mday < 5) {
        month <- month - 2
      } else {
        month <- month - 1
      }
    }
  }
  as.POSIXct(paste(year, month, '01', sep = '-'), format = '%Y-%m-%d', tz = 'UTC')
}
