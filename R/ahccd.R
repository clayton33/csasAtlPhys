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
  s <- read_xls(path = file, range = 'a5:k342',
                col_names = tolower(c("PROVINCE","STATION_NAME","STNID","START_YEAR","START_MONTH","END_YEAR","END_MONTH",
                              "LATITUDE","LONGITUDE","ELEVATION","JOINED")))
  # make it a data frame, read_xls makes it a tibble, ew.
  stnInfo <- as.data.frame(s)
  stnInfo
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
#' @importFrom utils read.table
#'
#' @return named list of data for each station provided in stns

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
  df <- data.frame(year = year,
                   month = month,
                   temperature = data, # wondering what to name this, obviously this fn could be used to read other ahccd data...
                   flag = flag)
  metaEng <- trimws(strsplit(lines[grep('^\\d+,(\\w+\\s)*?\\s+,\\s+\\w+, station( not)? joined', lines)],
                             split = ',')[[1]],
                    which = 'both')
  list(stationId = metaEng[1],
       stationName = metaEng[2],
       province = metaEng[3],
       latitude = latitude,
       longitude = longitude,
       elevation = elevation,
       updatedTo = metaEng[7],
       data = df)
}
