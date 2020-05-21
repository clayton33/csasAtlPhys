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
#' @details
#' Read Adjusted and Homogenized Canadian Climate Data files. Data can be downloaded from
#' the ftp site at \url{ftp://ccrp.tor.ec.gc.ca/pub/AHCCD/} . It is required that the data file be downloaded
#' along with the associated stations excel file. For example, if the 'Adj_monthly_rain.zip' file
#' is downloaded, then the 'Precipitation_Stations.xls' file should also be downloaded whether it
#' be using download.file(), or manually from the site.
#'
#' Flag description from 'Temperature_Documentation.doc', which is on the ftp site, last downloaded : 20190723,
#' M = missing value, E = estimated during archiving process, a = value has been adjusted due to homogeneity
#' and no flag indicates original unaltered value.
#'
#' @param file the file containing the data
#'
#' @importFrom utils read.table
#' @export

read.ahccd <- function(file){
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
  mm
}
