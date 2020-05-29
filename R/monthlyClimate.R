#' Download a Cache a Met Climate Summary File
#'
#' Data are downloaded from the Environment and Climate Change Canada's
#' climate weather website and cached locally
#'
#' The data are downloaded from
#' \url{http://climate.weather.gc.ca/prods_servs/cdn_climate_summary_report_e.html}
#' using [utils::download.file()]
#' pointed to the Environment and Climate Change Canada website (reference 1)
#' using queries that had to be devised by reverse-engineering, since the agency
#' does not provide documentation about how to construct queries. Caution: the
#' query format changes from time to time, so [download.met.climateSummaries()] may work one
#' day, and fail the next.
#'
#' @param year A number giving the year of interest.
#' @param month A number giving the month of interest.
#' @param province A string giving the province abbreviation of interest.
#' @param type A string indicating which type of file to download, either
#' `"xml"` for an XML file or `"csv"` for a CSV file.
#' @param destdir Optional string indicating the directory in which to store downloaded files.
#' If not supplied, "." is used, i.e. the data file is stored in the present working directory.
#' @param destfile Optional string indicating the name of the file. If not supplied, the file
#' name is constructed from the other parameters of the function call, so subsequent calls with
#' the same parameters will yield the same result, thus providing the key to the caching scheme.
#'
#' @author Chantelle Layton
#'
#' @importFrom utils download.file
#' @importFrom utils capture.output
#'
#' @export


download.climateSummaries <- function(year, month, province, type,
                                          destdir = '.', destfile)
{

  if(missing(year) | missing(month)){
    warning("year or month was not provided, downloading the most recent file.")
    today <- as.POSIXlt(presentTime())
    year <- today$year + 1900
    month <- today$mon + 1 # so 1 = jan
    # data, for example, for the month of march, are available on april 5
    # so if the month day is less than 5, set the month
    # to be 2 less than today's. Some extra logic if 'today' is
    # january or february
    {
      if (month == 1) {
        year <- year - 1
        if (today$mday < 5) {
          month <- 11
        } else {
          month <- 12
        }
      } else if (month == 2) {
        if (today$mday < 5) {
          month <- 12
          year <- year - 1
        } else {
          month <- month - 1
        }
      } else {
        if (today$mday < 5) {
          month <- month - 2
        } else {
          month <- month - 1
        }
      }
    }
  }

  if(missing(month)){
    stop("Please provide a month.")
  }

  provinceChoices <- c('All', 'AB', 'BC', 'MB', 'NB', 'NL', 'NT', 'NS', 'NU', 'ON', 'PE', 'QC', 'SK', 'YT')
  if(!province %in% provinceChoices){
    stop("Province abbreviation '", province, "' is not correct, try one of the following: \n", paste(provinceChoices, collapse = ' '))
  }

  typeChoices <- c('csv', 'xml')
  if(!type %in% typeChoices){
    stop("type '", type, "' is not permitted ; try either 'csv' or 'xml' ")
  }

  if(missing(destfile)){
    destfile <- paste0('metClimateSummaries_',province,'_',year,ifelse(month < 10, paste0('0', month), month),'.', type)
  }


  url <- paste0('http://climate.weather.gc.ca/prods_servs/cdn_climate_summary_report_e.html?int',
                'Year=',year,
                '&intMonth=',month,
                '&prov=',province,
                '&dataFormat=',type,
                '&btnSubmit=Download+data')

  destination <- paste(destdir, destfile, sep="/")
  if (!force && 1 == length(list.files(path=destdir, pattern=paste("^", destfile, "$", sep="")))) {
    message(paste("Not downloading \"", destfile, "\" because it is already present in the \"", destdir, "\" directory\n", sep=""))
  } else {
    ##?owarn <- options()$warn # this, and the capture.output, quieten the processing
    ##?options(warn=-1)
    capture.output({download.file(url, destination, quiet=TRUE, mode = 'wb')})
    ##?options(warn=owarn)
    message(paste("Downloaded file stored as '", destination, "'\n", sep=""))
  }

}

#' @title Read met xml climate summary file
#'
#' @description Read xml formatted climate summary files from Environment and Climate Change Canada.
#' Since no file format is not formally published, the function has been based off of some sample
#' files that were downloaded on 20200529. File formats are subject to change, therefore, this
#' function has the potential to break.
#'
#' @param file a connection or a character string giving the name of the file to load.
#'
#' @author Chantelle Layton
#'
#' @return a data.frame containing all of the data, except for snow
#'
#' @importFrom XML xmlToList
#' @importFrom XML xmlParse
#'
#' @export


read.climateSummaries.xml <- function(file)
{
  xml <- xmlToList(xmlParse(file)) # a list
  ## The names of items in the list was discovered with
  ##     head(names(list))
  ## Isolate station data.
  isStation <- names(xml) %in% 'station'
  stationData <- xml[isStation]
  ## metadata for each station is
  ##  name
  ##  identifier
  ##  latitude
  ##   longitude
  ##   province
  ## data for each station is
  ##   mean_temperature
  ##   max_temperature
  ##   min_temperature
  ##   snow
  ##   precipitation
  ##   sunshine
  ##   degreedays
  # let's basically recreate a dataframe
  stationName <- unname(unlist(lapply(stationData, function(k) k[['name']])))
  stationId <- unname(unlist(lapply(stationData, function(k) k[['identifier']])))
  latitude <- unname(unlist(lapply(stationData, function(k) k[['latitude']])))
  longitude <- unname(unlist(lapply(stationData, function(k) k[['longitude']])))
  province <- unname(unlist(lapply(stationData, function(k) k[['province']][1])))
  meanTemperature <- unname(unlist(lapply(stationData, function(k) k[['mean_temperature']][1]))) # value is firs value
  maxTemperature <- unname(unlist(lapply(stationData, function(k) ifelse(is.null(k[['max_temperature']]), NA, k[['max_temperature']][1]))))
  minTemperature <- unname(unlist(lapply(stationData, function(k) ifelse(is.null(k[['min_temperature']]), NA, k[['min_temperature']][1]))))
  precipitation <- unname(unlist(lapply(stationData, function(k) k[['precipitation']][1])))
  sunshine <- unname(unlist(lapply(stationData, function(k) k[['name']])))
  degreeDaysHeating <- unname(unlist(lapply(stationData, function(k) ifelse(is.null(k[['degreedays']]), NA, k[['degreedays']][1]))))
  degreeDaysCooling <- unname(unlist(lapply(stationData, function(k) ifelse(is.null(k[['degreedays']]), NA, k[['degreedays']][2]))))
  month <- rep(unname(xml[['.attrs']][['month']]), length(stationData))
  year <- rep(unname(xml[['.attrs']][['year']]), length(stationData))

  df <- data.frame(stationName = stationName,
                   stationId = stationId,
                   year = year,
                   month = month,
                   latitude = latitude,
                   longitude = longitude,
                   province = province,
                   meanTemperature = meanTemperature,
                   maxTemperature = maxTemperature,
                   minTemperature = minTemperature,
                   precipitation = precipitation,
                   sunshine = sunshine,
                   degreeDaysHeating = degreeDaysHeating,
                   degreeDaysCooling = degreeDaysCooling,
                   stringsAsFactors = FALSE)
  df
}
