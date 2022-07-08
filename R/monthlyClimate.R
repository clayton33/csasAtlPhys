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
#' @param force Logical value indicating whether to force a download, even if the file already
#' exists locally.
#'
#' @author Chantelle Layton
#'
#' @importFrom utils download.file
#' @importFrom utils capture.output
#'
#' @export


download.climateSummaries <- function(year, month, province, type,
                                          destdir = '.', destfile,
                                      force = TRUE)
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

  if(!dir.exists(destdir)){
    dir.create(destdir, recursive = TRUE)
  }

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
#' @return a data.frame containing all of the data, except for snow and sunshine.
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
  # they changed the variable name for province sometime in the first half of 2022
  provTerrVarName <- c('province', 'province_or_territory')
  province <- unname(unlist(lapply(stationData, function(k) k[[which(names(k) %in% provTerrVarName)]][1])))
  meanTemperature <- unname(unlist(lapply(stationData, function(k) k[['mean_temperature']][1]))) # value is firs value
  maxTemperature <- unname(unlist(lapply(stationData, function(k) ifelse(is.null(k[['max_temperature']]), NA, k[['max_temperature']][1]))))
  minTemperature <- unname(unlist(lapply(stationData, function(k) ifelse(is.null(k[['min_temperature']]), NA, k[['min_temperature']][1]))))
  precipitation <- unname(unlist(lapply(stationData, function(k) k[['precipitation']][1])))
  degreeDaysHeating <- unname(unlist(lapply(stationData, function(k) ifelse(is.null(k[['degreedays']]), NA, k[['degreedays']][1]))))
  degreeDaysCooling <- unname(unlist(lapply(stationData, function(k) ifelse(is.null(k[['degreedays']]), NA, k[['degreedays']][2]))))
  month <- rep(unname(xml[['.attrs']][['month']]), length(stationData))
  year <- rep(unname(xml[['.attrs']][['year']]), length(stationData))

  df <- data.frame(stationName = stationName,
                   stationId = stationId,
                   year = as.numeric(year),
                   month = as.numeric(month),
                   latitude = as.numeric(latitude),
                   longitude = as.numeric(longitude),
                   province = province,
                   meanTemperature = as.numeric(meanTemperature),
                   maxTemperature = as.numeric(maxTemperature),
                   minTemperature = as.numeric(minTemperature),
                   precipitation = as.numeric(precipitation),
                   degreeDaysHeating = as.numeric(degreeDaysHeating),
                   degreeDaysCooling = as.numeric(degreeDaysCooling),
                   stringsAsFactors = FALSE)
  df
}

#' @title Convert monthly climate data to ahccd format
#'
#' @description This function takes the monthly climate data, which is read in as a data frame,
#' and puts it into the decided list format of the ahccd data.
#'
#' @param x a data.frame of monthly climate data that was manipulated after reading it in.
#'
#' @author Chantelle Layton
#'
#' @return a list that contains the same format as [read.ahccd]. Note that the `elevation` variable
#' is missing from the climate summary files.
#'
#' @export
#'

climateSummary2ahccd <- function(x) {
  metaDataNames <- c('stationName', 'stationId', 'latitude', 'longitude', 'province')
  okDataNames <- !names(x) %in% metaDataNames
  list(stationId = x$stationId[1],
       stationName = x$stationName[1],
       province = x$province[1],
       latitude = x$latitude[1],
       longitude = x$longitude[1],
       elevation = NA,
       updatedTo = NA,
       data = x[, okDataNames])
}

#' @title Combine ahccd and climate summary data
#'
#' @description This function combines ahccd and climate summary data together into the same list.
#'
#' @details There is some additional data that is read in and retained for the climate summary
#' data, but here only `meanTemperature` is retained. The climate summary metadata has `climateSummary`
#' appended to the name in the event that a comparisson is required. Ahccd data takes precedence,
#' so when comparing the year and month, the ahccd data is kept over the climate summary data.
#'
#' @param ahccd a list that has been read in using `read.ahccd`
#' @param climateSummary a list that has been converted to the ahccd format using `climateSummary2ahccd`
#'
#' @return a list in the same form as `read.ahccd`, but all metadata has been preserved. Any metadata from
#' the `climateSummary` data has `climate` added in front of it's name.
#' @author Chantelle Layton
#'
#' @importFrom oce geodDist
#'
#' @export


combineAhccdAndClimateSummary <- function(ahccd, climateSummary){
  # just do a double check that either
  # 1. the stationId matches
  # 2. the station name of the climateSummary matches portion of ahccd
  # 3. the distance between stations is within
  ok <- (ahccd[['stationId']] == climateSummary[['stationId']]) |
    grepl(ahccd[['stationName']], climateSummary[['stationName']]) |
    (geodDist(longitude1 = as.numeric(ahccd[['longitude']]),
              latitude1 = as.numeric(ahccd[['latitude']]),
              longitude2 = as.numeric(climateSummary[['longitude']]),
              latitude2 = as.numeric(climateSummary[['latitude']])) < 25)
  if(ok){
    adata <- ahccd[['data']]
    cdata <- climateSummary[['data']]
    okadd <- apply(cdata, 1, function(k) {!(k[['year']] %in% adata[['year']] & k[['month']] %in% adata[['month']])})
    okcols <- names(cdata) %in% c('year', 'month', 'meanTemperature')
    cdatadd <- data.frame(cdata[okadd,okcols], flag = rep('u', dim(cdata[okadd,])[1]))
    # re-name 'meanTemperature' to be able to rbind
    names(cdatadd)[names(cdatadd) %in% 'meanTemperature'] <- 'temperature'
    # rbind ahccd and climateSummary
    data <- rbind(adata, cdatadd)
    list(stationId = ahccd[['stationId']],
         climateStationId = climateSummary[['stationId']],
         stationName = ahccd[['stationName']],
         climateStationName = climateSummary[['stationName']],
         province = ahccd[['province']],
         climateProvince = climateSummary[['province']],
         longitude = ahccd[['longitude']],
         climateLongitude = climateSummary[['longitude']],
         latitude = ahccd[['latitude']],
         climateLatitude = climateSummary[['latitude']],
         elevation = ahccd[['elevation']],
         climateElevation = climateSummary[['elevation']],
         updatedTo = ahccd[['updatedTo']],
         data = data)
  }
}
