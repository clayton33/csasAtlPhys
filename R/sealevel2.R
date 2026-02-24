#' @title Read sea level data
#'
#' @description This function takes in sea level data and puts it into a form
#' that allows for easy use for calculations and data output. It's similar in nature
#' to as.ahccd.
#'
#' @param file a connection or a character string or list giving the name(s) of the
#' file(s) to load.
#' @param combineData a logical value indicating whether or not to combine data
#' into a single time-series if the length of files is greater than one. If the
#' length of files is equal to one, then ignored. If FALSE then a list is returned.
#'
#' @author Chantelle Layton
#'
#' @importFrom oce read.sealevel
#' @importFrom oce read.sealevel.gc2026
#'
#' @export

read.sealevel2 <- function(file, combineData = TRUE){
  # read in data for all 3 cases
  # put it into format for easy use
  if(length(file) == 1){
    dd <- read.sealevel(file)
    dl <- list(stationNumber = ifelse('stationNumber' %in% names(dd@metadata), dd[['stationNumber']], dd[['Station Code - Code de la station']]),
               stationName = ifelse('stationName' %in% names(dd@metadata), dd[['stationName']], dd[['Station Name - Nom de la station']]),
               longitude = ifelse('longitude' %in% names(dd@metadata), dd[['longitude']] * -1, dd[['Longitude - Longitude']]),
               latitude = ifelse('latitude' %in% names(dd@metadata), dd[['latitude']], dd[['Latitude - Latitude']]),
               data = data.frame(time = dd[['time']],
                                 elevation = dd[['elevation']]
                                 ))
  } else {
    dd <- lapply(file, function(k) if(length(k) == 1) read.sealevel(k) else oce::read.sealevel.gc2026(k))
    if(combineData){
      k <- dd[[1]]
      dl <- list(stationNumber = ifelse('stationNumber' %in% names(k@metadata), k[['stationNumber']], k[['Station Code - Code de la station']]),
                 stationName = ifelse('stationName' %in% names(k@metadata), k[['stationName']], k[['Station Name - Nom de la station']]),
                 longitude = ifelse('longitude' %in% names(k@metadata), k[['longitude']] * -1, k[['Longitude - Longitude']]),
                 latitude = ifelse('latitude' %in% names(k@metadata), k[['latitude']], k[['Latitude - Latitude']]),
                 data = data.frame(time = as.POSIXct(unlist(lapply(dd, function(k) k[['time']])),
                                                     origin = '1970-01-01',
                                                     tz = 'UTC'),
                                   elevation = unlist(lapply(dd, function(k) k[['elevation']]))))
    } else {
      dl <- lapply(dd, function(k) list(stationNumber = ifelse('stationNumber' %in% names(k@metadata), k[['stationNumber']], k[['Station Code - Code de la station']]),
                                        stationName = ifelse('stationName' %in% names(k@metadata), k[['stationName']], k[['Station Name - Nom de la station']]),
                                        longitude = ifelse('longitude' %in% names(k@metadata), k[['longitude']] * -1, k[['Longitude - Longitude']]),
                                        latitude = ifelse('latitude' %in% names(k@metadata), k[['latitude']], k[['Latitude - Latitude']]),
                                        data = data.frame(time = as.POSIXct(k[['time']],
                                                                            origin = '1970-01-01',
                                                                            tz = 'UTC'),
                                                          elevation = k[['elevation']]
                                                          )))
    }
  }

  dl
}
