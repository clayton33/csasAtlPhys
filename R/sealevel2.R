#' @title Read sea level data
#'
#' @description This function takes in sea level data and puts it into a form
#' that allows for easy use for calculations and data output. It's similar in nature
#' to as.ahccd.
#'
#' @param file a connection or a character string or vector giving the name(s) of the
#' file(s) to load.
#' @param combineData a logical value indicating whether or not to combine data
#' into a single time-series if the length of files is greater than one. If the
#' length of files is equal to one, then ignored. If FALSE then a list is returned.
#'
#' @author Chantelle Layton
#'
#' @importFrom oce read.sealevel
#'
#' @export

read.sealevel2 <- function(file, combineData = TRUE){
  # read in data for all 3 cases
  # put it into format for easy use
  if(length(file) == 1){
    dd <- read.sealevel(file)
    dl <- list(stationNumber = dd[['stationNumber']],
               stationName = dd[['stationName']],
               longitude = dd[['longitude']] * -1, # assuming longitude always needs to be changed to negative
               latitude = dd[['latitude']],
               data = data.frame(time = dd[['time']],
                                 elevation = dd[['elevation']]
                                 ))
  } else {
    dd <- lapply(file, read.sealevel)
    if(combineData){
      dl <- list(stationNumber = dd[[1]][['stationNumber']],
                 stationName = dd[[1]][['stationName']],
                 longitude = dd[[1]][['longitude']] * -1, # see above
                 latitude = dd[[1]][['latitude']],
                 data = data.frame(time = as.POSIXct(unlist(lapply(dd, function(k) k[['time']])),
                                                     origin = '1970-01-01',
                                                     tz = 'UTC'),
                                   elevation = unlist(lapply(dd, function(k) k[['elevation']]))))
    } else {
      dl <- lapply(dd, function(k) list(stationNumber = k[['stationNumber']],
                                        stationName = k[['stationName']],
                                        longitude = k[['longitude']] * -1,
                                        latitude = k[['latitude']],
                                        data = data.frame(time = as.POSIXct(k[['time']],
                                                                            origin = '1970-01-01',
                                                                            tz = 'UTC'),
                                                          elevation = k[['elevation']]
                                                          )))
    }
  }

  dl
}
