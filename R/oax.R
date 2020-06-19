#' @title Format CTD for input into optimal estimation routine
#'
#' @description This function takes a list of CTD profiles and puts them into
#' the format that is required for the optimal estimation routine.
#'
#' @author Chantelle Layton
#'
#' @param x a list of CTD profiles
#' @param var Variable name to be extracted
#' @param longitude0,latitude0 Reference location of the point from which distance is measured for each
#' station if utm is FALSE, ignored otherwise
#' @param addDayNumber Logical indicating whether to append day number to output
#' @param onlyBottom Logical indicating whether to extract only bottom values
#' @param noise Numeric value required for input into the optimal estimation routine, default
#' is set to `0.1` which should be used.
#' @param utm Logical indicating whether or not to use lonlat2utm function to calculate distances,
#' if FALSE, geodDist is used
#' @param zone zone used in calculation if utm is TRUE, ignored otherwise. Default is associated
#' with the summer groundfish area.
#'
#' @details The data format required to run the optimal estimation routine requires
#' that data have columns of data be x-dist, y-dist, depth, dayNumber(optional), data, noise
#'
#' @importFrom oce lonlat2utm
#' @importFrom oce geodXy
#'
#' @export

as.oax <- function(x, var, longitude0, latitude0, addDayNumber = FALSE, onlyBottom = FALSE, noise = 0.1, utm = TRUE, zone = 19){
  # put in some checks here
  # brainstorming
  # 1. Check that latitude0 is positive
  # 2. Check that longitude0 is negative
  # 3. Check that x is a list

  # get variables
  if(onlyBottom){
    longitude <- unlist(lapply(x, function(k) k[['longitude']]))
    latitude <- unlist(lapply(x, function(k) k[['latitude']]))
    p <- unlist(lapply(x, function(k) k[['pressure']][length(k[['pressure']])]))
    data <- unlist(lapply(x, function(k) k[[var]][length(k[[var]])]))
    noised <- rep(noise, length(data))
    time <- unlist(lapply(x, function(k) k[['time']]))
  } else {
    longitude <- unlist(lapply(x, function(k) rep(k[['longitude']], length(k[['temperature']]))))
    latitude <- unlist(lapply(x, function(k) rep(k[['latitude']], length(k[['temperature']]))))
    p <- unlist(lapply(x, function(k) k[['pressure']]))
    data <- unlist(lapply(x, function(k) k[[var]]))
    noised <- rep(noise, length(data))
    time <- unlist(lapply(x, function(k) {if(length(k[['time']]) == length(k[['pressure']])){
                                              k[['time']]} else {
                                              rep(k[['time']], length(k[['pressure']]))}}))
  }

  # calculate xy coordinates
  if(!utm){
    xy <- geodXy(longitude = longitude, latitude = latitude,
                 longitudeRef = longitude0, latitudeRef = latitude0) / 1000
    xx <- xy$x
    yy <- xy$y
  }
  if(utm){
    xy <- lonlat2utm(longitude = longitude, latitude = latitude,
                     zone = zone, km = TRUE)
    xx <- xy$easting
    yy <- xy$northing
  }

  if(addDayNumber){
    timelt <- as.POSIXlt(time, origin = '1970-01-01', tz = 'UTC')
    yearDay <- timelt$yday
    d <- cbind(xx, yy, yearDay, p, data, noised)
    colnames(d) <- c('xDistance', 'yDistance', 'dayNumber', 'pressure', var, 'noise')

  }
  if(!addDayNumber){
    d <- cbind(xx, yy, p, data, noised)
    colnames(d) <- c('xDistance', 'yDistance', 'pressure', var, 'noise')
  }

  d
}

#' @title Create deckfile for OAX
#'
#' @description This function will create character strings necessary
#' to save in the form of a deck file for input into OAX analysis.
#'
#' @details Currently, there are three different OAX analyses that are completed,
#' the parameter `which` indicates the specific program.
#'
#' @param dependent a character string indicating the name of the dependent
#' variable
#' @param dataFile a character string indicating the file name associated
#' with the input data
#' @param gridFile a character string indicating the file name associated with
#' the OAX grid
#' @param which a character string indicating which OAX analysis, options include
#' `winterGroundfish`, `summerGroundfish`, `snowCrab`.
#'
#' @author Chantelle Layton
#'
#' @export

createOaxDeckfile <- function(dependent, dataFile, gridFile, which){
  validWhich <- c('winterGroundfish',
                  'summerGroundfish',
                  'snowCrab')
  if(!which %in% validWhich){
    stop('Must provide a valid value for which, please read parameter description for which.')
  }

  # go through each which
  if(which == 'winterGroundfish'){
    d <- rbind(paste('DEPENDENT', dependent),
               'INDEPENDENT x y daynumber depth',
               'GLOBAL_SCALES 30.0 30.0 30.0 20.0',
               paste('DATAFILE', dataFile),
               paste('GRIDFILE', gridFile),
               'BUCKET 10',
               'NUM_CLOSEST 15',
               'METHOD EST_MEAN')
  }
  if(which == 'summerGroundfish'){
    d <- rbind(paste('DEPENDENT', dependent),
               'INDEPENDENT x y depth',
               'GLOBAL_SCALES 35.0 35.0 20.0',
               paste('DATAFILE', dataFile),
               paste('GRIDFILE', gridFile),
               'BUCKET 10',
               'NUM_CLOSEST 15',
               'METHOD EST_MEAN')
  }
  if(which == 'snowCrab'){
    d <- rbind(paste('DEPENDENT', dependent),
               'INDEPENDENT x y depth',
               'GLOBAL_SCALES 35.0 35.0 20.0',
               paste('DATAFILE', dataFile),
               paste('GRIDFILE', gridFile),
               'BUCKET 10',
               'NUM_CLOSEST 15',
               'METHOD EST_MEAN')
  }
  d
}
