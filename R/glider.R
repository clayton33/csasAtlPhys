#' @title Read .dat glider data
#'
#' @description This function is very specialized for a non-standard data format. It reads in .dat files that
#' are being produced, as of 20191002, for DFO gliders, as processed by Dave Hebert.
#'
#' @param file a .dat file
#'
#' @details This function reads in the data and then puts the data into `oce` ctd objects for easy
#' use. Apart from the standard physical parameters, temperature and salinity, oxygen percent saturation
#' and fluoresence is also added as oxygenPercentSaturation and fluorometer respectively.
#'
#' @return a list of ctd objects
#'
#' @author Chantelle Layton
#'
#' @importFrom oce as.ctd
#' @importFrom oce oceSetData
#'
#' @export
#'
read.gliderDat <- function(file) {
  dd <- read.table(file, header = TRUE)
  okyear <- names(dd) %in% c('Year', 'Yr')
  okmonth <- names(dd) %in% c('Month', 'Mth')
  okday <- names(dd) %in% c('Day')
  okhour <- names(dd) %in% c('Hour', 'Hr')
  okminute <- names(dd) %in% c('Min', 'Mn')
  oksecond <- names(dd) %in% c('Sec')
  time <- as.POSIXct(paste(paste(dd[, okyear], dd[,okmonth], dd[, okday], sep = '-'),
                           paste(dd[,okhour], dd[,okminute], dd[,oksecond], sep = ':'), sep = ' '), tz = 'UTC')
  pressure <- as.vector(dd$P)
  oklatitude <- names(dd) %in% c('Lat')
  oklongitude <- names(dd) %in% c('Long', 'Lon')
  latitude <- as.vector(dd[, oklatitude])
  longitude <- as.vector(dd[, oklongitude])
  #npos <- as.vector(dd[['npos']])
  temperature <- dd$T
  #ndataT <- dd[['ndatatw']]
  salinity <- dd$S
  okfl <- grep('^Chl', names(dd))
  fluorometer <- dd[,okfl]
  oxygenSaturation <- dd$O2percentsat
  # this is dirty
  badS <- salinity > 40
  salinity[badS] <- NA
  badT <- temperature < -3
  temperature[badT] <- NA
  badOxy <- oxygenSaturation < 0
  oxygenSaturation[badOxy] <- NA
  #ndataS <- dd[['ndatas']]

  utime <- unique(time)
  ctd <- vector(mode = 'list', length = length(utime))
  for (i in 1:length(utime)){
    ok <- time == utime[i]
    ctd[[i]] <- as.ctd(temperature = temperature[ok],
                       salinity = salinity[ok],
                       longitude = longitude[ok],
                       latitude = latitude[ok],
                       pressure = pressure[ok],
                       time = time[ok],
                       startTime = utime[i])
    ctd[[i]] <- oceSetData(ctd[[i]], 'fluorometer', fluorometer[ok])
    ctd[[i]] <- oceSetData(ctd[[i]], 'oxygenPercentSaturation', oxygenSaturation[ok])
  }
  ctd
}
