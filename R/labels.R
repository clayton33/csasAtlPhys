#' @title Provide axis names for anomaly labels
#'
#' @description This function provides the axis name for anomaly variables
#'
#' @param item A character string the desired label. Supported labels include `temperatureAnomaly`,
#' `salinityAnomaly`, and `sigmaThetaAnomaly`.
#' @param sep An optional character string inserted between the unit and the unit bracket that encloses it.
#' If not provided, the default `oceUnitSep` is used.
#' @param bold A logical value indicating whether or not to return a bold version of the label.
#'
#' @author Chantelle Layton
#'
#' @export

getAnomalyLabel <- function(item, sep = "", bold = FALSE){
  #if (getOption("oceUnitBracket") == "[") {
    L <- " ["
    R <- "]"
  #} else {
  #  L <- " ("
  #  R <- ")"
  #}
  #if (missing(sep)) {
  #  tmp <- getOption("oceUnitSep")
  #  sep <- if (!is.null(tmp)) tmp else ""
  #}
  L <- paste(L, sep, sep="")
  R <- paste(sep, R, sep="")
  label <- NULL
  if(item == 'temperatureAnomaly'){
    var <- gettext("Temperature anomaly", domain="R-csasAtlPhys")
    if(bold){
      label <- bquote(bold(.(var)*.(L)*degree*"C"*.(R)))
    } else {
      label <- bquote(.(var)*.(L)*degree*"C"*.(R))
    }

  }
  if(item == 'salinityAnomaly'){
    var <- gettext("Practical salinity anomaly", domain = "R-csasAtlPhys")
    if(bold){
      label <- bquote(bold(.(var)))
    } else {
      label <- var
    }
  }
  if(item == 'sigmaThetaAnomaly'){
    if(Sys.getenv('LANG') == 'en'){
      var <- gettext('anomaly', domain = 'R-csasAtlPhys')
      if(bold){
        label <- bquote(bold(sigma[theta]*' '*.(var)*.(L)*kg/m^3*.(R)))
      } else {
        label <- bquote(sigma[theta]*' '*.(var)*.(L)*kg/m^3*.(R))
      }
    } else if(Sys.getenv('LANG') == 'fr'){
      var <- gettext("anomaly of", domain = 'R-csasAtlPhys')
      if(bold){
        label <- bquote(bold(.(var)*' '*sigma[theta]*' '*.(L)*kg/m^3*.(R)))
      } else {
        label <- bquote(.(var)*' '*sigma[theta]*' '*.(L)*kg/m^3*.(R))
      }
    } else{ # just use english convention
      var <- gettext('anomaly', domain = 'R-csasAtlPhys')
      if(bold){
        label <- bquote(bold(sigma[theta]*' '*.(var)*' '*.(L)*kg/m^3*.(R)))
      } else {
        label <- bquote(sigma[theta]*' '*.(var)*' '*.(L)*kg/m^3*.(R))
      }
    }
  }
    if(item == 'sigmaThetaGradientAnomaly'){
      if(Sys.getenv('LANG') == 'en'){
        var <- gettext('anomaly', domain = 'R-csasAtlPhys')
        if(bold){
          label <- bquote(bold('d'* rho * '/dz' *' '*.(var)*.(L)*'kg ' * m ^-3 * '/m'*.(R)))
        } else {
          label <- bquote('d'* rho * '/dz' *' '*.(var)*.(L)*'kg ' * m ^-3 * '/m'*.(R))
        }
      } else if(Sys.getenv('LANG') == 'fr'){
        var <- gettext("Anomaly", domain = 'R-csasAtlPhys')
        if(bold){
          label <- bquote(bold(.(var)*' '*'d'* rho * '/dz'*' '*.(L)*'kg ' * m ^-3 * '/m'*.(R)))
        } else {
          label <- bquote(.(var)*' '*'d'* rho * '/dz'*' '*.(L)*'kg ' * m ^-3 * '/m'*.(R))
        }
      } else{ # just use english convention
        var <- gettext('anomaly', domain = 'R-csasAtlPhys')
        if(bold){
          label <- bquote(bold('d'* rho * '/dz'*' '*.(var)*' '*.(L)*'kg ' * m ^-3 * '/m'*.(R)))
        } else {
          label <- bquote('d'* rho * '/dz'*' '*.(var)*' '*.(L)*'kg ' * m ^-3 * '/m'*.(R))
        }
      }
    }
  if(item == 'normalizedAnomaly'){
    var <- gettext('Normalized anomaly', domain = 'R-csasAtlPhys')
    if(bold){
      label <- bquote(bold(.((var))))
    } else {
      label <- var
    }
  }
  if(item == 'annualAirTemperatureAnomaly'){
    var <- gettext('Annual air temperature anomaly', domain = 'R-csasAtlPhys')
    if(bold){
      label <- bquote(bold(.(var)*" "*.(L)*degree*"C"*.(R)))
    } else {
      label <- bquote(.(var)*" "*.(L)*degree*"C"*.(R))
    }
  }
  if(item == 'averageAirTemperatureAnomaly'){
    var <- gettext('Average air temperature anomaly', domain = 'R-csasAtlPhys')
    if(bold){
      label <- bquote(bold(.(var)*" "*.(L)*degree*"C"*.(R)))
    } else {
      label <- bquote(.(var)*" "*.(L)*degree*"C"*.(R))
    }
  }
  if(is.null(label)){
    cat('Please provide a valid item, if you believe the desired has not been implemented, please contact creator.', sep = '\n')
    cat('Returning item')
    item
  } else {
    label
  }
}

#' @title Provide names for various labels
#'
#' @description This function provides various labels.
#'
#' @param item A character string the desired label.
#' @param sep An optional character string inserted between the unit and the unit bracket that encloses it.
#' If not provided, the default `oceUnitSep` is used.
#'
#' @author Chantelle Layton
#'
#' @export

getLabel <- function(item, sep = ""){
  L <- " ["
  R <- "]"

  L <- paste(L, sep, sep="")
  R <- paste(sep, R, sep="")
  label <- NULL
  if(item == 'Sea level height'){
    var <- gettext('Sea level height', domain = 'R-csasAtlPhys')
    label <- bquote(.(var)*" "*.(L)*"m"*.(R))
  }
  if(item == 'Residual height'){
    var <- gettext('Residual height', domain = 'R-csasAtlPhys')
    label <- bquote(.(var)*" "*.(L)*"m"*.(R))
  }
  if(item == 'NAO'){
    label <- gettext('NAO', domain = 'R-csasAtlPhys')
  }
  if(item == 'NAO index'){
    label <- gettext('NAO index', domain = 'R-csasAtlPhys')
  }
  if(item == 'North Atlantic oscillation index'){
    label <- gettext('North Atlantic oscillation index', domain = 'R-csasAtlPhys')
  }
  if(item == 'Index'){
    label <- gettext('Index', domain = 'R-csasAtlPhys')
  }
  if(item == 'Surface sea level pressure climatology'){
    label <- gettext('Surface sea level pressure climatology', domain = 'R-csasAtlPhys')
  }
  if(item == 'Surface sea level pressure'){
    label <- gettext('Surface sea level pressure', domain = 'R-csasAtlPhys')
  }
  if(item == 'Total volume'){
    label <- gettext('Total volume', domain = 'R-csasAtlPhys')
  }
  if(item == 'CIL thickness'){
    var <- gettext('CIL thickness', domain = 'R-csasAtlPhys')
    label <- bquote(.(var)*" "*.(L)*"m"*.(R))
  }
  if(item == 'Annual'){
    label <- gettext('Annual', domain = 'R-csasAtlPhys')
  }
  if(item == 'oxygenPercentSaturation'){
    label <- gettext('Oxygen saturation', domain = 'R-csasAtlPhys')
  }
  if(is.null(label)){
    cat('Please provide a valid item, if you believe the desired has not been implemented, please contact creator.', sep = '\n')
    cat('Returning item')
    item
  } else {
    label
  }
}

#' @title Provide labels for locations.
#'
#' @description This function provides labels for locations in the
#' Atlantic region.
#'
#' @param item A character string the desired location.
#'
#' @author Chantelle Layton
#'
#' @export

getLocationName <- function(item){
  location <- NULL
  if(item == 'Emerald Basin'){
    location <- gettext('Emerald Basin', domain = 'R-csasAtlPhys')
  }
  if(item == 'Sable Island'){
    location <- gettext('Sable Island', domain = 'R-csasAtlPhys')
  }
  if(item == 'Georges Basin'){
    location <- gettext('Georges Basin', domain = 'R-csasAtlPhys')
  }
  if(item == 'Georges Bank'){
    location <- gettext('Georges Bank', domain = 'R-csasAtlPhys')
  }
  if(item == 'Laurentian Channel'){
    location <- gettext('Laurentian Channel', domain = 'R-csasAtlPhys')
  }
  if(item == 'Gulf of Maine'){
    location <- gettext('Gulf of Maine', domain = 'R-csasAtlPhys')
  }
  if(item == 'Sydney'){
    location <- gettext('Sydney', domain = 'R-csasAtlPhys')
  }
  if(item == 'North Sydney'){
    location <- gettext('North Sydney', domain = 'R-csasAtlPhys')
  }
  if(item %in% c('Yarmouth', 'yarmouth')){
    location <- gettext('Yarmouth', domain = 'R-csasAtlPhys')
  }
  if(item == 'Sable Island'){
    location <- gettext('Sable Island', domain = 'R-csasAtlPhys')
  }
  if(item == 'Saint John'){
    location <- gettext('Saint John', domain = 'R-csasAtlPhys')
  }
  if(item %in% c('Halifax', 'halifax', 'halifaxInshore')){
    location <- gettext('Halifax', domain = 'R-csasAtlPhys')
  }
  if(item == 'Boston'){
    location <- gettext('Boston', domain = 'R-csasAtlPhys')
  }
  if(item == 'St. Andrews' | item == 'St.Andrews'){
    location <- gettext('St. Andrews', domain = 'R-csasAtlPhys')
  }
  if(item %in% c('Cabot Strait', 'cabotStrait')){
    location <- gettext('Cabot Strait', domain = 'R-csasAtlPhys')
  }
  if(item %in% c('Browns Bank', 'brownsBank')){
    location <- gettext('Browns Bank', domain = 'R-csasAtlPhys')
  }
  if(item %in% c('Louisbourg', 'louisbourg')){
    location <- gettext('Louisbourg', domain = 'R-csasAtlPhys')
  }
  if(item == 'Misaine Bank'){
    location <- gettext('Misaine Bank', domain = 'R-csasAtlPhys')
  }
  if(item == 'Lurcher Shoal'){
    location <- gettext('Lurcher Shoal', domain = 'R-csasAtlPhys')
  }
  if(item == 'E Georges Bank'){
    location <- gettext('E Georges Bank', domain = 'R-csasAtlPhys')
  }
  if(item %in% c('St. Anns Bank', 'St Anns Bank', 'stAnnsBank')){
    location <- gettext('St. Anns Bank', domain = 'R-csasAtlPhys')
  }
  if(item %in% c('Northeast Channel', 'northEastChannel')){
    location <- gettext('Northeast Channel', domain = 'R-csasAtlPhys')
  }
  if(item %in% c('Portsmouth', 'portsmouth')){
    location <- gettext('Portsmouth', domain = 'R-csasAtlPhys')
  }
  if(item %in% c('Laurentian Channel Mouth', 'laurentianChannelMouth')){
    location <- gettext('Laurentian Channel Mouth', domain = 'R-csasAtlPhys')
  }
  if(item %in% c('St. Pierre Bank', 'stPierreBank')){
    location <- gettext('St. Pierre Bank', domain = 'R-csasAtlPhys')
  }
  if(item == 'Gully'){
    location <- gettext('Gully', domain = 'R-csasAtlPhys')
  }
  if(item == 'theGully'){
    location <- gettext('The Gully', domain = 'R-csasAtlPhys')
  }
  if(item == 'theGullyNew'){
    location <- gettext('The Gully', domain = 'R-csasAtlPhys')
  }
  if(item == 'laHaveBank'){
    location <- gettext('La Have Bank', domain = 'R-csasAtlPhys')
  }
  if(item == 'sableIslandBank'){
    location <- gettext('Sable Island Bank', domain = 'R-csasAtlPhys')
  }
  if(item == 'roseway'){
    location <- gettext('Roseway', domain = 'R-csasAtlPhys')
  }
  if(item == 'NSC'){
    location <- gettext('NSC', domain = 'R-csasAtlPhys')
  }
  if(item == '4XSS'){
    location <- gettext('4XSS', domain = 'R-csasAtlPhys')
  }
  if(item == '4XeGoM+BoF'){
    location <- gettext('4XeGoM+BoF', domain = 'R-csasAtlPhys')
  }
  if(item == '4V' | item == '4v'){
    location <- gettext('4V', domain = 'R-csasAtlPhys')
  }
  if(item == '4Vn' | item == '4vn'){
    location <- gettext('4Vn', domain = 'R-csasAtlPhys')
  }
  if(item == '4Vs' | item == '4vs'){
    location <- gettext('4Vs', domain = 'R-csasAtlPhys')
  }
  if(item == '4W' | item == '4w'){
    location <- gettext('4W', domain = 'R-csasAtlPhys')
  }
  if(item == '4X' | item == '4x'){
    location <- gettext('4X', domain = 'R-csasAtlPhys')
  }
  if(item == 'Prince5'){
    location <- gettext('Prince5', domain = 'R-csasAtlPhys')
  }
  if(item == 'Scotian Shelf' | item == 'Scotian shelf'){
    location <- gettext('Scotian Shelf', domain = 'R-csasAtlPhys')
  }
  if(item == 'GoM inflow ratio'){
    location <- gettext('GoM inflow ratio', domain = 'R-csasAtlPhys')
  }
  if(item == 'Cabot Strait (west), nearshore'){
    location <- gettext('Cabot Strait (west), nearshore', domain = 'R-csasAtlPhys')
  }
  if(item == 'Halifax, nearshore'){
    location <- gettext('Halifax, nearshore', domain = 'R-csasAtlPhys')
  }
  if(item == 'Halifax, shelfbreak'){
    location <- gettext('Halifax, shelfbreak', domain = 'R-csasAtlPhys')
  }
  if(item == 'Cape Sable Island, nearshore'){
    location <- gettext('Cape Sable Island, nearshore', domain = 'R-csasAtlPhys')
  }
  if(item == 'Northeast Channel (inflow)'){
    location <- gettext('Northeast Channel (inflow)', domain = 'R-csasAtlPhys')
  }
  if(is.null(location)){
    cat('Please provide a valid location, if you believe the location has not been implemented, please contact creator.', sep = '\n')
    cat('Returning item')
    item
  } else {
    location
  }
}

#' @title Get metadata translation
#'
#' @description This function serves as a interface to obtain translations for a number
#' of potential meta data fields that will be needed when saving data products.
#'
#' @param item A character string the desired label.
#'
#' @author Chantelle Layton
#'
#' @export
#'
getMetadata <- function(item){
  label <- NULL
  if(item == 'filename'){
    label <- gettext('Filename', domain = 'R-csasAtlPhys')
  }
  if(item == 'dateCreated'){
    label <- gettext('Creation date', domain = 'R-csasAtlPhys')
  }
  if(item == 'contact'){
    label <- gettext('Contact', domain = 'R-csasAtlPhys')
  }
  if(item == 'description'){
    label <- gettext('Description', domain = 'R-csasAtlPhys')
  }
  if(item == 'longitude'){
    label <- gettext('Longitude', domain = 'R-csasAtlPhys')
  }
  if(item == 'latitude'){
    label <- gettext('Latitude', domain = 'R-csasAtlPhys')
  }
  if(item == 'divisionLongitude'){
    label <- gettext('Division longitude', domain = 'R-csasAtlPhys')
  }
  if(item == 'divisionLatitude'){
    label <- gettext('Division latitude', domain = 'R-csasAtlPhys')
  }
  if(item == 'areaLongitude'){
    label <- gettext('Area longitude', domain = 'R-csasAtlPhys')
  }
  if(item == 'areaLatitude'){
    label <- gettext('Area latitude', domain = 'R-csasAtlPhys')
  }
  if(item == 'boundaryLongitude'){
    label <- gettext('Boundary longitude', domain = 'R-csasAtlPhys')
  }
  if(item == 'boundaryLatitude'){
    label <- gettext('Boundary latitude', domain = 'R-csasAtlPhys')
  }
  if(item == 'stationName'){
    label <- gettext('Station name', domain = 'R-csasAtlPhys')
  }
  if(item == 'regionName'){
    label <- gettext('Region name', domain = 'R-csasAtlPhys')
  }
  if(item == 'divisionName'){
    label <- gettext('Division name', domain = 'R-csasAtlPhys')
  }
  if(item == 'areaName'){
    label <- gettext('Area name', domain = 'R-csasAtlPhys')
  }
  if(item == 'areaNumber'){
    label <- gettext('Area number', domain = 'R-csasAtlPhys')
  }
  if(item == 'variable'){
    label <- gettext('Variable', domain = 'R-csasAtlPhys')
  }
  if(item == 'stationId'){
    label <- gettext('Station ID', domain = 'R-csasAtlPhys')
  }
  if(item == 'elevation'){
    label <- gettext('Elevation', domain = 'R-csasAtlPhys')
  }
  if(item == 'depth'){
    label <- gettext('Depth', domain = 'R-csasAtlPhys')
  }
  if(item == 'slope'){
    label <- gettext('Slope', domain = 'R-csasAtlPhys')
  }
  if(item == 'slopeUnit'){
    label <- gettext('Slope unit', domain = 'R-csasAtlPhys')
  }
  if(item == 'standardDeviation'){
    label <- gettext('Standard deviation', domain = 'R-csasAtlPhys')
  }
  if(item == 'climatologyStandardDeviation'){
    label <- gettext('Climatological standard deviation', domain = 'R-csasAtlPhys')
  }
  if(item == 'climatologyMean'){
    label <- gettext('Climatological mean', domain = 'R-csasAtlPhys')
  }
  if(item == 'confidenceInterval'){
    label <- gettext('Confidence interval', domain = 'R-csasAtlPhys')
  }
  if(item == 'climatologyReferenceYears'){
    label <- gettext('Climatological reference years', domain = 'R-csasAtlPhys')
  }
  if(item == 'monthsAveraged'){
    label <- gettext('Months averaged', domain = 'R-csasAtlPhys')
  }
  if(is.null(label)){
    cat('Please provide a valid item, if you believe the desired has not been implemented, please contact creator.', sep = '\n')
    cat('Returning item')
    item
  } else {
    label
  }
}

#' @title Get data translation
#'
#' @description This function serves as a interface to obtain translations for a number
#' of potential data fields that will be needed when saving data products.
#'
#' @param item A character string the desired label.
#'
#' @author Chantelle Layton
#'
#' @export
#'
getData <- function(item){
  label <- NULL
  if(item == 'anomaly'){
    label <- gettext('anomaly', domain = 'R-csasAtlPhys')
  }
  if(item == 'normalizedAnomaly'){
    label <- gettext('normalizedAnomaly', domain = 'R-csasAtlPhys')
  }
  if(item == 'year'){
    label <- gettext('year', domain = 'R-csasAtlPhys')
  }
  if(item == 'Year'){
    label <- gettext('Year', domain = 'R-csasAtlPhys')
  }
  if(item == 'month'){
    label <- gettext('month', domain = 'R-csasAtlPhys')
  }
  if(item == 'Month'){
    label <- gettext('Month', domain = 'R-csasAtlPhys')
  }
  if(item == 'time'){
    label <- gettext('time', domain = 'R-csasAtlPhys')
  }
  if(item == 'elevation'){
    label <- gettext('elevation', domain = 'R-csasAtlPhys')
  }
  if(item == 'elevationResidual'){
    label <- gettext('elevationResidual', domain = 'R-csasAtlPhys')
  }
  if(item == 'nao'){
    label <- gettext('nao', domain = 'R-csasAtlPhys')
  }
  if(item == 'deviation'){
    label <- gettext('devation', domain = 'R-csasAtlPhys')
  }
  if(item == 'rank'){
    label <- gettext('rank', domain = 'R-csasAtlPhys')
  }
  if(item == 'volume'){
    label <- gettext('volume', domain = 'R-csasAtlPhys')
  }
  if(item == 'temperature'){
    label <- gettext('temperature', domain = 'R-csasAtlPhys')
  }
  if(item == 'minimumTemperature'){
    label <- gettext('minimumTemperature', domain = 'R-csasAtlPhys')
  }
  if(item == 'sigmaThetaGradient'){
    label <- gettext('Density gradient', domain = 'R-csasAtlPhys')
  }
  if(item == 'depth'){
    label <- gettext('depth', domain = 'R-csasAtlPhys')
  }
  if(is.null(label)){
    cat('Please provide a valid item, if you believe the desired has not been implemented, please contact creator.', sep = '\n')
    cat('Returning item')
    item
  } else {
    label
  }
}

#' @title Get unit translation
#'
#' @description This function serves as a interface to obtain translations for a number
#' of potential units needed when plotting.
#'
#' @param item A character string the desired label.
#'
#' @author Chantelle Layton
#'
#' @export
#'
getUnit <- function(item){
  if(item == 'cm/century'){
    label <- gettext('cm/century', domain = 'R-csasAtlPhys')
  }
  label
}

#' @title Make cruise map title
#'
#' @description This function constructs a nice title that includes the
#' mission number, with the long ship name, the date ranges, and
#' the number of stations.
#'
#' @param ctd a list of ctd stations
#'
#' @author Chantelle Layton
#'
#' @export
#'

makeCruiseMapTitle <- function(ctd) {
  # get the time from each ctd
  time <- as.POSIXct(unlist(lapply(ctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
  # obtain the cruise number and tease it apart
  # into ship, year, and number
  cruiseNumber <- unique(unlist(lapply(ctd, function(k) k[['cruiseNumber']])))
  ship <- lapply(cruiseNumber, function(k) ifelse(nchar(k) == 10 , substr(k, start = 1, stop = 3),
                                                  substr(k, start = 1, stop = 2)))
  year <- lapply(cruiseNumber, function(k) ifelse(nchar(k) == 10 , substr(k, start = 4, stop = 7),
                                                  substr(k, start = 3, stop = 6)))
  number <- lapply(cruiseNumber, function(k) ifelse(nchar(k) == 10 , substr(k, start = 8, stop = 10),
                                                    substr(k, start = 7, stop = 9)))

  lenship <- length(unique(unlist(ship)))
  lenyear <- length(unique(unlist(year)))
  lennumber <- length(unique(unlist(number)))
  if(length(lenship) < length(ship) &
     length(lenyear) < length(year) &
     length(number) == length(number)){
    shipname <- getShipnameLong(unique(unlist(ship)))
    numbers <- paste0(unique(unlist(number)), collapse ='/')
    shipnamenumber <- paste(shipname, numbers)
  } else {
    cship <- mapply(function(x, y) c(getShipnameLong(x), y), ship, number)
    shipnamenumber <- paste(apply(cship, 2, paste, collapse = ' '), collapse = ' , ')
  }

  timerange <- as.POSIXlt(range(time))
  if(timerange$year[1] == timerange$year[2]){
    start <- format(range(time)[1], '%d %b')
    end <- format(range(time)[2], '%d %b %Y')
  } else {
    start <- format(range(time)[1], '%d %b, %Y')
    end <- format(range(time)[2], '%d %b %Y')
  }
  # will probably eventually need to figure out the cex if there are two different ships
  # and the time ranges over 2 years
  nstn <- length(ctd)
  bquote(.(shipnamenumber) * ',' ~ .(start) ~ .(gettext('to', domain = 'R-csasAtlPhys')) ~ .(end) * ',' ~ .(nstn) ~ 'stations')
}

#' @title Make section plot title
#'
#' @description This function uses CTD for a given transect to make a nice title that
#' includes the transect name and the date range in which measurements were obtained.
#'
#' @param ctd a list of CTD objects
#'
#' @author Chantelle Layton
#'
#' @export

makeSectionPlotTitle <- function(ctd){
  transectInCtd <- unlist(lapply(ctd, function(k) 'transect' %in% names(k@metadata)))
  if(!all(transectInCtd)){
    stop("All CTD objects do not have 'transect' in the metadata slot.")
  }
  transect <- unlist(lapply(ctd, function(k) k[['transect']]))
  utransect <- unique(transect)
  if(length(utransect) > 1){
    stop("More than one transect has been identified in the supplied CTD objects")
  }

  transectName <- getLocationName(utransect)
  timeRange <- as.POSIXct(range(unlist(lapply(ctd, function(x) x[['time']])), na.rm = TRUE),
                          origin = '1970-01-01', tz = 'UTC')
  timeRangeFormat <- format(timeRange, '%d %b %Y')
  # right now it assumes that the profiles were all taken during the same year
  if(timeRangeFormat[1] == timeRangeFormat[2]){
    bquote(.(transectName) * ':' ~ .(timeRangeFormat[1]))
  } else {
    start <- format(timeRange[1], '%d %b')
    end <- timeRangeFormat[2]
    bquote(.(transectName) * ':' ~ .(start) ~ .(gettext('to', domain = 'R-csasAtlPhys')) ~ .(end))
  }
}

#' @title Get the long name of a ship abbreviation
#'
#' @description This function takes an abbreviation of a ship and returns the long name.
#'
#' @param x a character string indicating the abbreviation of a ship
#'
#' @author Chantelle Layton
#'
#' @export
#'
getShipnameLong <- function(x){
  long <- NULL
  if(x == 'COR'){
    long <- 'R/V Coriolis II'
  }
  if (x == 'HUD'){
    long <- paste(gettext('CCGS', domain = 'R-csasAtlPhys'),  'Hudson')
  }
  if ( x == 'NED'){
    long <- paste(gettext('CCGS', domain = 'R-csasAtlPhys'),  'Alfred Needler')
  }
  if (x == 'EN'){
    long <- 'R/V Endeavor'
  }
  if (x == 'TEL'){
    long <- paste(gettext('CCGS', domain = 'R-csasAtlPhys'),  'Teleost')
  }

  if(is.null(long)){
    cat('Ship abbreviation not found, please contact author and provide abbreviation and long name', sep = '\n')
    cat('Returning abbreviation')
    x
  } else {
    long
  }
}
