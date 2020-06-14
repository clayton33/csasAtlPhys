#' @title Provide axis names for anomaly labels
#'
#' @description This function provides the axis name for anomaly variables
#'
#' @param item A character string the desired label. Supported labels include `temperatureAnomaly`,
#' `salinityAnomaly`, and `sigmaThetaAnomaly`.
#' @param sep An optional character string inserted between the unit and the unit bracket that encloses it.
#' If not provided, the default `oceUnitSep` is used.
#'
#' @author Chantelle Layton
#'
#' @export

getAnomalyLabel <- function(item, sep = ""){
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
    label <- bquote(.(var)*.(L)*degree*"C"*.(R))
  }
  if(item == 'salinityAnomaly'){
    var <- gettext("Salinity anomaly", domain = "R-csasAtlPhys")

  }
  if(item == 'sigmaThetaAnomaly'){
    if(Sys.getenv('LANG' == 'en')){
      var <- gettext('anomaly', domain = 'R-csasAtlPhys')
      label <- bquote(sigma[theta]*.(var)*.(L)*kg/m^3*.(R))
    } else if(Sys.getenv('LANG' == 'fr')){
      var <- gettext("anomaly of", domain = 'R-csasAtlPhys')
      label <- bquote(.(var)*sigma[theta]*.(L)*kg/m^3*.(R))
    } else{ # just use english convention
      label <- bquote(sigma[theta]*.(var)*.(L)*kg/m^3*.(R))
    }

  }
  if(item == 'normalizedAnomaly'){
    label <- gettext('Normalized anomaly', domain = 'R-csasAtlPhys')
  }
  if(item == 'annualAirTemperatureAnomaly'){
    var <- gettext('Annual air temperature anomaly', domain = 'R-csasAtlPhys')
    label <- bquote(.(var)*" "*.(L)*degree*"C"*.(R))
  }
  if(item == 'averageAirTemperatureAnomaly'){
    var <- gettext('Average air temperature anomaly', domain = 'R-csasAtlPhys')
    label <- bquote(.(var)*" "*.(L)*degree*"C"*.(R))
  }
  if(is.null(label)) stop('Please provide a valid item, if item desired has not been implemented, please contact creator.')
  label
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
  if(item == 'Yarmouth'){
    location <- gettext('Yarmouth', domain = 'R-csasAtlPhys')
  }
  if(item == 'Sable Island'){
    location <- gettext('Sable Island', domain = 'R-csasAtlPhys')
  }
  if(item == 'Saint John'){
    location <- gettext('Saint John', domain = 'R-csasAtlPhys')
  }
  if(item == 'Halifax'){
    location <- gettext('Halifax', domain = 'R-csasAtlPhys')
  }
  if(item == 'Boston'){
    location <- gettext('Boston', domain = 'R-csasAtlPhys')
  }
  if(item == 'St. Andrews'){
    location <- gettext('St. Andrews', domain = 'R-csasAtlPhys')
  }
  if(item == 'Cabot Strait'){
    location <- gettext('Cabot Strait', domain = 'R-csasAtlPhys')
  }
  if(item == 'Browns Bank'){
    location <- gettext('Browns Bank', domain = 'R-csasAtlPhys')
  }
  if(item == 'Louisbourg'){
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
  if(item == 'St. Anns Bank'){
    location <- gettext('St. Anns Bank', domain = 'R-csasAtlPhys')
  }
  if(item == 'North East Channel'){
    location <- gettext('North East Channel', domain = 'R-csasAtlPhys')
  }
  if(is.null(location)) stop('Please provide a valid location, if you believe the location has not been implemented, please contact creator.')
  location
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
  if(item == 'stationName'){
    label <- gettext('Station name', domain = 'R-csasAtlPhys')
  }
  if(item == 'stationId'){
    label <- gettext('Station ID', domain = 'R-csasAtlPhys')
  }
  if(item == 'elevation'){
    label <- gettext('Elevation', domain = 'R-csasAtlPhys')
  }
  if(item == 'slope'){
    label <- gettext('Slope', domain = 'R-csasAtlPhys')
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
  if(is.null(label)) stop('Please provide a valid item, if you believe the desired has not been implemented, please contact creator.')
  label
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
  if(item == 'anomaly'){
    label <- gettext('anomaly', domain = 'R-csasAtlPhys')
  }
  if(item == 'normalizedAnomaly'){
    label <- gettext('normalizedAnomaly', domain = 'R-csasAtlPhys')
  }
  if(item == 'year'){
    label <- gettext('year', domain = 'R-csasAtlPhys')
  }
  label
}
