#' @title Calculate the approximate area for specified bins
#'
#' @description Calculate the area weighted value of a variable within a polygon
#'
#' @param x a vector of the data that is to be used to calculate the weighted area
#' @param longitude a vector of the same length as x
#' @param latitude a vector of the same length as x
#' @param gridDiffLongitude a numerical value indicating the grid spacing in longitude space
#' @param gridDiffLatitude a numerical value indicating the grid spacing in latitude space
#' @param useArea a logical indicating if x should be subset within an area
#' @param areaLongitude a vector of longitude values defining the area, ignored if useArea = FALSE
#' @param areaLatitude a vector of latitude values defining the area, ignored if useArea = FALSE
#' @param useBins logical indicating whether to bin x
#' @param bins a vector defining how to bin x, ignored if useBins = FALSE
#'
#' @author Chantelle Layton
#'
#' @importFrom sp point.in.polygon
#' @export
#'
#' @return a list of bin0 and bin1 which are the ranges in which data was averaged over.
#' binArea, which is the calculated area within each bin range, npoints, which is the
#' number of points included in the calculation. And finally, avg, which is the average
#' value within the bin.

binArea <- function(x, longitude, latitude, gridDiffLongitude, gridDiffLatitude, useArea = TRUE, areaLongitude, areaLatitude, useBins = TRUE , bins) {
  ####
  # 1. Obtain data points that are in the area if true
  ####
  {if(useArea){
    okarea <- point.in.polygon(point.x = longitude,
                               point.y = latitude,
                               pol.x = c(areaLongitude, areaLongitude[1]),
                               pol.y = c(areaLatitude, areaLatitude[1]))
    # use anything that is not equal to zero
    inarea <- okarea != 0
    # extract this data from x, and get the lon,lat associated with it
    d <- x[inarea]
    dlon <- longitude[inarea]
    dlat <- latitude[inarea]
  } else {
    d <- x
    dlon <- longitude
    dlat <- latitude
  }}

  ####
  # 2. Calculate areas for each bin and weighted mean value
  ####
  binarea <- npoints <- avg <- vector(mode = 'logical', length = length(bins)-1)
  {if(useBins){
    for(i in 1:(length(bins) -1)){
      okbin <- d >= bins[i] & d < bins[(i+1)]
      dbin <- d[okbin]
      if(!all(is.na(dbin))){
        binarea[i] <- sum(111.12^2 * gridDiffLongitude * gridDiffLatitude * cos(dlat[okbin] * (pi/180)), na.rm = TRUE)
        npoints[i] <- length(which(!is.na(dbin)))
        avg[i] <- mean(dbin, na.rm = TRUE)
      } else {
        binarea[i] <- 0
        npoints[i] <- 0
      }
    }
    #weightedMean <- sum((bins[1:(length(bins)-1)] + 0.5) * binarea, na.rm = TRUE) / sum(binarea, na.rm = TRUE)
  } else {
    message('Calculating area average not using bins has not been coded. Contact the author')
  }}
  return(list(bin0 = bins[1:(length(bins)-1)], bin1 = bins[2:length(bins)],
              binArea = binarea, npoints = npoints, avg = avg))
  #weightedMean
}


#' @title Calculate weighted area for binned data
#'
#' @description Calculate weighted area value for binned data
#'
#' @param bins a numeric vector indicating the bins
#' @param binarea a numeric vector of the total area calculated for the specified bin
#'
#' @export
#' @author Chantelle Layton
weightedArea <- function(bins, binarea){
  sum((bins + 0.5) * binarea, na.rm = TRUE) / sum(binarea, na.rm = TRUE)
}


#' @title Calculate cold intermediate layer thickness
#'
#' @param T a vector of temperatures
#' @param p a vector of pressure
#' @param longitude,latitude the location
#' @param dlongitude,dlatitude the grid spacing the longitude and latitude coordinates respectively
#' @param Tlim cold intermediate layer temperature limit, default of 4
#'
#' @details This function calculates the thickness and volume of the cold intermediate layer
#' at a given location. The thickness at 3 different locations along the profile is calculated
#' in the following manner. If the surface value is below Tlim, the thickness is calculated
#' as (p_i + p_(i+1)) / 2. If the bottom value, (p_i - p_(i-1))/2. If interior, (p_(i+1) - p(i-1)) / 2.
#'
#' @author Chantelle Layton
#' @return a list with thickness with units kilometer and volume with units kilometer^3
#' and the minimum temperature.
#' @export
#'
cilVolume <- function(T, p, longitude, latitude, dlongitude, dlatitude, Tlim = 4){
  thickness <- NULL
  idx <- NULL
  for (i in 1:length(p)){
    if(T[i] <= Tlim){
      {if(i == 1){ # first point
        thick <- (p[i] + p[(i+1)])/ 2
        thickness <- c(thickness, thick)
        idx <- c(idx, i)
      } else if(i == length(p)){ # bottom point
        thick <- (p[i] - p[(i-1)]) / 2
        thickness <- c(thickness, thick)
        idx <- c(idx, i)
      } else{ # interior point
        thick <- (p[(i+1)] - p[(i-1)]) / 2
        thickness <- c(thickness, thick)
        idx <- c(idx, i)
      }
      }
    }
  }

  if(is.null(thickness)) {
    thickness <- 0
    minTemp <- NA
  } else {
    minTemp <- min(T[idx], na.rm = TRUE)
  }

  # convert thickness from meters to kilometers
  thicknesskm <- sum(thickness) / 1000
  area <- (dlongitude * 111.12) * (cos(latitude * pi / 180) * dlatitude * 111.12)
  volume <- area * thicknesskm
  return(list(thickness = thicknesskm, volume = volume, minTemp = minTemp))
}

#' @title Find valid snow crab oax grid points
#'
#' @description This function identifies points that should be included
#' when calculating various metrics using the oax results for snow crab analysis.
#'
#' @param longitude,latitude an optional numeric vector of longitudes and latitudes,
#' note that both must be supplied if `checkNthl` or `checkIgnore` are `TRUE`.
#' @param pressure an optional numeric vector of pressure, ignored if
#' `checkPressure` is `FALSE`.
#' @param checkNthl a logical value indicating whether or not to check points in
#' western Northumberland, default is `TRUE`.
#' @param checkIgnore a logical value indicating whether or not to check for points
#' that have been identified to be ignored.
#' @param checkPressure a logical value indicating whether or not to check points
#' based on the pressure, default is `FALSE`.
#' @param pressureMinimum a numeric value indicating the minimum pressure value
#' for valid points, default is 20. If `checkPressure` is `FALSE`, then ignored.
#' @param pressureMaximum a numeric value indicating the maximum pressure value
#' for valid points, default is 200. If `checkPressure` is `FALSE`, then ignored.
#'
#' @details The oax snow crab analysis grids have varied over the years. When
#' a new one was developed, for continuity to past results, various points
#' are ommited from the analysis. See the source code for more details,
#' but in general, there are a handful of points that are omitted from the
#' western Northumberland strait, various ones to be ignored along the coast,
#' and points where the bottom depth of a grid point that is less than 20m and
#' greater than 200 m.
#'
#' @return a vector of logical values, TRUE indicating a point that should be included
#' in analysis.
#'
#' @author Chantelle Layton
#'
#' @export

findValidSnowCrabGridPoints <- function(longitude, latitude, pressure = NULL,
                                        checkNthl = TRUE, checkIgnore = TRUE,
                                        checkPressure = FALSE, pressureMinimum = 20, pressureMaximum = 200){
  # do some initial checks to make sure appropriate paramters have been supplied
  if((missing(longitude) | missing(latitude)) & (checkNthl | checkIgnore)){
    stop('Must provide longitude and latitude.')
  }
  if(missing(pressure) & checkPressure){
    stop('Must provide pressure.')
  }

  # defined the various functions to call for varying combinations
  # of checkNthl, checkIgnore, and checkPressure

  #returns TRUE/FALSE vector, TRUE value indicates points are in the area to be ignored
  # so results of this fn should be negated if checkNthl = TRUE
  checkNthlPoints <- function(longitude, latitude){
    (latitude < 46.51 & longitude < -63.59) | (latitude > 46.59 & latitude < 46.61 & longitude < -64.09) # 57 points
  }
  # omit certain points that were not present in earlier years
  # returns TRUE/FALSE vector, TRUE values indicates that the points
  # are the points that should be ignored,
  # so when checkIgnore = TRUE, the results of this fn should be negated
  checkIgnorePoints <- function(longitude, latitude){
    (latitude == 46.0 & (longitude >= -63.5 & longitude <= -63.3)) | # 3 points
      (latitude == 47.0 & longitude == -60.4) | # 1 point
      (latitude == 47.8 & longitude == -65.2) | # 1 point
      (latitude == 48.1 & (longitude == -65.1 | longitude == -66.1)) | # 2 points
      (latitude == 48.8 & (longitude >= -64.4 & longitude <= -64.3)) # 2 points
  }
  # bottom depth must be between certain range
  checkBtmDepthPoints <- function(p, pmin, pmax){
    p[length(p)] >= pmin & p[length(p)] <= pmax
  }

  if(checkNthl){
    badNthl <- checkNthlPoints(longitude,latitude)
  }
  if(checkIgnore){
    badIgnore <- checkIgnorePoints(longitude, latitude)
  }
  if(checkPressure){
    okPressure <- checkPressure(p = pressure,
                                pmin = pressureMinimum,
                                pmax = pressureMaximum)
  }

  if(checkNthl & checkIgnore & checkPressure){
    !badNthl & !badIgnore & okPressure
  } else if (checkNthl & checkIgnore & !checkPressure){
    !badNthl & !badIgnore
  } else if(checkNthl & !checkIgnore & !checkPressure){
    !badNthl
  } else if(!checkNthl & checkIgnore & !checkPressure){
    !badIgnore
  } else if(!checkNthl & !checkIgnore & checkPressure){
    okPressure
  } else if(!checkNthl & checkIgnore & checkPressure){
    !badIgnore & okPressure
  } else if(checkNthl & !checkIgnore & checkPressure){
    !badNthl & okPressure
  }

}

#' @title Calculate monthly climatology
#'
#' @description This function calculates the monthly climatology
#'
#' @param d a data.frame containing at least a column named month and one other variable
#' @param climatologyYears a vector of length two indicating the range of years
#' to calculate the climatology
#'
#' @return the results of aggregate
#'
#' @author Chantelle Layton
#'
#' @importFrom stats aggregate
#'
#' @export

monthlyClimatology <- function(d, climatologyYears){
  okclim <- d$year >= climatologyYears[1] & d$year <= climatologyYears[2]
  dd <- d[okclim, ]
  mm <- aggregate(temperature ~ month, dd, mean, na.rm = TRUE)
  mm
}

#' @title Calculate monthly climatology standard deviation
#'
#' @description This function calculates the monthly climatology standard deviation.
#'
#' @param d a data.frame containing at least a column named month and one other variable
#' @param climatologyYears a vector of length two indicating the range of years
#'
#' @return the results of the aggregate function, a data.frame with columns `month` and `temperature`
#'
#' @details Monthly climatology standard deviation is calculated by taking the values within the
#' defined climatology range and finding the standard deviation for each month.
#'
#' @author Chantelle Layton
#'
#' @importFrom stats sd
#' @importFrom stats aggregate
#'
#' @export

monthlyStandardDeviation <- function(d, climatologyYears){
  okclim <- d$year >= climatologyYears[1] & d$year <= climatologyYears[2]
  dd <- d[okclim, ]
  mm <- aggregate(temperature ~ month, dd, sd, na.rm = TRUE)
  mm
}

#' @title Caclulate monthly anomaly values
#'
#' @description This function calculates the monthly anomaly.
#'
#' @param d a data.frame containing at least a column named year, month, and one other variable
#' @param climatologyYears a vector of length two indicating the range of years
#' to calculate the climatology.
#' @param normalizedAnomaly a logical value indicating whether or not to also calculated the
#' normalized anomaly. Default is `TRUE`.
#'
#' @return a data.frame containing columns `year`, `month`, `anomaly`, and `normalizedAnomaly`
#' if desired.
#'
#' @author Chantelle Layton
#'
#' @export
#'

monthlyAnomaly <- function(d, climatologyYears, normalizedAnomaly = TRUE){
  mm <- monthlyClimatology(d, climatologyYears)
  months <- 1:12
  mok <- lapply(months, function(k) which(d$month == k))
  anomaly <- vector(length = length(d$year))
  for (i in 1:length(months)){
    anomaly[mok[[i]]] <- d$temperature[mok[[i]]] - mm$temperature[i]
  }
  if(!normalizedAnomaly) {
    cbind(d, anomaly = anomaly)
  } else {
    cbind(monthlyNormalizedAnomaly(d, climatologyYears), anomaly = anomaly)
  }

}

#' @title Caclulate monthly normalized anomaly values
#'
#' @description This function calculates the monthly normalized anomaly.
#'
#' @param d a data.frame containing at least a column named year, month, and one other variable
#' @param climatologyYears a vector of length two indicating the range of years
#' to calculate the climatology.
#'
#' @return a data.frame containing columns `year`, `month`, and `normalizedAnomaly`
#'
#' @details To calculated the normalized anomaly, two climatology values are required, those being the
#' monthly and standard deviation. The normalized anomaly values are calculated
#' by subtracting the monthly climatology value from the monthly value, and the dividing it
#' by the monthly climatology standard deviation.
#'
#' @author Chantelle Layton
#'
#' @export
#'

monthlyNormalizedAnomaly <- function(d, climatologyYears){
  mm <- monthlyClimatology(d, climatologyYears)
  msd <- monthlyStandardDeviation(d, climatologyYears)
  months <- 1:12
  mok <- lapply(months, function(k) which(d$month == k))
  anomaly <- vector(length = length(d$year))
  for (i in 1:length(months)){
    anomaly[mok[[i]]] <- (d$temperature[mok[[i]]] - mm$temperature[i]) / msd$temperature[i]
  }
  cbind(d, normalizedAnomaly = anomaly)
}

#' @title Calculate annual anomaly values
#'
#' @description This function calculates annual anomaly values from provided
#' monthly anomaly values.
#'
#' @param d a data.frame containing year, month, and at least anomaly, and optionally normalizedAnomaly
#'
#' @return a data.frame with year, anomaly, and optionally normalizedAnomaly
#'
#' @author Chantelle Layton
#'
#' @importFrom stats aggregate
#'
#' @export

annualAnomaly <- function(d){
  aa <- aggregate(anomaly ~ year, d, mean, na.rm = TRUE)
  aa
}

#' @title Calculate normalized annual anomaly values
#'
#' @description This function calculates normalized annual anomaly values from provided
#' monthly anomaly values.
#'
#' @param d a data.frame containing year, month, and at least anomaly
#' @param climatologyYears a vector of length two indicating the range of years
#' to calculate the climatology.
#'
#' @return a data.frame with year, normalizedAnomaly, and other columns in d
#'
#' @author Chantelle Layton
#'
#' @importFrom stats aggregate
#'
#' @export

annualNormalizedAnomaly <- function(d, climatologyYears){
  okclim <- d$year >= climatologyYears[1] & d$year <= climatologyYears[2]
  dd <- d[okclim, ]
  sd <- sd(dd$anomaly, na.rm = TRUE)
  normanom <- d$anomaly / sd
  cbind(d, normalizedAnomaly = normanom)
}

#' @title Calculate monthly mean
#'
#' @description This function calculates the monthly mean values from data that has a temporal
#' resolution greater than a month.
#'
#' @param x the times of observations in POSIXct format.
#' @param y the observations
#' @param minN the minimum number of data points
#'
#' @return A list containing the resulting monthly mean values of x, y, the number of data points,
#' within that month, n.
#'
#' @author Chantelle Layton
#'
#' @export

monthlyMeanCutSplit <- function(x, y, minN = NULL){
  ys <- unname(unlist(lapply(split(y, cut(x, 'month')), mean)))
  xs <- as.POSIXct(unname(unlist(lapply(split(x, cut(x, 'month')), mean))),
                   origin = '1970-01-01',
                   tz = 'UTC')
  n <- unname(unlist(lapply(split(y, cut(x, 'month')), function(k) length(is.finite(k)))))
  if(!is.null(minN)){
    ok <- n >= minN
    df <- list(x = xs[ok],
               y = ys[ok],
               n = n[ok])
  } else {
    df <- list(x = xs,
               y = ys,
               n = n)
  }
  df
}

#' @title Calculate annual mean
#'
#' @description This function calculates the annual mean value from data that has a temporal
#' resolution greater than a year.
#'
#' @param x the times of observations in POSIXct format.
#' @param y the observations
#' @param minN the minimum number of data points
#'
#' @return A list containing the resulting annual mean value of x, y, the number of data points,
#' within that year, n.
#'
#' @author Chantelle Layton
#'
#' @export

annualMeanCutSplit <- function(x, y, minN = NULL){
  ys <- unname(unlist(lapply(split(y, cut(x, 'year')), mean)))
  xs <- as.POSIXct(unname(unlist(lapply(split(x, cut(x, 'year')), mean))),
                   origin = '1970-01-01',
                   tz = 'UTC')
  n <- unname(unlist(lapply(split(y, cut(x, 'year')), function(k) length(is.finite(k)))))
  if(!is.null(minN)){
    ok <- n >= minN
    df <- list(x = xs[ok],
               y = ys[ok],
               n = n[ok])
  } else {
    df <- list(x = xs,
               y = ys,
               n = n)
  }
  df
}

#' @title Get transect angle
#'
#' @details This function calculates a neutral regression on
#' geographical coordinates and outputs the resulting angle from
#' the regression.
#'
#' @param longitude vector of longitudes
#' @param latitude vector of latitudes
#'
#' @return The angle from 0 degrees pointing east, as well as the neutral regression
#'         output.
#' @author Chantelle Layton
#'
#' @export
#' @importFrom smatr sma
#'

getTransectAngle <- function(longitude, latitude){
  zone <- lonlat2utm(longitude = longitude[1],
                     latitude = latitude[1])$zone
  xy <- lonlat2utm(longitude = longitude,
                   latitude = latitude,
                   zone = zone)
  {if(length(longitude) > 2){
    m <- sma(xy$northing ~ xy$easting)
    a <- atan(m$coef[[1]][2,1]) * 180 / pi
  }
    else{
      m <- lm(xy$northing ~ xy$easting)
      a <- atan(unname(coef(m)[2])) * 180 / pi
    }
  }
  angle <- a
  return(list(angle = angle, m = m))
}

#' @title Bin-average a CTD object in pressure space.
#'
#' @description Vertically average and CTD object through pressure by defined bins and tolerances.
#'
#' @param x a ctd object
#' @param bin a vector of numerical values of the center of the bin
#' @param tolerance a vector of numerical values of the tolerance for the bin
#' @param trimBin a logical value indicating if the supplied bin values should be trimmed to the
#' maximum pressure, default is TRUE.
#'
#' @return a ctd object that has the same metadata and processing log as the supplied ctd object,
#' but with bin-averaged data.
#'
#' @author Chantelle Layton
#' @importFrom methods new
#'
#' @export

binMeanPressureCtd <- function(x, bin, tolerance, trimBin = TRUE){
  # set up new ctd object
  res <- new("ctd")
  # add previous metadata and processing log
  res@metadata <- x@metadata
  res@processingLog <- x@processingLog
  # vertically average the data
  # omitting pressure, we'll get that from one of the
  pok <- names(x@data) %in% 'pressure'
  pressure <- x@data[pok][[1]]
  if(trimBin) {
    ok <- bin <= max(pressure)
    bin <- bin[ok]
    tolerance <- tolerance[ok]
  }
  for (i in seq_along(x@data)[!pok]) {
    data <- x@data[[i]]
    res@data[[i]] <- mapply(function(bin, tolerance) mean(data[pressure >= (bin - tolerance) & pressure < (bin + tolerance)], na.rm = TRUE),
                            bin,
                            tolerance)
  }
  res@data[[which(pok)]] <- bin #+ tolerance # check this, not sure when I added + tolerance
  names(res@data) <- names(x@data)
  # set some 'metadata' things in 'data' if they are na
  # assumes that if time is na, then longitude and latitude will be as well
  if(is.na(res[['time']][1])){
    res@data$time[1] <- res@data$time[2]
    res@data$longitude[1] <- res@data$longitude[2]
    res@data$latitude[1] <- res@data$latitude[2]
  }
  res
}

#' @title Average a list of CTD objects.
#'
#' @description Average and list of CTD objects. This function will take all available data within all of the ctd objects
#' and will average in pressure space. It takes all unique pressure values from all ctd objects, and then averages
#' all availble data for each unique pressure values. Therefore, it's best if the data have already been vertically averaged.
#'
#' @param x a list of ctd objects
#' @param bin an optional vector of numerical values of the center of the bin to average the data
#' @param tolerance an optional vector of numerical values of the tolerance for the bin
#'
#' @details There are three different schemes for averaging based on which combination of `bin` and `tolerance`
#' is supplied. If `bin = tolerance = NULL`, then profiles are averaged by unique pressure values in all
#' of the ctd profiles. If `bin` and `tolerance` are supplied, then the data is averaged using the supplied
#' values such that intervals are closed on the left and open on the right. If `bin` is supplied and `tolerance = NULL`
#' then the data is averaged by combining `cut()` and `split()`, and for consistency to other schemes the intervals
#' are closed on the left and open on the right.
#'
#' @return a ctd object that has the same metadata and processing log as the supplied ctd object,
#' but with bin-averaged data.
#'
#' @author Chantelle Layton
#' @importFrom methods new
#'
#' @export

averageCtds <- function(x, bin = NULL, tolerance = NULL){
  # set up new ctd object
  res <- new("ctd")
  # add previous metadata and processing log
  res@metadata <- x[[1]]@metadata
  res@processingLog <- x[[1]]@processingLog
  res[['startTime']] <- as.POSIXct(mean(unlist(lapply(x, function(k) k[['startTime']]))), origin = '1970-01-01', tz = 'UTC')
  udatanames <- unique(unlist(lapply(x, function(k) names(k@data))))
  # get all the data out of all the ctd objects
  df <- do.call('rbind', lapply(x, function(k) {alld <- lapply(udatanames, function(kk) if(kk %in% names(k@data)) k[[kk]] else rep(NA, length(k[['pressure']])));
  m <- do.call('cbind', alld);
  colnames(m) <- udatanames;
  as.data.frame(m)}))
  # profiles probably have already been vertically averaged
  if(is.null(bin) & is.null(tolerance)){
    upressure <- unique(df[['pressure']])
    dfavg <- as.data.frame(do.call('rbind', lapply(upressure, function(k) {ok <- df[['pressure']] %in% k;
                                                                           dfsub <- df[ok, ];
                                                                           apply(dfsub, 2, mean, na.rm = TRUE)})))
    dfavg <- data.frame(pressure = upressure,
                        dfavg)

  }
  # use cut split method with right = FALSE, meaning left closed (included) right open (excluded)
  if(!is.null(bin) & is.null(tolerance)){
    notpressure <- !names(df) %in% 'pressure'
    pressure <- df[,!notpressure]
    dfavg <- apply(df[,notpressure], 2, function(k) unlist(lapply(split(k, cut(pressure, bin, right = FALSE)), mean, na.rm = TRUE)))
    dfn <- apply(df[,notpressure], 2, function(k) unlist(lapply(split(k, cut(pressure, bin, right = FALSE)), function(x) length(which(!is.na(x))))))
    dfavg <- data.frame(pressure = bin[2:length(bin)] - (diff(bin)/2),
                        dfavg)
  }
  # bin the data
  if(!is.null(bin) & !is.null(tolerance)){
    dfavg <- mapply(function(bin, tolerance) {ok <- df[['pressure']] >= (bin - tolerance) & df[['pressure']] < (bin + tolerance);
                                              dfsub <- df[ok, ];
                                              apply(dfsub, 2, mean, na.rm = TRUE)},
                            bin,
                            tolerance)
    dfavg <- as.data.frame(t(dfavg))
    dfavg[['pressure']] <- bin
  }

  # remove time and scan because it means nothing
  if(any(c('scan', 'time') %in% names(dfavg))){
    ok <- !names(dfavg) %in% c('scan', 'time')
    dfavg <- dfavg[, ok]
  }
  res@data <- as.list(dfavg)
  res
}

#' @title Find bottom depth index for given list of longitude, latitude, and depth
#'
#' @description This function takes in unlisted longitude, latitude, and depth values and finds
#' the index at which the maximum depth for each unique longitude,latitude pair occurs
#'
#' @param longitude a numeric vector of longitudes
#' @param latitude a numeric vector of latitudes
#' @param depth a numeric vector of depths
#'
#' @return a logical vector of TRUE/FALSE, TRUE indicating the bottom depth value for the unique
#' longitude,latitude pair
#' @author Chantelle Layton
#' @export

findBottomDepthIndex <- function(longitude, latitude, depth){
  # put in a length check ?
  ulonlat <- unique(data.frame(longitude = longitude,
                               latitude = latitude))
  idx <- vector(mode = 'logical', length = length(longitude))
  for (i in 1:dim(ulonlat)[1]){
    lonlook <- ulonlat[['longitude']][i]
    latlook <- ulonlat[['latitude']][i]
    oklonlat <- which(longitude == lonlook & latitude == latlook)
    depthlook <- max(depth[oklonlat])
    idx[oklonlat] <- FALSE
    oklonlatdepth <- which(longitude == lonlook & latitude == latlook & depth == depthlook)
    idx[oklonlatdepth] <- TRUE
  }
  idx
}

