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
#' @return a list with thickness with units kilometer, volume with units kilometer^3,
#' and minimum temperature.
#' @export
#'
cilVolume <- function(T, p, longitude, latitude, dlongitude, dlatitude, Tlim = 4){
  thickness <- NULL
  idx <- NULL
  for (i in 1:length(p)){
    if(!is.na(T[i])){
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
#' @description This function calculates the monthly climatology, along with the standard deviation
#'
#' @param d a data.frame containing at least a column named month and one other variable
#' @param climatologyYears a vector of length two indicating the range of years
#' to calculate the climatology
#' @param standardDeviation a logical vector indicating if (`TRUE`) the standard deviation should be calculated.
#'
#' @return the results of aggregate
#'
#' @author Chantelle Layton
#'
#' @importFrom stats aggregate
#'
#' @export

monthlyClimatology <- function(d, climatologyYears, standardDeviation = FALSE){
  okclim <- d$year >= climatologyYears[1] & d$year <= climatologyYears[2]
  dd <- d[okclim, ]
  mm <- aggregate(temperature ~ month, dd, mean, na.rm = TRUE)
  if(standardDeviation){
    mmsd <- aggregate(temperature ~ month, dd, sd, na.rm = TRUE)
    mm <- data.frame(mm,
                     temperatureSd = mmsd[['temperature']])
  }
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
#' @param minNMonths an numeric value indicating the minimum number of months required to calculate an
#' annual value for a given year. The default value is set to 12.
#'
#' @return a data.frame with year, anomaly, and optionally normalizedAnomaly
#'
#' @author Chantelle Layton
#'
#' @importFrom stats aggregate
#'
#' @export

annualAnomaly <- function(d, minNMonths = 12){
  # remove any character columns
  # most likely flag columns
  dclass <- sapply(d, class)
  d <- d[, !dclass %in% 'character']
  ds <- split(d, d[['year']])
  dout <- vector(mode = 'list', length = length(ds))
  for(i in 1:length(ds)){
    dd <- ds[[i]]
    # check that there are at least minimum number of values present for temperature
    if(length(which(!is.na(dd[['temperature']]))) > minNMonths){ # enough data
      dsave <- apply(dd, 2, mean, na.rm = TRUE)
      # remove the month column
      dout[[i]] <- dsave[names(dsave) != 'month']
    } else { # not enough data
      dsave <- c(as.numeric(names(ds)[i]), rep(NA, length(names(dd)[!names(dd) %in% c('year', 'month')])))
      names(dsave) <- c('year', names(dd)[!names(dd) %in% c('year', 'month')])
      dout[[i]] <- dsave
    }
  }
  dfout <- as.data.frame(do.call('rbind', dout))
  dfout
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
#' @param method a character vector indicating the type of averaging to do. Ones that are currently
#' available include `'mean'` and `'triangleWeight`. `'mean'` is the default.
#'
#' @return a ctd object that has the same metadata and processing log as the supplied ctd object,
#' but with bin-averaged data.
#'
#' @author Chantelle Layton
#' @importFrom methods new
#'
#' @export

binMeanPressureCtd <- function(x, bin, tolerance, trimBin = TRUE, method = 'mean'){
  # for triangleWeight method
  triangle <- function(n){
    # check if even or odd
    N <- n
    n <- seq(-floor(N/2), floor(N/2), 1)
    wn <- 1 - (abs(n) / N)
    # return warning message that length of wn will be 1 more if n is even
    wn
  }
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
    ok <- bin <= max(pressure, na.rm = TRUE)
    bin <- bin[ok]
    tolerance <- tolerance[ok]
  }
  for (i in seq_along(x@data)[!pok]) {
      data <- x@data[[i]]
      if(names(x@data)[i] == 'time'){
        res@data[[i]] <- mapply(function(bin, tolerance) mean(data[pressure >= (bin - tolerance) & pressure < (bin + tolerance)], na.rm = TRUE),
                                bin,
                                tolerance)
      } else {
          if(method == 'mean'){
             res@data[[i]] <- mapply(function(bin, tolerance) mean(data[pressure >= (bin - tolerance) & pressure < (bin + tolerance)], na.rm = TRUE),
                              bin,
                              tolerance)
        }
          if(method == 'triangleWeight'){
          res@data[[i]] <- mapply(function(bin, tolerance) {lookbin <- (bin - floor(tolerance/2)):(bin + ceiling(tolerance/2));
                                                            # get triangle weight
                                                            weight <- triangle(n = length(lookbin));
                                                            dataidx <- unlist(lapply(lookbin, function(k) which(pressure == k))); # not sure what will happen if there is no match
                                                            weightidx <- unlist(lapply(pressure, function(k) which(lookbin == k)));
                                                            x <- data[dataidx];
                                                            w <- weight[weightidx];
                                                            ok <- !is.na(x);
                                                            if(all(!ok)) {NA} else {sum(x[ok] * w[ok], na.rm = TRUE) / sum(w[ok], na.rm = TRUE)}},
                                                            #if(all(is.na(data[dataidx]))){NA} else {sum(data[dataidx] * weight[weightidx], na.rm = TRUE) / sum(weight[weightidx], na.rm = TRUE)}},
                                  bin,
                                  tolerance)
            # for debugging
          # newd <- vector(mode = 'logical', length = length(bin))
          # for(ib in 1:length(bin)){
          #   lookbin <- (bin[ib] - floor(tolerance[ib]/2)):(bin[ib] + ceiling(tolerance[ib]/2))
          #   # get triangle weight
          #   weight <- triangle(n = length(lookbin))
          #   dataidx <- unlist(lapply(lookbin, function(k) which(pressure == k))) # not sure what will happen if there is no match
          #   weightidx <- unlist(lapply(pressure, function(k) which(lookbin == k)))
          #   xd <- data[dataidx]
          #   w <- weight[weightidx]
          #   ok <- !is.na(xd)
          #   newd[ib] <- if(all(!ok)) {NA} else {sum(xd[ok] * w[ok], na.rm = TRUE) / sum(w[ok], na.rm = TRUE)}
          # }
          # res@data[[i]] <- newd
        }
      }

    }
  res@data[[which(pok)]] <- bin #+ tolerance # check this, not sure when I added + tolerance

  names(res@data) <- names(x@data)
  # set some 'metadata' things in 'data' if they are na
  # assumes that if time is na, then longitude and latitude will be as well
  if(!is.null(res[['time']])){
    if(is.na(res[['time']][1])){
      res@data$time[1] <- res@data$time[2]
      res@data$longitude[1] <- res@data$longitude[2]
      res@data$latitude[1] <- res@data$latitude[2]
    }
  }
  res
}

#' @title Linearly interpolate a CTD object in pressure space.
#'
#' @description Vertically average and CTD object through pressure by defined bins and tolerances.
#'
#' @param x a ctd object
#' @param bin a vector of numerical values of the center of the bin
#' @param trimBin a logical value indicating if the supplied bin values should be trimmed to the
#' maximum pressure, default is FALSE.
#'
#' @return a ctd object that has the same metadata and processing log as the supplied ctd object,
#' but with bin-averaged data.
#'
#' @author Chantelle Layton
#' @importFrom methods new
#' @importFrom stats approx
#'
#' @export
approxPressureCtd <- function(x, bin, trimBin = FALSE){
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
    res@data[[i]] <- approx(x = pressure,
                            y = data,
                            xout = bin)$y
  }
  res@data[[which(pok)]] <- bin #+ tolerance # check this, not sure when I added + tolerance
  names(res@data) <- names(x@data)
  # set some 'metadata' things in 'data' if they are na
  # assumes that if time is na, then longitude and latitude will be as well
  if(!is.null(res[['time']])){
    if(is.na(res[['time']][1])){
      res@data$time[1] <- res@data$time[2]
      res@data$longitude[1] <- res@data$longitude[2]
      res@data$latitude[1] <- res@data$latitude[2]
    }
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

#' @title Shelf survey barnes interpolation method
#'
#' @description This function applies the appropriate methodology decided on for interpolating shelf survey
#' CTD measurements that has been tailored to the scotian shelf.
#'
#' @param ctd a list of [ctd-class] objects
#' @param fullgrid a data.frame containing the full original grid which contains columns, `x` which is the longitude,
#' `y` which is the latitude, `depth`, and `isBottom` which indicates if the grid point is the bottom depth.
#' @param grid a data.frame containing the grid points that data should be interpolated. This includes columns
#' `y` which is the latitude, `x`, which is the longitude, `depth`, a positive value, `tolerance`, a positive value
#' indicating a depth range used to get an average value at the given depth, `isBottom`, an optional column
#' indicating if the grid point is a bottom depth point.
#' @param variable a character value specifying which variable to extract from the `ctd` profiles.
#' @param xg,yg vectors defining x and y grids, see [interpBarnes] for additional clarification.
#' @param xr,yr lengths of the x and y grids for barnes interpolation, see [interpBarnes] for additional clarification.
#' @param iterations number of iterations for barnes interpolation, see [interpBarnes] for additional clarification.
#' @param gamma grid-focussing parameter for barnes interpolation, see [interpBarnes] for additional clarification.
#' @param averagingMethod a character vector of length one indicating how to vertically average the data to input
#' into the barnes interpolation. Current options include `mean` and `triangleWeight`.
#' @param trimOutput a logical value indicating if the barnes output should trim the grid output based on the input
#' data. If `TRUE`, any grid points which don't have any data within the value supplied to `trimDistance`.
#' @param trimDistance a numeric value indicating the distance, in kilometers, where barnes interpolation
#' output will be deemed valid if supplied data falls within the given distance. If not, the barnes output
#' at that grid point will be assigned a value of `NA`.
#' @param debug a logical value indicating if some debug messages should be displayed.
#'
#' @return a data.frame of the barnes interpolation output at each grid point defined in full grid, if the given
#' depth value and or grid points are specified in grid. The data.frame includes `x` the longitude, `y` the latitude,
#' `depth` the depth, and `data` the barnes output of the smoothed interpolated data for the given variable.
#'
#' @author Chantelle Layton
#' @export
#'
#' @importFrom oce interpBarnes
#' @importFrom oce lonlat2utm
#' @importFrom oce geodDist

shelfSurveyInterpBarnes <- function(ctd, fullgrid, grid, variable, xg, yg, xr, yr, iterations, gamma, averagingMethod, trimOutput, trimDistance, debug = FALSE) {
  # add some checks to see if parameters are supplied

  # functions that don't need to be their own ... yet ...
  # for vertical weighted averaging
  triangle <- function(n){
    # check if even or odd
    N <- n
    n <- seq(-floor(N/2), floor(N/2), 1)
    wn <- 1 - (abs(n) / N)
    # return warning message that length of wn will be 1 more if n is even
    wn
  }

  # essentially code from AZMPreporting/climatology/summerGroundfish/17_interparnes1981to2010climatology.R
  catpts <- seq(0, dim(grid)[1], 50) # for debug and development purposes
  expgrid <- expand.grid(x = xg, y = yg)
  barnesd <- NULL
  for (id in 1:dim(grid)[1]){
    if(id %in% catpts){
      cat(paste('Doing barnes interp for pt', id ,'/', dim(grid)[1]), sep = '\n')
    }
    griddfsub <- grid[id, ]
    lookdepth <- griddfsub[['depth']]
    looktolerance <- griddfsub[['tolerance']]

    # subset the grid to either the depth or the bottom point
    if(griddfsub[['isBottom']]){
      subgrid <- griddfsub
      gridmask <- TRUE
    } else {
      ok <- fullgrid[['depth']] == lookdepth
      subgrid <- fullgrid[ok, ]
      if(dim(subgrid)[1]  == 0){
        if(debug) cat(paste('No points in the grid found for depth', griddfsub[['depth']], ', moving on to next depth'), sep = '\n')
        next
      }
      gridmask <- apply(expgrid, 1, function(k) {any(subgrid[['x']] %in% k[['x']] & subgrid[['y']] %in% k[['y']])})
    }

    # prep the data
    alllookdepth <- (lookdepth - floor(looktolerance/2)):(lookdepth + ceiling(looktolerance/2))
    # get triangle weight
    if(averagingMethod == 'triangleWeight'){weight <- triangle(n = length(alllookdepth))}
    x <- y <- z <- NULL
    xlon <- ylat <- NULL
    for (i in 1:length(ctd)){
      # 1. subset the data
      dd <- ctd[[i]]
      pressure <- dd[['pressure']]
      okp <- pressure %in% alllookdepth
      #ddsub <- subset(dd, pressure %in% alllookdepth)
      #if(length(ddsub[[variable]]) == 0){
      if(all(!okp)){
        next
      } else{
        # 1. get x and y
        # utm <- lonlat2utm(longitude = ddsub[['longitude']][1], latitude = ddsub[['latitude']][1], zone = zone, km = TRUE)
        # x <- c(x, utm$easting)
        # y <- c(y, utm$northing)

        #xlon <- c(xlon, ddsub[['longitude']][1])
        #ylat <- c(ylat, ddsub[['latitude']][1])

        xlon <- c(xlon, dd[['longitude']][1])
        ylat <- c(ylat, dd[['latitude']][1])
        # 2. get z
        #data <- ddsub[[variable]]
        #datapressure <- ddsub[['pressure']]
        data <- dd[[variable]][okp]
        datapressure <- dd[['pressure']][okp]

        if(averagingMethod == 'triangleWeight'){
          # match up datapressure with alllookdepth
          dataidx <- unlist(lapply(alllookdepth, function(k) which(datapressure == k))) # not sure what will happen if there is no match
          weightidx <- unlist(lapply(datapressure, function(k) which(alllookdepth == k)))
          x <- data[dataidx]
          w <- weight[weightidx]
          ok <- !is.na(x)
          wa <- if(all(!ok)) {NA} else {sum(x[ok] * w[ok], na.rm = TRUE) / sum(w[ok], na.rm = TRUE)}
        }
        if(averagingMethod == 'mean'){
          wa <- mean(data[dataidx], na.rm = TRUE)
        }
        z <- c(z, wa)
      }
    }
    # do barnes interpolation
    # if there is no data to input, spit out a NA data frame
    if(is.null(z)){
      if(griddfsub[['isBottom']]){
        if(debug) cat(paste('No data points for bottom depth', lookdepth, ' m.'), sep = '\n')
      } else {
        if(debug) cat(paste('No data points for depth', lookdepth, ' m.'), sep = '\n')
      }

      if(griddfsub[['isBottom']]){
        #cat(paste('Appending bottom depth pt, length of ib$xg is', length(ib$xg)), sep = '\n')
        if(debug) cat(paste('Appending NA bottom depth pt'), sep = '\n')
        barnesdadd <- data.frame(#x = ib$xg,
                                 #y = ib$yg,
                                 x = griddfsub[['x']],
                                 y = griddfsub[['y']],
                                 depth = lookdepth,
                                 data = NA,
                                 isBottom = griddfsub[['isBottom']])
      }
      else {
        barnesdadd <- data.frame(x = expgrid$x[gridmask],
                                 y = expgrid$y[gridmask],
                                 depth = rep(lookdepth, times = length(expgrid$x[gridmask])),
                                 data = NA,
                                 isBottom = griddfsub[['isBottom']])
      }
      if(trimOutput){
        barnesdadd <- data.frame(barnesdadd,
                                 dataTrimmed = NA)
      }
    } else {
    # do barnes based on either entire grid or just bottom point
    if(griddfsub[['isBottom']]){
      ib <- interpBarnes(x = xlon, y = ylat, z = z,
                         xg = griddfsub[['x']], yg = griddfsub[['y']],
                         xr = xr, yr = yr,
                         gamma = gamma,
                         iterations = iterations
                         #trim = 0.2
                         #debug = 33
      )
    } else {
      ib <- interpBarnes(x = xlon, y = ylat, z = z,
                         xg = xg, yg = yg,
                         xr = xr, yr = yr,
                         gamma = gamma,
                         iterations = iterations
                         #trim = 0.2
                         #debug = 33
      )
    }

    # NA out any barnes output where there isn't a data point within xr,yr radius
    #   note that i'll use Rdist here for simplicity
    if(trimOutput){
      blon <- ib[['xg']]
      blat <- ib[['yg']]
      zgNew <- matrix(data = NA, nrow = dim(ib[['zg']])[1], ncol = dim(ib[['zg']])[2])
      for(ilon in 1:length(blon)){
        lonlook <- blon[ilon]
        for(ilat in 1:length(blat)){
          latlook <- blat[ilat]
          distToGridPt <- geodDist(longitude2 = lonlook,
                                   latitude2 = latlook,
                                   longitude1 = xlon,
                                   latitude1 = ylat)
          # zgNew already filled with NA so no need to do else case
          if(any(distToGridPt < trimDistance)){
            zgNew[ilon, ilat] <- ib[['zg']][ilon, ilat]
          }
        }
      }
      ib[['zgTrimmed']] <- zgNew
    }

    # save some things from interpBarnes
    # want only the grid values, not the entire expanded grid
      if(griddfsub[['isBottom']]){
        if(debug) cat(paste('Appending bottom depth pt, length of ib$xg is', length(ib$xg)), sep = '\n')
        barnesdadd <- data.frame(x = ib$xg,
                                 y = ib$yg,
                                 depth = lookdepth,
                                 data = ib$zg,
                                 isBottom = griddfsub[['isBottom']])
      }
      else {
            barnesdadd <- data.frame(x = expgrid$x[gridmask],
                             y = expgrid$y[gridmask],
                             depth = rep(lookdepth, times = length(expgrid$x[gridmask])),
                             data = ib$zg[gridmask],
                             isBottom = griddfsub[['isBottom']])
      }

    if(trimOutput){
      barnesdadd <- data.frame(barnesdadd,
                               dataTrimmed = ib$zgTrimmed[gridmask])
    }
    } # closes is.na(z)
    dim1 <- if(is.null(barnesd)) 0 else dim(barnesd)[1]
    if(debug) cat(dim1, sep = '\n')
    if(is.null(barnesd)){
      barnesd <- barnesdadd
    } else {
      barnesd <- rbind(barnesd,
                       barnesdadd)
    }
    if(debug) dim2 <- dim(barnesd)[1]
    if(debug) cat(paste('Added', dim2 - dim1, 'points'), sep = '\n')
    #rmsezd <- rmse(z[!is.na(z)], ib$zd)
  } # closes grid for loop
  barnesd
}

#' @title Calculate mixed layer depth
#'
#' @description This function calculates the mixed layer depth using one
#' of three methods implemented. Methods include the `gradient`, `N2`, and `density`.
#'
#' @param ctd a `ctd` object.
#' @param method a character string indicting which method to
#'               use to calculate the mixed layer depth, options include
#'               'gradient', 'N2', and 'density'. Default is 'gradient'.
#' @param mldMin a numeric value indicating the minimum value the
#'               mixed layer depth should be. Default is set to `NULL`.
#'               Ignored for `method = 'density'`.
#' @param nPointsMin a numeric value indicating the minimum number of
#'                   points in ctd object
#' @param densityThreshold a numeric value indicating the threshold value
#'                         to use for the `method = density`, ignored for other methods.
#'                         Default is 0.03.
#' @param densityReferenceDepth a numeric value indicating the reference
#'                              depth for the `method = density` . It will find the closest
#'                              value in the profile within 5dbar. Default is 10dbar.
#' @param debug a logical value indicating to show debug information
#' @return a single numeric value of the calculated mixed layer depth
#'
#' @importFrom oce plotProfile
#' @importFrom pracma interp1
#'
#' @author Chantelle Layton and Benoit Casault
#' @export
#'

calculateMixedLayerDepth <- function(ctd,
                                     method = 'gradient',
                                     mldMin = NULL,
                                     nPointsMin = 3,
                                     densityThreshold = 0.03,
                                     densityReferenceDepth = 10,
                                     debug = TRUE){
  if(!'salinity' %in% names(ctd@data)){
    message('No salinity data')
    # add fake salinity for debug
    ctd <- oceSetData(object=ctd,
                      name='salinity',
                      value=rep(NA, length(ctd[['pressure']])))
    mldDefault <- NA
    mldForced <- NA
    referenceDepth <- densityReferenceDepth
    referenceDensity <- NA
  } else if(length(which(!is.na(ctd[['temperature']]))) <= nPointsMin | length(which(!is.na(ctd[['salinity']]))) <= nPointsMin){
    message(paste('Less than', nPointsMin, 'points in ctd object'))
    mldDefault <- NA
    mldForced <- NA
    referenceDepth <- densityReferenceDepth
    referenceDensity <- NA
  } else { # MLD can be calculated
    if(method == 'gradient'){
      # calculate density gradient
      nz <- length(ctd[['pressure']])
      dz <- diff(ctd[['pressure']])
      zm <- ctd[['pressure']][1:(nz - 1)] + (dz/2)
      vargrad <- 100 * diff(ctd[['sigmaTheta']])/dz
      # default calculation
      gradCheck <- any(vargrad >= 1, na.rm = TRUE) # to prevent infinite warnings
      if(!gradCheck){ # no values
        if(all(is.na(vargrad))){
          mldDefault <- NA
        } else {
          # if none, set mixed layer depth to max pressure
          mldDefault <- max(ctd[['pressure']], na.rm = TRUE)
        }
      } else {
        iMld <- min(which(vargrad >= 1))
        mldDefault <- (zm[iMld]+zm[iMld+1]) / 2
      }
      # forced calculation (below mldMin)
      mldForced <- as.numeric(NA)
      if(!is.null(mldMin)){
        gradCheck <- any(vargrad >= 1 & zm > mldMin, na.rm = TRUE) # to prevent infinite warnings
        if(!gradCheck){ # no values
          if(all(is.na(vargrad))){
            mldForced <- NA
          } else {
            # if none, set mixed layer depth to max pressure
            mldForced <- max(ctd[['pressure']], na.rm = TRUE)
          }
        } else {
          iMld <- min(which(vargrad >= 1 & zm > mldMin))
          mldForced <- (zm[iMld]+zm[iMld+1]) / 2
        }
        #df_results$mld <- df_results$mld_forced
      }
    } # closes method == 'gradient'
    if(method == 'N2'){
      # calculate N^2
      # equation is N^2 = -g/rho0 * drho / dz
      # we want to smooth drho, so we'll do 5m average, +/- 2.5m
      runningMeanN2 <- function(ctd){
        tol <- 2.5
        # define parameters
        g <- 9.8 #m/s^2
        rho0 <- mean(ctd[['density']], na.rm = TRUE)
        # get data out of ctd object
        rho <- rev(ctd[['sigmaTheta']])
        z <- rev(ctd[['pressure']]) * -1
        ## when doing calculation on binned bottle data
        ## there can be 'NA' values at multiple depth bins
        ## subset data where there are not NA values in rho
        notNA <- !is.na(rho)
        rho <- rho[notNA]
        z <- z[notNA]
        # calculate derivative
        drho <- diff(rho)
        dz <- diff(z)
        # smooth drho
        drhoSmooth <- vector(mode = 'logical', length = length(drho))
        for(i in 1:length(drho)){
          Z <- z[(i+1)] # use the i+1 value for z since diff decreases length by 1.
          lower <- Z - tol
          upper <- Z + tol
          ok <- z > lower & z <= upper
          if(all(!ok)){ # I don't think this will ever happen, but for safety
            drhoSmooth[i] <- NA
          } else {
            drhoSmooth[i] <- mean(drho[ok], na.rm = TRUE)
          }
        }
        N2 <- (-1) * (g / rho0) * (drhoSmooth/ dz)
        N2[is.infinite(N2)] <- NA
        return(list(z = z, N2 = N2))
      }
      N2 <- runningMeanN2(ctd)
      okN2 <- which.max(N2$N2)
      mldDefault <- abs(N2$z[(okN2 + 1)])
      #okN2 <- which.max(ctd[['N2']])
      #mldDefault <- ctd[['pressure']][okN2]
      mldForced <- as.numeric(NA)
      if(!is.null(mldMin)){
        ctdsub <- oce::subset(ctd, pressure > mldMin)
        N2sub <- runningMeanN2(ctdsub)
        okN2sub <- which.max(N2sub$N2)
        if(length(okN2sub) == 0){
          mldForced <- NA
        } else {
          mldForced <- abs(N2sub$z[(okN2sub + 1)])
        }
        # okN2sub <- which.max(ctdsub[['N2']])
        # mldForced <- ctdsub[['pressure']][okN2sub]
      }
    } # closes if method == 'N2'
    if(method == 'density'){
      idxrd <- which.min(abs(ctd[['pressure']] - densityReferenceDepth))
      # check that it is within 5dbar of referenceDepth
      okidx <- ctd[['pressure']][idxrd] >= (densityReferenceDepth - 5) & ctd[['pressure']][idxrd] <= (densityReferenceDepth + 5)
      if(!okidx) idxrd <- NULL
      if(debug & okidx) cat(paste('Density reference depth :', ctd[['pressure']][idxrd]), sep = '\n')
      if(length(idxrd) != 0){
        densityLook <- ctd[['sigmaTheta']][idxrd] + densityThreshold
        idxmld <- which(ctd[['sigmaTheta']][idxrd:length(ctd[['pressure']])] > densityLook)[1] + (idxrd - 1) # only interested in the first instance, and it has to be below the referenceDepth
        if(length(idxmld) != 0 & !is.na(idxmld)){
          #mldDefault <- ctd[['pressure']][idxmld]
          # need to get index that is not NA before the mld index
          idxsmldprev <- which(!is.na(ctd[['sigmaTheta']][1:(idxmld-1)]))
          idxmldprev <- idxsmldprev[length(idxsmldprev)]
          mldDefault <- pracma::interp1(x = ctd[['sigmaTheta']][c(idxmldprev,idxmld)],
                                y = ctd[['pressure']][c(idxmldprev,idxmld)],
                                xi = densityLook)
          referenceDepth <- ctd[['pressure']][idxrd]
          referenceDensity <- densityLook
        } else {
          cat(paste0("No points in profile exceed density look value of ", densityLook), sep = '\n')
          mldDefault <- NA
          referenceDepth <- densityReferenceDepth
          referenceDensity <- NA
        }
      } else { # no data at 10dbar/m
        cat("No data at 10dbar", sep = '\n')
        mldDefault <- NA
        referenceDepth <- densityReferenceDepth
        referenceDensity <- NA
      }
      # note that since it take density value at 10m, no mldForced for this method
      mldForced <- NA
    } # closes if method == 'density'
  } # closes else statement for MLD to be calculated
  # debug plot
  # taken out of else statement to see data that failed initial checks (not enough points and no salinity)
  if(debug){
    par(mfrow = c(1,2))
    # all combinations
    if(all(is.na(ctd[['salinity']]) & is.na(ctd[['temperature']]))){ # NA in both S and T data slots
      # add fake temperature data
      ctd <- oceSetData(ctd,
                        name = 'temperature',
                        value = 1:length(ctd[['pressure']]) # so we don't get a xlim error
      )
      # add fake salinity data
      ctd <- oceSetData(ctd,
                        name = 'salinity',
                        value = 1:length(ctd[['pressure']]) # so we don't get a xlim error
      )
      # set col to white
      col <- 'white'
      densityCol <- 'white'
      # set xtype to just temperature
      xtype <- 'temperature'
      hasNoData <- TRUE
      cat(xtype, sep = '\n')
    } else if (all(is.na(ctd[['salinity']])) & any(!is.na(ctd[['temperature']]))){ # S is NA, T is not all NA
      # add fake salinity data
      ctd <- oceSetData(ctd,
                        name = 'salinity',
                        value = 1:length(ctd[['pressure']]) # so we don't get a xlim error
      )
      xtype <- 'temperature'
      col <- 'black' # this is default, but for completeness of NA for both T and S
      densityCol <- 'white'
      hasNoData <- FALSE
      cat(xtype, sep = '\n')
    } else if(any(!is.na(ctd[['salinity']])) & all(is.na(ctd[['temperature']]))){ # S is not all NA, T is all NA
      # add fake temperature data
      ctd <- oceSetData(ctd,
                        name = 'temperature',
                        value = 1:length(ctd[['pressure']]) # so we don't get a xlim error
      )
      xtype <- 'salinity'
      col <- 'black'
      densityCol <- 'white'
      hasNoData <- FALSE
      cat(xtype, sep = '\n')
    } else { # ideal, both S and T data
      xtype <- 'salinity+temperature' # the default
      col <- 'black' # it will be ignored for this case, keeping for completeness
      densityCol <- 'black'
      hasNoData <- FALSE
      cat(xtype, sep = '\n')
    }
    oce::plotProfile(ctd, xtype = xtype, type = 'o', col = col, mar = c(3.0, 3.5, 3.5, 2.0))
    if(hasNoData){
      text(x = mean(ctd[['temperature']]),
           y = mean(ctd[['pressure']]),
           labels = 'No valid data')
    }
    if(method == 'gradient'){
      # have to fake zm and grad if they don't exist
      if(!exists('zm')){
        zm <- ctd[['pressure']]
      }
      if(!exists('vargrad')){
        # make it all NA
        vargrad <- rep(NA, length(zm))
      }
      if(all(is.na(vargrad))){ # no data
        fakeGrad <- rep(0, length = length(zm))
        plot(fakeGrad, zm,
             xlim = c(0,8), ylim = rev(range(zm)),
             xlab = 'density gradient', ylab = '',
             col = 'white', xaxt = 'n')
        abline(h = pretty(zm), lty = 3, col = 'lightgrey')
        text(x = 4,
             y = mean(zm),
             labels = 'No valid data')
      } else { # valid data
        plot(vargrad, zm, ylim = rev(range(zm)),
             ylab = '',
             xlab = bquote('100('*Delta*rho*'/'*Delta*z*')'))
        abline(h = pretty(zm), lty = 3, col = 'lightgrey')
        abline(v = 1, lty = 2)
        if(!is.na(mldDefault)){
          abline(h=mldDefault)
          axis(side = 4, at = mldDefault)
        }
      }
    } # closes if method == 'gradient
    if(method == 'N2'){
      if(all(is.na(ctd[['N2']]))){
        hasNoData <- TRUE
        N2lim <- c(0,1)
         # add fake data to prevent warning message
         ctd <- oceSetData(ctd,
                           name = 'N2',
                           value = 1:length(ctd[['pressure']])) # so we don't get a xlim error
         col <- 'white'
      } else { # there is data
        hasNoData <- FALSE
        #N2lim <- range(ctd[['N2']], na.rm = TRUE)
        if(exists('N2')){
          N2lim <- range(N2[['N2']], na.rm = TRUE)
        } else {
          N2lim <- range(ctd[['N2']], na.rm = TRUE)
        }
        if(N2lim[1] == N2lim[2]){
          N2lim <- c(N2lim[1], N2lim[1] + 1)
        }
        col <- 'black'
      }

      plotProfile(ctd, xtype = 'N2',
                  mar = c(3.0, 3.5, 3.5, 2.0),
                  N2lim = N2lim,
                  col = 'white') # not sure what happens if I put in fake T/S data, if N2 gets calculated
      if(hasNoData){
        text(x = mean(ctd[['N2']]),
             y = mean(ctd[['pressure']]),
             labels = 'No data')
      }
      if(!is.na(mldDefault)){
        lines(N2[['N2']], abs(N2[['z']][2:length(N2[['z']])]), type = 'o')
      }

      if(!is.na(mldDefault)){
        abline(h=mldDefault)
        axis(side = 4, at = mldDefault)
      }
    } # closes method == 'N2'
    if(method == 'density'){
      # basically identical to N2
      if(all(is.na(ctd[['sigmaTheta']]))){
        hasNoData <- TRUE
        densitylim <- c(0,1)
        col <- 'white'
      } else { # there is data
        xtype <- 'sigmaTheta'
        hasNoData <- FALSE
        densitylim <- range(ctd[['sigmaTheta']], na.rm = TRUE)
        if(densitylim[1] == densitylim[2]){
          densitylim <- c(densitylim[1], densitylim[1] + 1)
        }
        col <- 'black'
      }

      oce::plotProfile(ctd, xtype = 'sigmaTheta',
                       type = 'o',
                       mar = c(3.0, 3.5, 3.5, 2.0),
                       #xlab = oce::resizableLabel(item = 'sigmaTheta', axis = 'x'), # keep getting Error : object 'label' not found
                       densitylim = densitylim,
                       #xlim = rholim, # provide both to cover for sigmaThetaFake
                       col = densityCol) # not sure what happens if I put in fake T/S data, if sigmaTheta gets calculated
      if(hasNoData){
        text(x = mean(ctd[[xtype]]),
             y = mean(ctd[['pressure']]),
             labels = 'No data')
      }
      if(!is.na(mldDefault)){
        abline(h=mldDefault)
        axis(side = 4, at = mldDefault)
      }
    } # closes method == 'density'
    mtext(text = ctd[['startTime']], side = 1, outer = TRUE)
  } # closes debug

  # output results in 1-row data.frame
  out <- data.frame(time = ctd[['startTime']],
                    year = as.POSIXlt(ctd[['startTime']])$year + 1900,
                    month = as.POSIXlt(ctd[['startTime']])$mon + 1,
                    day = as.POSIXlt(ctd[['startTime']])$mday,
                    yearDay = as.POSIXlt(ctd[['startTime']])$yday,
                    mixedLayerDepthDefault = mldDefault,
                    mixedLayerDepthForced = mldForced)
  if(method == 'density'){
    out <- data.frame(out,
                      referenceDepth = referenceDepth,
                      referenceDensity = referenceDensity
                      )
  }
  out
}

## this is the develop version of the function
## it was migrated to `csasAtlPhys` on 20230502
## this version is deemed to not be the most current version

#' @title Calculate stratification index
#'
#' @description This function calculates the stratification index, which is
#' defined as the difference in density between two defined depths divided
#' by the difference in the two depths. The calculation completes two
#' iterations. The first is between the two defined depths, and then if the
#' calculated value is less than 0, the depth range is slightly increased.
#'
#' @param ctd a `ctd` object.
#' @param depth1 a numeric value, such that depth1 < depth2
#' @param depth2 a numeric value, such that depth2 > depth1
#' @param debug a logical value indicating if debug info should be displayed
#' @return a single row data frame with relevant information for easy output use
#'
#' @author Chantelle Layton and Benoit Casault
#' @export

calculateStratificationIndex <- function(ctd, depth1, depth2, debug = TRUE){
  # check that depth1 is less than depth2
  okdepths <- depth1 < depth2
  if(!okdepths){
    warning('depth1 is greater than depth2')
  }
  # check that salinity is a variable in the ctd object,
  if(!'salinity' %in% names(ctd@data)){
    message('No salinity data')
    si <- NA
    siZmin <- NA
    siZmax <- NA
  } else {
    # pre-define si, siZmin, and siZmax for checking
    si <- NA
    siZmin <- NA
    siZmax <- NA
    # calculate stratification index (depth1 to depth2)
    if(min(ctd[['pressure']], na.rm=T) <= depth1 & max(ctd[['pressure']], na.rm=T) >= depth2){
      i_depth1 <- which(abs(ctd[['pressure']] - depth1) <= 0.5)
      z_min <- mean(ctd[['pressure']][i_depth1], na.rm=T)
      i_depth2 <- which(abs(ctd[['pressure']] - depth2) <= 0.5)
      z_max <- mean(ctd[['pressure']][i_depth2], na.rm=T)
      si <- (mean(ctd[['sigmaTheta']][i_depth2], na.rm=T) - mean(ctd[['sigmaTheta']][i_depth1], na.rm=T)) / (z_max - z_min)
      siZmin <- z_min
      siZmax <- z_max
    }
    # increase depth range when si<0
    if(!is.na(si) & si < 0){
      i_depth1 <- which(abs(ctd[['pressure']] - depth1) <= 1.0)
      z_min <- mean(ctd[['pressure']][i_depth1], na.rm=T)
      i_depth2 <- which(abs(ctd[['pressure']] - depth2) <= 1.0)
      z_max <- mean(ctd[['pressure']][i_depth2], na.rm=T)
      si <- (mean(ctd[['sigmaTheta']][i_depth2], na.rm=T) - mean(ctd[['sigmaTheta']][i_depth1], na.rm=T)) / (z_max - z_min)
      siZmin <- z_min
      siZmax <- z_max
    }
  }
  # output results in 1-row data.frame
  out <- data.frame(time = ctd[['startTime']],
                    year = as.POSIXlt(ctd[['startTime']])$year + 1900,
                    month = as.POSIXlt(ctd[['startTime']])$mon + 1,
                    day = as.POSIXlt(ctd[['startTime']])$mday,
                    yearDay = as.POSIXlt(ctd[['startTime']])$yday,
                    stratificationIndex = si,
                    minDepth = siZmin,
                    maxDepth = siZmax)
  out
}

#' @title Rotate axes
#'
#' @details This function rotates points in a x-y Cartesian coordinate system. Setting `inverse = TRUE` is effectively the
#' same as multiplying the provided `angle` by `-1`, so for example, providing parameters `angle = 50` and `inverse = TRUE` would give
#' the same results providing parameters `angle = -50` and `inverse = FALSE`.
#'
#' @param x a numeric vector of the x-component value(s)
#' @param y a numeric vector of the y-component value(s)
#' @param angle rotation angle in degrees
#' @param inverse logical value indicating whether or not to do inverse transformation, default is `FALSE`.
#'
#' @return list of rotated x and y components as `X` and `Y`.
#' @author Chantelle Layton
#' @export
#'
#' @references
#' 1. Basic explanation for rotation of axes \url{https://en.wikipedia.org/wiki/Rotation_of_axes}
#'
rotateAxes <- function(x, y, angle, inverse = FALSE){
  thetarad <- angle * pi/180
  if(!inverse){
    X <- x * cos(thetarad) + y * sin(thetarad)
    Y <- -x * sin(thetarad) + y * cos(thetarad)
  }
  if(inverse){
    X <- x * cos(thetarad) - y * sin(thetarad)
    Y <- x * sin(thetarad) + y * cos(thetarad)
  }
  invisible(list(X=X, Y=Y))
}

#' @title Rotate axes clockwise from true north
#'
#' @details This function rotates points clockwise from true north in a x-y Cartesian coordinate system.
#' Adapted from MATLAB code provided to me by David Hebert, which was used in Mathieu Dever's PhD.
#'
#' @param x a numeric vector of the x-component value(s)
#' @param y a numeric vector of the y-component value(s)
#' @param angle rotation angle in degrees
#'
#' @return list of rotated x and y components as `X` and `Y`.
#' @author Chantelle Layton
#' @export
#'

rotateAxesTrueNorth <- function(x, y, angle){
  thetarad <- angle * pi/180
  X <- x * sin(thetarad) + y * cos(thetarad)
  Y <- x * cos(thetarad) - y * sin(thetarad)
  invisible(list(X=X, Y=Y))
}
