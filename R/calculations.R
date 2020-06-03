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
#' @param normalizedAnomaly a logical value indicating whether or not to calculate the
#' normalized annual anomaly as well, the default is `TRUE`
#'
#' @return a data.frame with year, anomaly, and optionally normalizedAnomaly
#'
#' @author Chantelle Layton
#'
#' @importFrom stats aggregate
#'
#' @export

annualAnomaly <- function(d, normalizedAnomaly = TRUE){
  aa <- aggregate(anomaly ~ year, d, mean, na.rm = TRUE)
  if(normalizedAnomaly){
    ana <- aggregate(normalizedAnomaly ~ year, d, mean, na.rm = TRUE)
    cbind(aa, normalizedAnomaly = ana$normalizedAnomaly)
  } else {
    aa
  }
}
