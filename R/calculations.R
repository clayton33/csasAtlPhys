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
