#' @title Create a grid mask
#'
#' @description This function takes supplied longitude, latitude along with the grid spacing, and creates
#' an equally spaced rectangular grid mask.
#'
#' @param gridLongitudeOriginal a numerical vector of the unique grid longitude values
#' @param gridLatitudeOriginal a numerical vector of the unique grid latitude values
#' @param gridPtLongitude a numerical vector of the grid point longitude values
#' @param gridPtLatitude a numerical vector of the grid point latitude values
#' @param gridDiffLongitude a numerical value indicating the delta value between \code{gridLongitude}
#' @param gridDiffLatitude a numerical value indicating the delta value between \code{gridLatitude}
#' @param useCtd a logical value indicating whether or not to use ctd data when making the mask, default is \code{FALSE}.
#' @param ctdLongitude a numerical vector of the ctd longitude, ignored if `useCTD = FALSE`, default is \code{NULL}.
#' @param ctdLatitude a numerical vector of the ctd latitude, ignored if `useCTD = FALSE`, default is \code{NULL}.
#' @param ctd a list of ctd objects, default is \code{NULL}
#' @param ctdDistance a numerical value indicating the minimum distance, in kilometers, that grid points should
#' be to a ctd profile, default is \code{30}.
#'
#' @author Chantelle Layton
#'
#' @return a list containing the gridmask, which is a vector of TRUE/FALSE values,the expanded grid,
#' and the gridLongitude and gridLatitude
#'
#' @export

makeGridMask <- function(gridLongitudeOriginal,
                         gridLatitudeOriginal,
                         gridPtLongitude,
                         gridPtLatitude,
                         gridDiffLongitude,
                         gridDiffLatitude,
                         useCtd = FALSE,
                         ctdLongitude = NULL,
                         ctdLatitude = NULL,
                         ctd = NULL,
                         ctdDistance = 30
){
  # 1. create equally spaced rectangular grid
  # the grid is 0.2 x 0.2 in spacing, get the min and max of lon and lat
  rlon <- range(gridLongitudeOriginal)
  rlat <- range(gridLatitudeOriginal)
  # grid must be in ascending order (small to large)
  # grid will start in bottom left corner
  gridlon <- seq(rlon[1], rlon[2], gridDiffLongitude)
  gridlat <- seq(rlat[1], rlat[2], gridDiffLatitude)
  expgrid <- expand.grid(x = gridlon, y = gridlat)

  # 2. create mask for grid

  ##
  # a. points not in original grid
  ##

  gridcoord <- data.frame(latitude = gridPtLatitude,
                          longitude = gridPtLongitude) # lat,lon
  ugrid <- unique(gridcoord)
  # 20201029 something going on with expand.grid, have to add round(,1) to expgrid
  # when matching up grids, not sure why ...
  expgrid <- expand.grid(x = gridlon, y = gridlat)
  origgridmask <- unlist(mapply(function(long, latg) any(geodDist(longitude1 = ugrid[,2],
                                                                  latitude1 = ugrid[,1],
                                                                  longitude2 = long,
                                                                  latitude2 = latg) == 0),
                                round(expgrid[,1],1), round(expgrid[,2],1)))

  ##
  # b. points where there is no ctd data within defined ctdDistance if useCtd = TRUE
  ##
  if(useCtd){
    # 1. make sure there is one of them, if all NULL, stop and provide message
    if(is.null(ctdLongitude) & is.null(ctdLatitude) & is.null(ctd)){
      stop("Please supply either 'ctdLongitude' and 'ctdLatitude', or 'ctd', or change useCtd to FALSE")
    }
    # 2. use supplied ctdLongitude and ctdLatitude over ctd if all three are provided
    #    so we'll pull out longitude and latitude from ctd if both ctdLon..[Lat..] are null
    if(is.null(ctdLongitude) & is.null(ctdLatitude) & !is.null(ctd)){
      ctdLongitude <- unlist(lapply(ctd, function(k) k[['longitude']][1]))
      ctdLatitude <- unlist(lapply(ctd, function(k) k[['latitude']][1]))
    }
    ctdgridmask <- unlist(mapply(function(long, latg) any(geodDist(longitude1 = ctdLongitude,
                                                                   latitude1 = ctdLatitude,
                                                                   longitude2 = long,
                                                                   latitude2 = latg) <= ctdDistance),
                                 expgrid[,1], expgrid[,2]))
    ##
    # create mask
    ##

    # combine the a. and b. steps
    masksteps <- cbind(origgridmask, ctdgridmask)

    # find when both a. and b. are true
    maskIdx <- apply(masksteps, 1, all)
  } else {
    # this is a matrix, will have figure out how to make it a vector, have to be careful if I do it
    # by row or columns.
    maskIdx <- origgridmask
  }

  # expanded grid lon wise, rectangular grid has lon as row and lat as columns

  gridmask <- matrix(data = maskIdx, nrow = length(gridlon), ncol = length(gridlat))
  list(gridmask = maskIdx,
       expgrid = expgrid,
       gridPtLongitude = gridPtLongitude,
       gridPtLatitude = gridPtLongitude)
}
