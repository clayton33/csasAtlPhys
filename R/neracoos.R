#' @title Download NERACOOS buoy CTD data
#'
#' @description Download hourly buoy CTD data hosted by Northeastern Regional Association
#' of Coastal Ocean Observing Systems (NERACOOS) from the ERDDAP server.
#'
#' @param buoyName A character string identifying the name of the buoy. As of now 20210104 a good
#' definition of all the buoy data currently available has not been identified. Although, the old
#' method of downloading the data has a list of buoy names with a more descriptive name,
#' http://www.neracoos.org/datatools/historical/graphing_download.
#' @param startDate A character string in the format `yyyy-mm-dd` identifying the start of the desired
#' timeseries beginning at 00:00UTC
#' @param endDate A character string in the format `yyyy-mm-dd` indentifying the end of the desired
#' timeseries ending at 23:59UTC
#' @param destdir Optional string indicating the directory in which to store
#' downloaded files. If not supplied, `"."` is used, i.e. the data file
#' is stored in the present working directory. Also, the directory will be created
#' if not already done so.
#'
#' @details This function downloads data from the NERACOOS ERDDAP server via a ftp connection.
#' It has been specifically written to download CTD data from their buoy sites. Data chosen to download
#' includes all variables, including QC flags. The file is downloaded as a netcdf file and is output with the following
#' naming convention, `buoyName_sbe37_all_startDate_endDate.nc` , which uses the supplied parameters.
#'
#' @author Chantelle Layton
#' @importFrom utils download.file
#' @export
#'

download.neracoos.buoy <- function(buoyName, startDate, endDate, destdir = '.'){
  # check that buoyName, startDate, and endDate are given
  if(missing(buoyName) | missing(startDate) | missing(endDate)){
    stop('Must supply all three parameters, buoyName, startDate, and endDate, in order to download data.')
  }

  # create output directory if not already done so
  if(!dir.exists(destdir)){
    dir.create(destdir, recursive = TRUE)
  }

  baseurl <- 'http://www.neracoos.org/'

  url <- paste0(baseurl,'erddap/tabledap/',
                buoyName, '_sbe37_all.nc?station%2Ctime%2Cmooring_site_desc',
                '%2Cconductivity%2Cconductivity_qc',
                '%2Ctemperature%2Ctemperature_qc',
                '%2Csalinity%2Csalinity_qc',
                '%2Csigma_t%2Csigma_t_qc',
                '%2Clongitude%2Clatitude',
                '%2Cdepth&time%3E=', startDate, 'T00%3A00%3A00Z',
                '&time%3C=', endDate, 'T14%3A23%3A00Z')
  start <- gsub('-','', startDate)
  end <- gsub('-','', endDate)
  destfile <- paste0(paste(buoyName, 'sbe37_all', start, end, sep = '_'),'.nc')
  download.file(url = url,
                destfile = paste(destdir, destfile, sep = '/'),
                mode = 'wb')

}

#' @title Read NERACOOS buoy data
#'
#' @description This function reads in NERACOOS buoy data that was downloaded using [read.neracoos.buoy].
#'
#' @param file a connection or a character string giving the name of the file to read.
#'
#' @return A list of `ctd` objects, one for each depth.
#'
#' @author Chantelle Layton
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 ncvar_get
#' @importFrom ncdf4 ncatt_get
#' @importFrom oce as.ctd
#'
#' @export
#'
read.neracoos.buoy <- function(file){
  nc <- nc_open(filename = file)
  Tqc <- ncvar_get(nc = nc, varid = 'temperature_qc')
  condqc <- ncvar_get(nc = nc, varid = 'conductivity_qc')
  Sqc <- ncvar_get(nc = nc, varid = 'salinity_qc')
  STqc <- ncvar_get(nc = nc, varid = 'sigma_t_qc')
  qcflags <- cbind(Tqc, condqc, Sqc, STqc)
  qcflagslog <- qcflags != 0
  badflags <- apply(qcflagslog, 1, any)

  globalAtts <- ncatt_get(nc, 0)
  stnName <- globalAtts$station_name
  waterDepth <- globalAtts$water_depth

  if(all(badflags)){
    stop(paste0('All data is bad for buoy ', stnName) , sep = '\n')
  } else {
    time <- as.POSIXct(ncvar_get(nc = nc, varid = 'time'), origin = '1970-01-01', tz = 'UTC')[!badflags]
    lat <- ncvar_get(nc = nc, varid = 'latitude')[!badflags]
    lon <- ncvar_get(nc = nc, varid = 'longitude')[!badflags]
    depth <- ncvar_get(nc = nc, varid = 'depth')[!badflags]
    T <- ncvar_get(nc = nc, varid = 'temperature')[!badflags]
    cond <- ncvar_get(nc = nc, varid = 'conductivity')[!badflags]
    S <- ncvar_get(nc = nc, varid = 'salinity')[!badflags]
    ST <- ncvar_get(nc = nc, varid = 'sigma_t')[!badflags]


    # mooring at different depths, pull out data for each depth
    udepth <- unique(depth)
    ctd <- vector(mode = 'list', length = length(udepth))
    for(j in 1:length(udepth)){
      lookdepth <- udepth[j]
      okdepth <- which(depth == lookdepth)
      oktime <- time[okdepth]
      ot <- order(oktime)
      ctd[[j]] <- as.ctd(salinity = S[okdepth[ot]],
                         temperature = T[okdepth[ot]],
                         conductivity = cond[okdepth[ot]],
                         pressure = depth[okdepth[ot]],
                         time = time[okdepth[ot]],
                         startTime = time[okdepth[ot]][1],
                         longitude = lon[okdepth[ot]],
                         latitude = lat[okdepth[ot]],
                         deploymentType = 'moored',
                         station = stnName,
                         cruise = stnName)
    }
  }
  ctd
}
