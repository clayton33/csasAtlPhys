#' @title Download IML data from CIOOS ERDDAP site
#'
#' @description Download IML data from CIOOS ERDDAP site and create ctd objects
#' for each associated `id`.
#'
#' @param startYear A numeric vector of length one indicating the start year for the data extraction.
#' @param endYear A numeric vector of length one indicating the end year for the data extraction.
#'
#' @author Chantelle Layton
#'
#' @importFrom oce as.ctd
#' @importFrom rerddap info
#' @importFrom rerddap tabledap
#'
#' @return list of oce ctd objects
#'
#' @export
#'
download.imlCTD.erddap <- function(startYear, endYear){
  # define url
  url <- "https://erddap.ogsl.ca/erddap/"
  # define dataset id
  datasetid <- "mpoSgdoCtdAgg"
  # get info for given url and datasetid
  info <- rerddap::info(datasetid = datasetid, url = url)
  # construct query's based on time
  time1 <- as.numeric(as.POSIXct(paste0(startYear, '-01-01')), origin = '1970-01-01', tz = 'UTC')
  time2 <- as.numeric(as.POSIXct(paste0(endYear, '-12-31')), origin = '1970-01-01', tz = 'UTC')
  query1 <- paste0('time>=', time1)
  query2 <- paste0('time<=', time2)
  # query data
  d <- rerddap::tabledap(x = info,
                         query1,
                         query2,
                         url = url)
  # get variables
  vars <- info[['variables']][['variable_name']]
  # science variables
  okvars <- grepl(pattern='.*1$',
                  x = vars)
  primaryDataVars <- vars[okvars]
  # hook up science variables with qc variables
  dfqcvars <- NULL
  for(ip in 1:length(primaryDataVars)){
    var <- primaryDataVars[ip]
    # find which index it matches with in info$alldata
    ok <- which(names(info$alldata) == var)
    if(length(ok) != 0){
      dvar <- info[['alldata']][[ok]]
      if('ancillary_variables' %in% dvar[['attribute_name']]){
        okattribute <- which(dvar[['attribute_name']] == 'ancillary_variables')
        dfadd <- data.frame(variable = var,
                            qcVariable = dvar[['value']][okattribute])
        dfqcvars <- rbind(dfqcvars, dfadd)
      }
    }
  }
  # # get flag meaning
  # qcvareg <- info$alldata$TEMPS901_QC
  # okmeaning <- which(qcvareg[['attribute_name']] == 'flag_meanings')
  # okvalue <- which(qcvareg[['attribute_name']] == 'flag_values')
  # flagMeaning <- data.frame(value = trimws(strsplit(qcvareg[['value']][okvalue], ',')[[1]]),
  #                           meaning = strsplit(qcvareg[['value']][okmeaning], split = '\\s+')[[1]])
  # apply qc
  dd <- d
  for(i in 1:dim(dfqcvars)[1]){
    datavar <- dfqcvars[['variable']][i]
    qcvar <- dfqcvars[['qcVariable']][i]
    # get variable data
    data <- d[[datavar]]
    # get qc data
    qcdata <- d[[qcvar]]
    # apply qc
    data[!qcdata %in% c(0,1)] <- NA
    # add qc'd data to queried data
    dd[[datavar]] <- data
  }
  dd <- as.data.frame(dd)
  # split up data.frame by id
  ds <- split(dd, dd[['id']])
  # iterate through each id and create a ctd objects
  ctd <- vector(mode = 'list', length = length(ds))
  for(id in 1:length(ds)){
    k <- ds[[id]]
    kk <- oce::as.ctd(salinity = k[['PSALST01']],
                      temperature = k[['TEMPS901']],
                      pressure = k[['PRESPR01']],
                      time = k[['time']],
                      cruise = k[['cruise_number']][1],
                      longitude = k[['longitude']][1],
                      latitude = k[['latitude']][1],
                      startTime = k[['time']][1],
                      deploymentType = 'profile',
                      station = k[['station']][1])
    ctd[[id]] <- kk
  }
  return(ctd)
}
