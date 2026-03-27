#' @title Create ICES WGOH formatted file for submission
#'
#' @description
#' Outputs a ICES WGOH formatted file for data submission. Based off the standard format provided on 20251109.
#'
#' @param data a data.frame of the data that is to be output
#' @param destdir a connection object or a character string indicating the directory the file should be saved.
#' @param fileDescriptor A character vector of length one. Indicates a brief description of the timeseries
#' for the filename.
#' @param fileType A character vector of length one. Indicates the time series file type.
#' Accepted options are "Annual", "Mnthly", and "Timeseries"
#' @param index A numeric vector of length one. It is the index as agreed by the WGOH regional group.
#' @param stationDescription A character vector of length one. Short station
#' description/name. This needs to be same across all file resolutions relating
#' to the same station/region (_Annual, _Mnthly, and _Timeseries) and the different
#' depth levels. For example, if three different timeseries relate to the same
#' station but different depth levels, then this needs to be the same for all
#' three of these timeseries.
#' @param regionCode A character vector of length one. It is the region code as agreed by
#' WGOH regional group. It is the region the timeseries is reported in.
#' @param latitude A numeric vector of length one. Nominal latitude position (for plotting purposes).
#' Max 3 decimal places.
#' @param lloongitude A numeric vector of length one. Nominal latitude position (for plotting purposes).
#' Max 3 decimal places.
#' @param detailedLocation A Well Known Text (WKT) formatted object to describe more detailed position
#' information (e.g. point, linestring, polygon, multilinestring, multipoint, multipolygon).
#' @param measurementDepth Depth or depth range related to the time series.
#' @param samplingFrequency A character vector of length one. Short description of how often sampling occurs.
#' @param longTermAveragingPeriod A character vector of length one. The climatological reference period
#' in the format of `begin`-`end`, preferably 1991-2020. This is only included in fileType = "Annual".
#' @param longTermMean A numeric vector indicating the long term mean for the defined
#' `longTermAveragingPeriod`. for each parameter in `data`. Temperature should have a maximum
#' of 2 decimal places and salinity should have a maximum of 3 decimal places.
#' This is only included in fileType = "Annual".
#' @param longTermStandardDeviation A numeric vector indicating the long term standard deviation
#'  for the defined `longTermAveragingPeriod`. for each parameter in `data`.
#'  Temperature should have a maximum of 2 decimal places and salinity should have
#'  a maximum of 3 decimal places. This is only included in fileType = "Annual".
#' @param calculationMethodology A character vector of length one. A short description of calculation methodology.
#' It should provide details of the methods for spatial averaging (in horizontal space and through depth),
#' temporal averaging, removal of seasonality, and calculation monthly/annual means; as appropriate.
#' It should also include details of how the long-term mean and standard deviation were calculated.
#' This is only included in fileType = "Annual".
#' @param dataSource A character vector of length one. The organization who provides the data in the format
#' `acronym-full name-country`.
#' @param contactName A character vector of length one, with contact name and email address, in the format
#' `name (email)`. Generic inboxes are allowed for groups where an individual name is less appropriate.
#' @param notes An optional character vector of length one. Provides additional notes relating to the time series.
#' @param reference An optional character vector of length one. Journal publication reference, it should be
#' in quotation marks to avoid comma's in the reference causing issues in the CSV format.
#' @param doi An optional character vector of length one. The DOI of the dataset.
#' @param website An optional character vector of length one. The website for the project or dataset.
#'
#' @export
#' @author Chantelle Layton

createIcesWgohTimeserieFile <- function(data,
                                        destdir,
                                        fileDescriptor,
                                        fileType,
                                        index,
                                        stationDescription,
                                        regionCode,
                                        latitude,
                                        longitude,
                                        detailedLocation,
                                        measurementDepth,
                                        samplingFrequency,
                                        longTermAveragingPeriod,
                                        longTermMean,
                                        longTermStandardDeviation,
                                        calculationMethodology,
                                        dataSource,
                                        contactName,
                                        notes,
                                        reference,
                                        doi,
                                        website) {
  # define simple function to check for "," in provided header information
  checkForComma <- function(x){
    if(grepl('\\,', x)){
      xout <- paste0('"', x, '"')
    } else {
      xout <- x
    }
    return(xout)
  }
  # header ----
  ## create each line ----
  ### in order, parsed each line out to make adding/removing easy
  indexOut <- paste('# Index:', checkForComma(index), sep = ',')
  stationDescriptionOut <- paste('# Station Description:', checkForComma(stationDescription), sep = ',')
  regionCodeOut <- paste('# Region Code:', checkForComma(regionCode), sep = ',')
  latitudeOut <- paste('# Latitude:', latitude, sep = ',')
  longitudeOut <- paste('# Longitude:', longitude, sep = ',')
  #latitudeOut <- paste('# Latitude:', sprintf('%.3f', latitude), sep = ',')
  #longitudeOut <- paste('# Longitude:', sprintf('%.3f', longitude), sep = ',')
  detailedLocationOut <- paste('# Detailed Location (WKT):', checkForComma(detailedLocation), sep = ',') # this one is OK to have commas
  measurementDepthOut <- paste('# Measurement Depth/Range:', checkForComma(measurementDepth), sep = ',')
  samplingFrequencyOut <- paste('# Sampling Frequency:', checkForComma(samplingFrequency), sep = ',')
  if(fileType == 'Annual'){
    longTermAveragingPeriodOut <- paste('# Long Term Averaging Period:', paste(sapply(longTermAveragingPeriod, checkForComma), collapse = ','), sep = ',')
    longTermMeanOut <- paste('# Long Term Mean:', paste(sapply(longTermMean, checkForComma), collapse = ','), sep = ',')
    longTermStandardDeviationOut <- paste('# Long Term Standard Deviation:', paste(sapply(longTermStandardDeviation, checkForComma), collapse = ','), sep = ',')
    calculationMethodologyOut <- paste('# Calculation Methodology:', paste(sapply(calculationMethodology, checkForComma), collapse = ','), sep = ',')
  }
  dataSourceOut <- paste('# Data Source:', checkForComma(dataSource), sep = ',')
  contactNameOut <- paste('# Contact Name (email):', checkForComma(contactName), sep = ',')
  notesOut <- paste('# Notes:', checkForComma(notes), sep = ',')
  referenceOut <- paste('# Reference:', checkForComma(reference), sep = ',')
  doiOut <- paste('# DOI:', checkForComma(doi), sep = ',')
  websiteOut <- paste('# Website:', checkForComma(website), sep = ',')
  ## create ----
  header <- c(indexOut,
              stationDescriptionOut,
              regionCodeOut,
              latitudeOut,
              longitudeOut,
              detailedLocationOut,
              measurementDepthOut,
              samplingFrequencyOut,
              dataSourceOut,
              contactNameOut,
              notesOut,
              referenceOut,
              doiOut,
              websiteOut)
  ### fileType = Annual case ----
  if(fileType == 'Annual'){
    #### define additional header lines to add ----
    headeradd <- c(longTermAveragingPeriodOut,
                   longTermMeanOut,
                   longTermStandardDeviationOut,
                   calculationMethodologyOut)
    #### find index for '# Sampling Frequency ----
    idxadd <- grep('# Sampling Frequency:', header)
    #### add additional lines to header ----
    header <- c(header[1:idxadd],
                headeradd,
                header[(idxadd + 1):length(header)])
  }
  # define file name ----
  ## note : the filenaming naming convention seems to be note well defined
  ##        a few e.g.'s that I have are
  ##        "NEAS_017_USA_WGOM_Annual"
  ##        "NEAS_017_USA_WGOM_Timeseries"
  ##        "BALT_001_Baltic_BY15_Mnthly"
  ##        first part is the index (NEAS_017 and BALT_001)
  ##        second part I'm unsure (USA_WGOM and Baltic_BY15) neither are in the metadata
  ##        third part is the fileType.
  ##        for the second part I added a parameter called 'fileDescriptor'
  filename <- paste0(paste(index, fileDescriptor, fileType, sep = '_'), '.csv')
  # create connection ----
  con <- file(description = paste(destdir, filename, sep = '/'),
              open = "w",
              encoding = "UTF-8")
  # save file with data ----
  ## header ----
  writeLines(text = header, con = con)
  ## data ----
  write.table(x = data,
              file = con,
              sep = ',', na = '',
              row.names = FALSE,
              col.names = TRUE,
              append = TRUE)
  close(con)
}


