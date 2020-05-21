#' @title Download and Cache a Met Climate Summary File
#'
#' @description Data are downloaded from the Environment and Climate Change Canada's
#' climate weather website and cached locally
#'
#' @details The data are downloaded from
#' \url{http://climate.weather.gc.ca/prods_servs/cdn_climate_summary_report_e.html}
#' using [utils::download.file()]
#' pointed to the Environment and Climate Change Canada website (reference 1)
#' using queries that had to be devised by reverse-engineering, since the agency
#' does not provide documentation about how to construct queries. Caution: the
#' query format changes from time to time, so [download.met.climateSummaries()] may work one
#' day, and fail the next.
#'
#' @param year A number giving the year of interest.
#' @param month A number giving the month of interest.
#' @param province A string giving the province abbreviation of interest.
#' @param type A string indicating which type of file to download, either
#' `"xml"` for an XML file or `"csv"` for a CSV file.
#' @param destdir Optional string indicating the directory in which to store
#' downloaded files. If not supplied, `"."` is used, i.e. the data file
#' is stored in the present working directory.
#' @param destfile Optional string indicating the name of the file.
#' If not supplied, the file name is constructed from the other
#' parameters of the function call, so subsequent calls with the same
#' parameters will yield the same result, thus providing the key
#' to the caching scheme.
#' @param force Logical value indicating whether to force a download, even
#' if the file already exists locally.
#' @param quiet Logical value passed to [download.file()]; a `TRUE` value
#' silences output.
#'
#' @importFrom utils download.file
#' @importFrom utils capture.output
#'
#' @export

download.met.climateSummaries <- function(year, month, province, type,
                                          destdir = '.', destfile,
                                          force = FALSE, quiet = TRUE)
{

  if(missing(year) | missing(month)){
    warning("year or month was not provided, downloading the most recent file.")
    today <- as.POSIXlt(presentTime())
    year <- today$year + 1900
    month <- today$mon + 1 # so 1 = jan
    # data, for example, for the month of march, are available on april 5
    # so if the month day is less than 5, set the month
    # to be 2 less than today's. Some extra logic if 'today' is
    # january or february
    {
      if (month == 1) {
        year <- year - 1
        if (today$mday < 5) {
          month <- 11
        } else {
          month <- 12
        }
      } else if (month == 2) {
        if (today$mday < 5) {
          month <- 12
          year <- year - 1
        } else {
          month <- month - 1
        }
      } else {
        if (today$mday < 5) {
          month <- month - 2
        } else {
          month <- month - 1
        }
      }
    }
  }

  if(missing(month)){
    stop("Please provide a month.")
  }

  provinceChoices <- c('All', 'AB', 'BC', 'MB', 'NB', 'NL', 'NT', 'NS', 'NU', 'ON', 'PE', 'QC', 'SK', 'YT')
  if(!province %in% provinceChoices){
    stop("Province abbreviation '", province, "' is not correct, try one of the following: \n", paste(provinceChoices, collapse = ' '))
  }

  typeChoices <- c('csv', 'xml')
  if(!type %in% typeChoices){
    stop("type '", type, "' is not permitted ; try either 'csv' or 'xml' ")
  }

  if(missing(destfile)){
    destfile <- paste0('metClimateSummaries_',province,'_',year,ifelse(month < 10, paste0('0', month), month),'.', type)
  }


  url <- paste0('http://climate.weather.gc.ca/prods_servs/cdn_climate_summary_report_e.html?int',
                'Year=',year,
                '&intMonth=',month,
                '&prov=',province,
                '&dataFormat=',type,
                '&btnSubmit=Download+data')

  destination <- paste(destdir, destfile, sep="/")
  # oceDebug(debug, "url:", url, "\n")
   if (!force && 1 == length(list.files(path=destdir, pattern=paste("^", destfile, "$", sep="")))) {
  #   #oceDebug(debug, "Not downloading \"", destfile, "\" because it is already present in the \"", destdir, "\" directory\n", sep="")
   } else {
  #   ##?owarn <- options()$warn # this, and the capture.output, quieten the processing
  #   ##?options(warn=-1)
     capture.output({download.file(url, destination, quiet=quiet, mode = 'wb')})
  #   ##?options(warn=owarn)
  #   #oceDebug(debug, "Downloaded file stored as '", destination, "'\n", sep="")
   }
  destination
}
