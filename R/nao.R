#' @title Download and Cache the North Atlantic Oscillation data
#'
#' @description Data are downloaded from the National Centers for Environmental Information
#' National Oceanic and Atmospheric Administration website.
#'
#' @details The data are downloaded from
#' \url{'https://www.ncdc.noaa.gov/teleconnections/nao/data.csv'}
#' using [utils::download.file()]
#' pointed to the National Centers for Environmental Information National
#' Oceanic and Atmospheric Administration website. Caution: the
#' location that these data are held could potentially vary, so it could work one
#' day, and fail the next.
#'
#' @param destdir Optional string indicating the directory in which to store downloaded files.
#' If not supplied, "." is used, i.e. the data file is stored in the present working directory.
#' @param destfile Optional string indicating the name of the file. If not supplied, the file
#' name as provided on the website.
#'
#' @importFrom utils download.file
#'
#' @export

download.nao <- function(destdir = '.', destfile = NULL){
  # check if destdir exists, if not create it
  if(!dir.exists(destdir)){
    dir.create(destdir, recursive = TRUE)
  }
  # define url to download data
  #url <- 'https://www.ncdc.noaa.gov/teleconnections/nao/data.csv'
  # late 2021/early 2022 changed, found on 20220106
  url <- 'https://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table'
  # check if filename is supplied, if not use the filename from
  # the url
  if(is.null(destfile)){
    destfile <- basename(url)
    ss <- strsplit(x = destfile, split = '\\.')[[1]]
    if(ss[length(ss)] != 'csv'){ # save new format as .txt file as it is tab deliminated
      destfile <- paste(destfile, 'txt', sep = '.')
    }
  }
  download.file(url = url, destfile = paste(destdir, destfile, sep = '/'))
}

#' @title Read North Atlantic Oscillation Index file
#'
#' @description This function reads in the north atlantic oscillation index
#' comma separated file.
#'
#' @param file a connection or a character string giving the name of the file to load.
#'
#' @return a data frame with columns named year, month, and value
#'
#' @author Chantelle Layton
#'
#' @importFrom utils read.csv
#'
#' @export
#'

read.nao <- function(file){
  fext <- strsplit(basename(file), split = '\\.')[[1]]
  if(fext[length(fext)] == 'csv'){ # data format for 2019 to 2021 data
    d <- read.csv(file, skip = 1, header = TRUE)
    # first column has the header named Date
    # it has the year and month combined
    # characters 1-4 are the year, characters 5-6 is the month
    # extract them
    year <- as.numeric(substr(d$Date, 1, 4))
    month <- as.numeric(substr(d$Date, 5, 6))
    df <- data.frame(year = year,
                     month = month,
                     value = d$Value)
    df
  }
  if(fext[length(fext)] == 'txt'){ # different data format identified 20220116, see download.nao for details
    # switch to readLines to accommodate partial years of data
    rl <- readLines(con = file)
    # first line are month headers
    months <- strsplit(x = rl[1], split = '\\s+')[[1]]
    d <- strsplit(x = rl[2:length(rl)], split = '\\s+')
    year <- as.numeric(unlist(lapply(d, function(k) k[1])))
    data <- lapply(d, function(k) as.numeric(k[2:length(k)]))
    # add NA values for partial years
    data <- lapply(data, function(k) if(length(k) != 12){k <- c(k, rep(NA, times = 12-length(k))); k} else {k})
    datam <- do.call('rbind', data)
    datav <- as.vector(datam) # decomposes it column wise
    df <- data.frame(year = rep(year, times = 12),
                     month = unlist(lapply(1:12, rep, time = length(year))),
                     value = datav)
  }
}
