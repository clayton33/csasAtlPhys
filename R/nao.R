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
  url <- 'https://www.ncdc.noaa.gov/teleconnections/nao/data.csv'
  # check if filename is supplied, if not use the filename from
  # the url
  if(is.null(destfile)){
    destfile <- basename(url)
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
