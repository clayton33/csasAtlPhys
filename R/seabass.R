#' @title Download SeaBASS Glider data
#'
#' @description Download glider data that is hosted on the SeaWiFS Bio-optical
#' Archive and Storage System (SeaBASS) that is maintained by the
#' NASA Ocean Biology Processing Group (OBPG).
#'
#' @details This function downloads glider data that is hosted on the SeaWiFS Bio-optical
#' Archive and Storage System (SeaBASS) site that is maintained by the
#' NASA Ocean Biology Processing Group (OBPG). Since there is no ftp site
#' to download data from, the website is interrograted to obtain a list
#' of glider files, and the files are simply downloaded from the website.
#' Since there is no descriptive information on the time range that each
#' file covers, all files are downloaded and it is up to the user to identify
#' which data is of use.
#'
#' @param destdir Optional string indicating the directory in which to store
#' downloaded files. If not supplied, `"."` is used, i.e. the data file
#' is stored in the present working directory. Also, if the directory will be created
#' if not already done so.
#'
#' @importFrom utils download.file
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
#'
#'

download.seabass.glider <- function(destdir = '.'){
  url <- 'https://seabass.gsfc.nasa.gov/archive/BIGELOW/BALCH/gnats/archive/glider/'
  # obtain the file list from a table on the website
  webpage <- read_html(url)
  tables <- html_nodes(webpage, 'table')
  table <- html_table(tables[2])[[1]] # table id = 'archive_table'
  filenames <- table$Name

  if(!dir.exists(destdir)){
    dir.create(destdir, recursive = TRUE)
  }

  lapply(filenames, function(k) download.file(url = paste0(url,k), destfile = paste(destdir, k, sep = '/')))

}

#' @title Read SeaBASS glider data
#'
#' @description Read in SeaBASS glider data files that were downloaded
#' using [download.seabass.glider()].
#'
#' @details This function reads in glider data files that were downloaded
#' using [download.seabass.glider()]. As of now, since there is no formal
#' way of handling glider data, the data is returned as an ctd object which
#' can be further used to obtain individual profiles.
#'
#' @param file a connection or a character string giving the name of the file to read.
#'
#' @return a ctd object
#'
#' @importFrom oce as.ctd
#'

read.seabass.glider <- function(file){
  rl <- readLines(file)
  bh <- grep('/begin_header', rl)
  eh <- grep('/end_header', rl)
  # get some metadata
  cruise <- gsub('^/cruise=(\\w+)', '\\1', rl[grep('^/cruise', rl[bh:eh])])
  data <- read.table(file, skip = eh)
  # replace -999 with NA
  data[data == -999] <- NA
  fl <- grep('^/fields', rl)
  headerline <- gsub('^/fields=(\\w+)', '\\1',rl[fl])
  header <- strsplit(headerline, ',')[[1]]
  names(data) <- header
  time <- as.POSIXct(paste(paste(data$year, data$month, data$day, sep = '-'), paste(data$hour, data$minute, data$second, sep = ':'),
                           sep = ' '), tz = 'UTC')
  as.ctd(salinity = data$sal,
         temperature = data$Wt,
         pressure = data$depth,
         time = time,
         longitude = data$lon,
         latitude = data$lat,
         cruise = cruise)
}
