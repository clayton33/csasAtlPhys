rm(list=ls())
library(usethis)

path <- 'data-raw/transects/binnedClimatology'
files <- list.files(path, pattern = '^.*\\.txt$')

transectClimatology <- vector(mode = 'list', length = length(files))
for(i in 1:length(files)){
  file <- files[i]
  cli <- read.table(paste(path, file, sep = '/'))
  names(cli) <- c('distanceBin', 'depthBin',
                  'temperature', 'temperatureSd', 'temperatureNobs',
                  'salinity', 'salinitySd', 'salinityNobs',
                  'sigmaTheta', 'sigmaThetaSd', 'sigmaThetaNobs')
  cli[cli == -99] <- NA
  # no information in the files other than the data,
  #   so in order to know which transect is which
  #   we have to pull apart the file name
  transectAbbreviation <- gsub('^(\\w+)_\\w+_\\w+\\.txt$', '\\1', file)
  season <- tolower(gsub('^\\w+_(\\w+)_\\w+\\.txt$', '\\1', file))
  program <- tolower(gsub('^\\w+_\\w+_(\\w+)\\.txt$', '\\1', file))
  transectFullname <- switch(transectAbbreviation,
                             'HL' = 'halifaxInshore',
                             'CS' = 'cabotStrait',
                             'LL' = 'louisbourg',
                             'BB' = 'brownsBank',
                             'PS' = 'northEastChannel',
                             'LH' = 'laHave',
                             'SIB' = 'sableIslandBank',
                             'BANQ' = 'banquereauBank')
  transectClimatology[[i]] <- list(transectAbbreviation = transectAbbreviation,
                                   transectFullname = transectFullname,
                                   season = season,
                                   program = program,
                                   climatology = cli)
}

transectClimatology1980to2010 <- transectClimatology
usethis::use_data(transectClimatology1980to2010, compress = 'xz', overwrite = TRUE)
