rm(list=ls())
library(usethis)
load('data-raw/seaSurfaceTemperatureInSitu/SST_Monthly_Series.RData')
names(halifax_sst) <- tolower(names(halifax_sst))

sstInSituHalifax <- halifax_sst
usethis::use_data(sstInSituHalifax, compress = 'xz', overwrite = TRUE)

names(standrews_sst) <- tolower(names(standrews_sst))

sstInSituStAndrews <- standrews_sst
usethis::use_data(sstInSituStAndrews, compress = 'xz', overwrite = TRUE)
