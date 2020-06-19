rm(list=ls())
library(usethis)
load('data-raw/winterGroundfish/winterGroundFishOE2019.rda')
winterGroundfishOaxResults <- oe_data
usethis::use_data(winterGroundfishOaxResults, compress = 'xz', overwrite = TRUE)
