rm(list=ls())
library(usethis)
load('data-raw/winterGroundfish/Winter_Groundfish_OE.RData')
names(oe_data) <- tolower(names(oe_data))
winterGroundfishOaxResults <- oe_data
usethis::use_data(winterGroundfishOaxResults, compress = 'xz', overwrite = TRUE)
