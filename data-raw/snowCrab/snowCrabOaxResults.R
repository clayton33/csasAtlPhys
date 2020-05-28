rm(list=ls())
library(usethis)
load('data-raw/snowCrab/September_Groundfish_OE.RData')
names(oe_data) <- tolower(names(oe_data))
snowCrabOaxResults <- oe_data
usethis::use_data(snowCrabOaxResults, compress = 'xz', overwrite = TRUE)
