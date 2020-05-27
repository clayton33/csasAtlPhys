library(usethis)
load('data-raw/summerGroundfish/julyGroundFishOE2019.rda')
summerGroundfishOaxResults <- oe_data
usethis::use_data(summerGroundfishOaxResults, compress = 'xz', overwrite = TRUE)
