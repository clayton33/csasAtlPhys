library(usethis)
# load oe results for calculating CIL and minimum temperature
# this is different that julyGroundFishOE2019.rda as the analysis was done differently
# due to low vertical resolution in the earlier years, data was combined to get better results
# for e.g 1972 includes data from 1970 to 1974. Data from 1990 to 2017
# is all high resolution data, so it is only the data from that year

load('data-raw/summerGroundfish/Groundfish_TS_OE.RData')
names(tsdata) <- tolower(names(tsdata))
omitnames <- names(tsdata) %in% 'error'
tsdata <- tsdata[, !omitnames]
load('data-raw/summerGroundfish/julyGroundFishOE2019.rda')
# add the 2019 results to tsdata, but this will be the full grid
# not sure if this will add confusion in the future, but i'll
# note it in the documentation
oknames <- unlist(lapply(names(tsdata), function(k) which(names(oe_data) == k)))
# there is no 'error' column in oe_data, but i'm going to omit it from tsdata because
# i have no clue where it comes from or what is represents
summerGroundfishOaxResultsForCIL <- rbind(tsdata,
                                          oe_data[oe_data$year == 2019, oknames])
usethis::use_data(summerGroundfishOaxResultsForCIL, compress = 'xz', overwrite = TRUE)
