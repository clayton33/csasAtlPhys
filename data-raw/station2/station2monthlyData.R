library(usethis)
load('data-raw/station2/Halifax2_Big_Monthly_TimeSeries.RData')
# reformat this to be similar to p5 so scripts can be easily used between the two stations
station2monthlyData <- data.frame(year = hfx2_monthly$YEAR,
                     month = hfx2_monthly$MONTH,
                     depth = hfx2_monthly$DEPTH,
                     temperature = hfx2_monthly$TEMPERATURE,
                     salinity = hfx2_monthly$SALINITY,
                     sigmaTheta = hfx2_monthly$SIGMAT)
usethis::use_data(station2monthlyData, compress = "xz", overwrite = T)
