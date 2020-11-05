rm(list=ls())
library(usethis)

# limits from stn2_contour_bluered_Dec16.m
limits <- list(temperature = c(-2, 20),
               temperatureAnomaly = c(-6,6),
               salinity = c(29, 35),
               salinityAnomaly = c(-1.5, 1.5),
               sigmaTheta = c(21, 27),
               sigmaThetaAnomaly = c(-1.5, 1.5))
# contourlevels
contourLevels <- list(temperature = c(seq(-2, 4, 1), seq(6, 20, 2)),
                      temperatureAnomaly = seq(-6,6,1),
                      salinity = seq(29, 36, 0.5),
                      salinityAnomaly = c(seq(-1.5, 1.5, 0.5)),
                      sigmaTheta = seq(21,27,0.5),
                      sigmaThetaAnomaly = seq(-1.5, 1.5, 0.25))
# contour level limits for jet palette
contourLevelLimits <- list(temperature = c(2,18),
                           temperatureAnomaly = c(-5,5),
                           salinity = c(30.5,34.5),
                           salinityAnomaly = c(-1.25, 1.25),
                           sigmaTheta = c(22, 26.5),
                           sigmaThetaAnomaly = c(-1.25, 1.25))
station2PlotLimits <- list(limits = limits,
                           contourLevels = contourLevels,
                           contourLevelLimits = contourLevelLimits)
usethis::use_data(station2PlotLimits, compress = 'xz', overwrite = TRUE)
