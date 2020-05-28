rm(list=ls())
library(usethis)

# limits (from azmp_section_plot.m)
limits <- list(temperature = c(-1, 18),
               temperatureAnomaly = c(-6,6),
               salinity = c(29, 36.5),
               salinityAnomaly = c(-3, 3),
               sigmaTheta = c(22, 28),
               sigmaThetaAnomaly = c(-3, 3))
# contourlevels
contourLevels <- list(temperature = c(seq(-2, 6, 1), seq(8, 24, 2)),
                      temperatureAnomaly = c(seq(-6, -2, 2), seq(-1, 1, 1), seq(2, 6, 2)),
                      salinity = c(seq(29, 34, 1), seq(34.5, 36.5, 0.5)),
                      salinityAnomaly = c(seq(-3, 3, 0.5)),
                      sigmaTheta = c(seq(22, 26, 1), seq(26.5, 28, 0.5)),
                      sigmaThetaAnomaly = c(seq(-3, 3, 0.5)))
# contour level limits for jet palette
contourLevelLimits <- list(temperature = c(2,16),
                           temperatureAnomaly = c(-5,5),
                           salinity = c(30,36),
                           salinityAnomaly = c(-2.5, 2.5),
                           sigmaTheta = c(23, 27.5),
                           sigmaThetaAnomaly = c(-2.5, 2.5))
transectPlotLimits <- list(limits = limits,
                           contourLevels = contourLevels,
                           contourLevelLimits = contourLevelLimits)
usethis::use_data(transectPlotLimits, compress = 'xz', overwrite = TRUE)
