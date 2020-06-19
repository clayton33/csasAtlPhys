rm(list=ls())
library(usethis)

# limits (from azmp_section_plot.m)
limits <- list(temperature = c(-1, 15),
               temperatureAnomaly = c(-3.5,5.5),
               salinity = c(29, 36),
               salinityAnomaly = c(-3.5, 5.5))
# contourlevels
contourLevels <- list(temperature = c(seq(-2,3,1), seq(14,20,2), seq(4,13,1)),
                      temperatureAnomaly = c(0, -1, seq(1,2,1), seq(-4, -2, 1), seq(3,4,1)),
                      salinity = c(seq(26,30,1), seq(30,35,1), seq(35.5, 37, 0.5)),
                      salinityAnomaly = c(0, seq(-.3, -.1, 0.1), seq(0.1, 0.3, 0.1), seq(-1.0, -0.4, 0.1), seq(0.4, 1.0, 0.1)))
# contour level limits for jet palette
contourLevelLimits <- list(temperature = c(3,14),
                           temperatureAnomaly = c(-2,3),
                           salinity = c(30,35.5),
                           salinityAnomaly = c(-0.4, 0.4))
winterGroundfishPlotLimits <- list(limits = limits,
                           contourLevels = contourLevels,
                           contourLevelLimits = contourLevelLimits)
usethis::use_data(winterGroundfishPlotLimits, compress = 'xz', overwrite = TRUE)
