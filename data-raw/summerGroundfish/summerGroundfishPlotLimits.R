rm(list=ls())
library(usethis)

# limits
limits <- list(temperature = c(-1, 15),
               temperatureAnomaly = c(-3.5,-3.5),
               salinity = c(29, 36),
               salinityAnomaly = c(-3.5, 3.5)
               #sigmaTheta = c(22, 28),
               #sigmaThetaAnomaly = c(-3, 3)
               )
# contourlevels
contourLevels <- list(temperature = c(seq(-2,3,1), seq(14,20,2), seq(4,13,1)),
                      temperatureAnomaly = c(0, -1, seq(1,2,1), seq(-4, -2, 1), seq(3,4,1)),
                      salinity = c(seq(26,30,1), seq(30,35,1), seq(35.5, 37, 0.5)),
                      salinityAnomaly = c(0, seq(-.3, -.1, 0.1), seq(0.1, 0.3, 0.1), seq(-1.0, -0.4, 0.1), seq(0.4, 1.0, 0.1))
                      #sigmaTheta = c(seq(22, 26, 1), seq(26.5, 28, 0.5)),
                      #sigmaThetaAnomaly = c(seq(-3, 3, 0.5))
                      )
# contour level limits for jet palette / anomaly palette
contourLevelLimits <- list(temperature = c(2,14),
                           temperatureAnomaly = c(-2,3),
                           salinity = c(30,35.5),
                           salinityAnomaly = c(-0.4, 0.4)
                           #sigmaTheta = c(23, 27.5),
                           #sigmaThetaAnomaly = c(-2.5, 2.5)
                           )
summerGroundfishPlotLimits <- list(limits = limits,
                           contourLevels = contourLevels,
                           contourLevelLimits = contourLevelLimits)
usethis::use_data(summerGroundfishPlotLimits, compress = 'xz', overwrite = TRUE)
