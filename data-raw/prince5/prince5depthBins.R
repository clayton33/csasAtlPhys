library(usethis)
std <- c(0, 10, 25, 50, 75, 90) # not the same as HL2
tol <- c(5, 5, 5, 10, 10, 10)

prince5depthBins <- data.frame(bin = std, tolerance = tol)
usethis::use_data(prince5depthBins, compress = 'xz', overwrite = T)
