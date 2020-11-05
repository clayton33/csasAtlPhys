library(usethis)
# define standard depths and tolerance
std <- c(seq(0,30,10),seq(50,250,25))
tol <- c(rep(5, length(std[std <= 75])), rep(10, length(std[std >= 100 & std <= 250])))
station2DepthBins <- data.frame(bin = std, tolerance = tol)
usethis::use_data(station2DepthBins, compress = "xz", overwrite = T)
