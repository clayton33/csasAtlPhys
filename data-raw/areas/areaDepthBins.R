library(usethis)
# define standard depths and tolerance
std <- c(seq(0,30,10),
         seq(50,250,25),
         seq(300,800,100),
         seq(1000,1200,200),
         seq(1500,10000,500))
tol <- c(rep(5, length(std[std <= 75])),
         rep(10, length(std[std >= 100 & std <= 250])),
         rep(25, length(std[std >= 300 & std <= 400])),
         rep(50, length(std[std >= 500 & std <= 800])),
         rep(100, length(std[std >= 1000 & std <= 1500])),
         rep(200, length(std[std >= 2000])))
areaDepthBins <- data.frame(bin = std, tolerance = tol)
usethis::use_data(areaDepthBins, compress = "xz", overwrite = T)

