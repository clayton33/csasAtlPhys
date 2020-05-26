depthBins <- c(seq(0, 90, 10),
               seq(100, 440, 20),
               seq(500, 3000, 100))
depthTol <- c(rep(5, length(which(depthBins < 100))),
              rep(10, length(which(depthBins >= 100 & depthBins < 500))),
              rep(50, length(which(depthBins >= 500))))

transectDepthBins <- data.frame(bin = depthBins, tolerance = depthTol)
usethis::use_data(transectDepthBins, compress = "xz", overwrite = T)
