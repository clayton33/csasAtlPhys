library(usethis)
transectDistanceBins <- list('halifaxInshore' = data.frame(bin = c(0, 40, 85, 135, 185, 235, 285),
                                                     tolerance = c(20, 20, 25, 25, 25, 25, 25)),
                             'cabotStrait' = data.frame(bin = c(0, 15, 25, 45, 75, 105),
                                                  tolerance = c(10, 5, 5, 15, 15, 15)),
                             'louisbourg' = list('fall' = data.frame(bin = c(0, 30, 60, 100, 140, 180, 230, 330),
                                                               tolerance = c(20, 10, 20, 20, 20, 20, 30, 70)),
                                                 'spring' = data.frame(bin = c(0, 30, 60, 100, 140, 180, 230, 280, 350),
                                                                 tolerance = c(20, 10, 20, 20, 20, 20, 30, 20, 50))),
                             'brownsBank' = data.frame(bin = c(0, 35, 65, 100, 130, 150, 170),
                                                 tolerance = c(20, 15, 15, 20, 10, 10, 10)),
                             'northEastChannel' = data.frame(bin = c(0, 10, 20, 30, 40, 50, 70),
                                                       tolerance = c(5, 5, 5, 5, 5, 5, 15)))
usethis::use_data(transectDistanceBins, compress = "xz", overwrite = T)
