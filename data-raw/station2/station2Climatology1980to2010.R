library(usethis)
load('data-raw/station2/Halifax2_Big_1981-2010.RData')
station2Climatology1980to2010 <- ltm
newnames <- gsub('\\_ltm', '', tolower(names(station2Climatology1980to2010)))
names(station2Climatology1980to2010) <- newnames

usethis::use_data(station2Climatology1980to2010, compress = "xz", overwrite = T)
