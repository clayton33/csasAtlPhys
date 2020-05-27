library(usethis)
load('data-raw/summerGroundfish/July_Groundfish_1981-2010_Means.RData')
names(lt_means) <- tolower(names(lt_means))
summerGroundfishClimatology1980to2010 <- lt_means
usethis::use_data(summerGroundfishClimatology1980to2010, compress = 'xz', overwrite = TRUE)
