rm(list=ls())
library(usethis)
load('data-raw/winterGroundfish/Winter_Groundfish_1981-2010_Means.RData')
names(lt_means) <- tolower(names(lt_means))
winterGroundfishClimatology1980to2010 <- lt_means
usethis::use_data(winterGroundfishClimatology1980to2010, compress = 'xz', overwrite = TRUE)
