file <- 'data-raw/P5_1981t2010_Means.csv'
prince5Climatology1980to2010 <- read.table(file, skip = 1, header = TRUE, sep = ",")
usethis::use_data(prince5Climatology1980to2010, compress = "xz", overwrite = T)
