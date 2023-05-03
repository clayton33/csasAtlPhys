rm(list=ls())
library(usethis)

# load in PSG's NAFO boundary definitions
bndryfiles <- list.files(path = 'data-raw/nafo/psgSSTpolygons', pattern = '.*\\.dat', full.names = TRUE)
psgZoneNames <- gsub(pattern = '(.*)\\.dat', replacement = '\\1', x = basename(bndryfiles))
psgnafo <- vector(mode = 'list', length = length(bndryfiles))
sstSatelliteNafoZones <- vector(mode = 'list', length = length(bndryfiles))
for(i in 1:length(bndryfiles)){
  rl <- readLines(bndryfiles[i])
  ss <- strsplit(rl, split = '\\s+')
  ss <- lapply(ss, as.numeric)
  df <- as.data.frame(do.call('rbind', ss))
  names(df) <- c('latitude', 'longitude')

  sstSatelliteNafoZones[[i]]$zoneName <- psgZoneNames[i]
  sstSatelliteNafoZones[[i]]$longitude <- df[['longitude']]
  sstSatelliteNafoZones[[i]]$latitude <- df[['latitude']]
}

usethis::use_data(sstSatelliteNafoZones, compress = "xz", overwrite = T)
