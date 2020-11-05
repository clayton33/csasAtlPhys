rm(list=ls())
library(usethis)
load('data-raw/summerGroundfish/Polygon_Definitions.RData')
areas <- ls(pattern="pts_*")
areaNames <- c(sub("pts_","",areas), '4V')


maritimeNafoRegions <- vector(mode = 'list', length = length(areas)+1)
for(j in 1:length(areas)){
  area <- areas[j]
  eval(parse(text=paste0('pts <- ',area)))
  maritimeNafoRegions[[j]]$longitude <- pts$LONGITUDE
  maritimeNafoRegions[[j]]$latitude <- pts$LATITUDE
  maritimeNafoRegions[[j]]$zoneName <- areaNames[j]
}

# create area 4v by combining 4vs and 4vn
ok4vn <- which(unlist(lapply(maritimeNafoRegions, function(k) k[['zoneName']])) == '4vn')
ok4vs <- which(unlist(lapply(maritimeNafoRegions, function(k) k[['zoneName']])) == '4vs')
lon4vn <- maritimeNafoRegions[[ok4vn]]$longitude
lat4vn <- maritimeNafoRegions[[ok4vn]]$latitude
lon4vs <- maritimeNafoRegions[[ok4vs]]$longitude
lat4vs <- maritimeNafoRegions[[ok4vs]]$latitude
lon4v <- c(lon4vn[length(lat4vn):2],
           lon4vs[8:1],
           lon4vs[length(lat4vs):9],
           lon4vn[1])
lat4v <- c(lat4vn[length(lat4vn):2],
           lat4vs[8:1],
           lat4vs[length(lat4vs):9],
           lat4vn[1])
ok4v <- which(areaNames == '4V')
maritimeNafoRegions[[ok4v]]$longitude <- lon4v
maritimeNafoRegions[[ok4v]]$latitude <- lat4v
maritimeNafoRegions[[ok4v]]$zoneName <- '4V'

summerGroundfishOaxNafoPolygons <- maritimeNafoRegions

usethis::use_data(summerGroundfishOaxNafoPolygons, compress = 'xz', overwrite = TRUE)
