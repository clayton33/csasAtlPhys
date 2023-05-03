rm(list=ls())
library(usethis)
library(oce)
library(csasAtlPhys)
library(fields)
topoFile <- download.topo(west = -75, east = -50,
                          south = 38, north = 50,
                          resolution = 1,
                          destdir = './data-raw/transects')
ocetopo <- read.topo(topoFile)
plot <- TRUE


# SIBwe05, SIBwe09 to SIBwe11
lon <- c(-61.18676453,
         -60.84057999,
         -60.39583118,
         -59.77083123)
lat <- c(43.65282745,
         43.99490128,
         44.14583325,
         44.18749993)
# define station names
nums <- unlist(lapply(c(5,9:11), function(k) ifelse(k < 10, paste0(0,k), k)))
names <- paste('SIBwe', nums, sep = '_')

# NO TRANSECT POLYGON DUE TO SHAPE

# next calculate the station polygons using same methods as above
dist <- NULL
for(i in 1:(length(lon)-1)){
  stndist <- geodDist(longitude1 = lon[i], latitude1 = lat[i],
                      longitude2 = lon[(i+1)], latitude2 = lat[(i+1)])
  dist <- c(dist, stndist)
}
distheight <- dist/2
distheight <- c(head(distheight,1), distheight, tail(distheight,1))
distheight[distheight > 8] <- 8
distwidth <- 8
distheight <- distheight * 1000
distwidth <- distwidth * 1000
stnpoly <- vector(mode = 'list', length = length(lon))
for(i in 1:length(lon)){
  # calculate the angle between points
  if(i == length(lon)){
    angleidx <- (i-1):i
  } else {
    angleidx <- i:(i+1)
  }
  a <- getTransectAngle(longitude = lon[angleidx], latitude = lat[angleidx])
  angle <- a$angle
  zone <- lonlat2utm(longitude = lon[i],
                     latitude = lat[i])$zone
  ptutm <- lonlat2utm(longitude = lon[i],
                      latitude = lat[i])
  angleadj <- 45 + 0:3 * 90
  eastingadj <- cos(angleadj * pi/180)
  northingadj <- sin(angleadj * pi/180)
  ptnorthing <- (distwidth * eastingadj)
  pteasting <- c(distheight[(i+1)], distheight[(i+1)],distheight[i], distheight[i]) * northingadj
  pteastingrotate <- pteasting * cos(angle * pi/180) - ptnorthing * sin(angle * pi/180)
  ptnorthingrotate <- pteasting * sin(angle * pi/180) + ptnorthing * cos(angle * pi/180)
  cornerlonlat <- utm2lonlat(easting = ptutm$easting + pteastingrotate,
                             northing = ptutm$northing + ptnorthingrotate,
                             zone = zone)
  stnpoly[[i]][['stationName']] <- names[i]
  stnpoly[[i]][['longitude']] <- lon[i]
  stnpoly[[i]][['latitude']] <- lat[i]
  stnpoly[[i]][['polyLongitude']] <- cornerlonlat$longitude
  stnpoly[[i]][['polyLatitude']] <- cornerlonlat$latitude
  stnpoly[[i]][['transectName']] <- 'sableIslandBankWestEast'

}
sableIslandBankWestEastStationPolygons <- stnpoly
usethis::use_data(sableIslandBankWestEastStationPolygons, compress = 'xz', overwrite = TRUE)


# # used for debugging purposes
if(plot){
  library(ocedata)
  data(coastlineWorldFine)
  proj <- '+proj=merc'
  fillcol <- 'lightgrey'
  lonlim <- range(c(lon)) + c(-0.5, 0.5)
  latlim <- range(c(lat)) + c(-0.5, 0.5)

  #png('00_sableIslandBankPolygon.png', width = 6, height = 4, unit = 'in', res = 200, pointsize = 12)
  par(mar = c(3.5, 3.5, 1, 1))
  mapPlot(coastlineWorldFine,
          longitudelim = lonlim,
          latitudelim = latlim,
          col = fillcol,
          proj = proj,
          grid = c(2,1))
  mapPoints(lon, lat, pch = 20, col = 'black')
  # mapPoints(eastlon, eastlat, col = 'red', pch = 20)
  # mapPoints(westlon, westlat, col = 'red', pch = 20)
  # mapPolygon(c(startcorner$longitude, endcorner$longitude),
  #            c(startcorner$latitude, endcorner$latitude))
  lapply(stnpoly, function(k) mapPolygon(k[['polyLongitude']], k[['polyLatitude']], border = 'red'))
  #mapText(lon, lat, labels = names, pos = 3)
  #dev.off()
}

# get new definition polygon
bottomlon <- bottomlat <- NULL
for(i in 1:(length(lon)-1)){
  a <- getTransectAngle(longitude = lon[c(i, i+1)],
                        latitude = lat[c(i, i+1)])
  angle <- a$angle
  startlon <- lon[i]
  startlat <- lat[i]
  endlon <- lon[(i+1)]
  endlat <- lat[(i+1)]
  zone <- lonlat2utm(longitude = startlon,
                     latitude = startlat)$zone
  startutm <- lonlat2utm(longitude = startlon,
                         latitude = startlat, zone = zone)
  endutm <- lonlat2utm(longitude = endlon,
                       latitude = endlat,
                       zone = zone)
  # save values for inferring bottom outline
  df <- data.frame(easting = c(startutm$easting, endutm$easting),
                   northing = c(startutm$northing, endutm$northing))
  lm <- lm(northing ~ easting, data = df)
  nstns <- 16
  dd <- diff(df$easting)/(nstns + 1) # nstns of points but have to add 1 for spacing
  eastingnew <- (dd * 1:nstns) + startutm$easting
  northingnew <- predict(lm, newdata = data.frame(easting = eastingnew))
  bottomlonlat <- utm2lonlat(easting = eastingnew, northing = northingnew, zone = zone)
  bottomlon <- c(bottomlon, bottomlonlat$lon)
  bottomlat <- c(bottomlat, bottomlonlat$lat)
}

# create a transect definition for sable Island Bank New
startLatitude <- lat[1]
startLongitude <- lon[1]

zs <- interp.surface(obj = list(x = ocetopo[['longitude']],
                                y = ocetopo[['latitude']],
                                z = ocetopo[['z']]),
                     loc = cbind(c(startLongitude, bottomlon),
                                 c(startLatitude, bottomlat)))
dist <- geodDist(longitude2 = startLongitude,
                 latitude2 = startLatitude,
                 longitude1 = bottomlon,
                 latitude1 = bottomlat)
dist <- c(0, dist)
plot(1:length(zs), zs, ylim = c(min(zs), 0))
bottomOutline <- data.frame(latitude = c(startLatitude, bottomlat),
                            longitude = c(startLongitude, bottomlon),
                            distance_km = dist,
                            elevation_m = zs)
sableIslandBankWestEastTransectDef <- list(info = data.frame(start_latitude = startLatitude,
                                                        start_longitude = startLongitude,
                                                        yaxis_max = 125), # need to think about yaxis_max, initially 125
                                      bottom_outline = bottomOutline)

save(sableIslandBankWestEastTransectDef, file = './data-raw/transects/sableIslandBankWestEastTransectDefinition.RData')
