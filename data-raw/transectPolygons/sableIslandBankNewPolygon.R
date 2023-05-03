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


# SIBnew04 to SIBnew01
lon <- rev(c(-61.29704285,
             -61.35416443,
             -61.45608795,
             -61.50390625))
lat <- rev(c(42.96304703,
             43.22916637,
             43.54095459,
             43.8634697))
# define station names
nums <- unlist(lapply(4:1, function(k) ifelse(k < 10, paste0(0,k), k)))
names <- paste('SIBnew', nums, sep = '_')

# get angle of transect
a <- getTransectAngle(longitude = lon, latitude = lat)
angle <- a$angle

# 1. Find new start and end points
westlon <- lon[1]
westlat <- lat[1]
eastlon <- lon[length(lon)]
eastlat <- lat[length(lon)]
zone <- lonlat2utm(longitude = eastlon,
                   latitude = eastlat)$zone
startutm <- lonlat2utm(longitude = westlon,
                       latitude = westlat, zone = zone)
endutm <- lonlat2utm(longitude = eastlon,
                     latitude = eastlat,
                     zone = zone)
#utm output in m, so do calc in m
dist <- 8 * 1000
starteastingadd <- dist * cos((angle + 180) * pi/180)
startnorthingadd <- dist * sin((angle + 180) * pi/180)
endeastingadd <- dist * cos(angle * pi/180)
endnorthingadd <- dist * sin(angle * pi/180)

startlonlat <- utm2lonlat(easting = startutm$easting + starteastingadd,
                          northing = startutm$northing + startnorthingadd,
                          zone = zone)
endlonlat <- utm2lonlat(easting = endutm$easting + endeastingadd,
                        northing = endutm$northing + endnorthingadd,
                        zone = zone)
# find the corner points of start
eastingaddcorner <- c(dist * cos((angle + 90) * pi/180),
                      dist * cos((angle + 270) * pi/180))
northingaddcorner <- c(dist * sin((angle + 90) * pi/180),
                       dist * sin((angle + 270) * pi/180))

startcorner <- utm2lonlat(easting = startutm$easting + starteastingadd + eastingaddcorner,
                          northing = startutm$northing + startnorthingadd + northingaddcorner,
                          zone = zone)

# find corner points of end
# note that I'm going counterclockwise around the polygon
endcorner <- utm2lonlat(easting = endutm$easting + endeastingadd + rev(eastingaddcorner),
                        northing = endutm$northing + endnorthingadd + rev(northingaddcorner),
                        zone = zone)
llpolyx <- c(startcorner$longitude, endcorner$longitude, startcorner$longitude[1])
llpolyy <- c(startcorner$latitude, endcorner$latitude, startcorner$latitude[1])
sableIslandBankNewPolygon <- list(longitude = llpolyx,
                                  latitude = llpolyy)
usethis::use_data(sableIslandBankNewPolygon, compress = 'xz', overwrite = TRUE)

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
  stnpoly[[i]][['transectName']] <- 'sableIslandBankNew'

}
sableIslandBankNewStationPolygons <- stnpoly
usethis::use_data(sableIslandBankNewStationPolygons, compress = 'xz', overwrite = TRUE)


# # used for debugging purposes
if(plot){
  library(ocedata)
  data(coastlineWorldFine)
  proj <- '+proj=merc'
  fillcol <- 'lightgrey'
  lonlim <- range(c(lon, startcorner$longitude, endcorner$longitude)) + c(-0.5, 0.5)
  latlim <- range(c(lat, startcorner$latitude, startcorner$latitude)) + c(-0.5, 0.5)

  #png('00_sableIslandBankPolygon.png', width = 6, height = 4, unit = 'in', res = 200, pointsize = 12)
  par(mar = c(3.5, 3.5, 1, 1))
  mapPlot(coastlineWorldFine,
          longitudelim = lonlim,
          latitudelim = latlim,
          col = fillcol,
          proj = proj,
          grid = c(2,1))
  mapPoints(lon, lat, pch = 20, col = 'black')
  mapPoints(eastlon, eastlat, col = 'red', pch = 20)
  mapPoints(westlon, westlat, col = 'red', pch = 20)
  mapPolygon(c(startcorner$longitude, endcorner$longitude),
             c(startcorner$latitude, endcorner$latitude))
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
sableIslandBankNewTransectDef <- list(info = data.frame(start_latitude = startLatitude,
                                           start_longitude = startLongitude,
                                           yaxis_max = 300), # need to think about yaxis_max, initially 100
                         bottom_outline = bottomOutline)

save(sableIslandBankNewTransectDef, file = './data-raw/transects/sableIslandBankNewTransectDefinition.RData')

