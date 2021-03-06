rm(list=ls())
library(usethis)
library(oce)
library(csasAtlPhys)
plot <- FALSE

# browns bank line coordinates
# obtained from cruise planning documents
# north to south, leg part of BB are coordinates 6,7
bblon <- c(-65.4800, -65.4800, -65.483, -65.483, -65.5, -65.51, -65.350)
bblat <- c(43.250, 43, 42.760, 42.45, 42.133, 42, 41.867)
nums <- unlist(lapply(1:7, function(k) ifelse(k < 10, paste0(0,k), k)))
bbnames <- paste('BBL', nums, sep = '_')

a <- getTransectAngle(longitude = bblon[1:6], latitude = bblat[1:6])
angle <- a$angle

a2 <- getTransectAngle(longitude = bblon[6:7], latitude = bblat[6:7])
angle2 <- a2$angle


# 1. Find new start and end points
startlon <- bblon[1]
startlat <- bblat[1]
endlon <- bblon[6]
endlat <- bblat[6]
zone <- lonlat2utm(longitude = startlon,
                   latitude = startlat)$zone
startutm <- lonlat2utm(longitude = startlon,
                       latitude = startlat, zone = zone)
endutm <- lonlat2utm(longitude = endlon,
                     latitude = endlat,
                     zone = zone)
#utm output in m, so do calc in m
dist <- 8 * 1000
# this is opposite from other ones
endeastingadd <- dist * cos((angle + 180) * pi/180)
endnorthingadd <- dist * sin((angle + 180) * pi/180)
starteastingadd <- dist * cos(angle * pi/180)
startnorthingadd <- dist * sin(angle * pi/180)

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

# for final polygon
endcorneradapt <- utm2lonlat(easting = endutm$easting + rev(eastingaddcorner),
                        northing = endutm$northing+ rev(northingaddcorner),
                        zone = zone)

## leg 2
# 1. Find new start and end points
startlon <- bblon[6] #endlonlat$longitude
startlat <- bblat[6] #endlonlat$latitude
endlon <- bblon[7]
endlat <- bblat[7]
zone <- lonlat2utm(longitude = startlon,
                   latitude = startlat)$zone
startutm <- lonlat2utm(longitude = startlon,
                       latitude = startlat, zone = zone)
endutm <- lonlat2utm(longitude = endlon,
                     latitude = endlat,
                     zone = zone)
#utm output in m, so do calc in m
dist <- 8 * 1000

starteastingadd <- dist * cos((angle2 + 180) * pi/180)
startnorthingadd <- dist * sin((angle2 + 180) * pi/180)
endeastingadd <- dist * cos(angle2 * pi/180)
endnorthingadd <- dist * sin(angle2 * pi/180)

startlonlat <- utm2lonlat(easting = startutm$easting, #+ starteastingadd,
                          northing = startutm$northing, #+ startnorthingadd,
                          zone = zone)
endlonlat <- utm2lonlat(easting = endutm$easting + endeastingadd,
                        northing = endutm$northing + endnorthingadd,
                        zone = zone)
# find the corner points of start
eastingaddcorner <- c(dist * cos((angle2 + 90) * pi/180),
                      dist * cos((angle2 + 270) * pi/180))
northingaddcorner <- c(dist * sin((angle2 + 90) * pi/180),
                       dist * sin((angle2 + 270) * pi/180))

startcorner2 <- utm2lonlat(easting = startutm$easting + starteastingadd + eastingaddcorner,
                          northing = startutm$northing + startnorthingadd + northingaddcorner,
                          zone = zone)

# find corner points of end
# note that I'm going counterclockwise around the polygon
endcorner2 <- utm2lonlat(easting = endutm$easting + endeastingadd + rev(eastingaddcorner),
                        northing = endutm$northing + endnorthingadd + rev(northingaddcorner),
                        zone = zone)

bbpolyx <- c(startcorner$longitude, endcorneradapt$longitude[1],
              rev(endcorner2$longitude), endcorner$longitude[2], startcorner$longitude[1])
bbpolyy <- c(startcorner$latitude, endcorneradapt$latitude[1],
              rev(endcorner2$latitude), endcorner$latitude[2], startcorner$latitude[1])
brownsBankPolygon <- list(longitude = bbpolyx,
                          latitude = bbpolyy)

usethis::use_data(brownsBankPolygon, compress = 'xz', overwrite = TRUE)


# next calculate the station polygons using same methods as above.
dist <- NULL
for(i in 1:(length(bblon)-1)){
  stndist <- geodDist(longitude1 = bblon[i], latitude1 = bblat[i],
                      longitude2 = bblon[(i+1)], latitude2 = bblat[(i+1)])
  dist <- c(dist, stndist)
}
distheight <- dist/2
distheight <- c(head(distheight,1), distheight, tail(distheight,1))
distheight[distheight > 8] <- 8
distwidth <- 8
distheight <- distheight * 1000
distwidth <- distwidth * 1000
stnpoly <- vector(mode = 'list', length = length(bblon))
for(i in 1:length(bblon)){
  zone <- lonlat2utm(longitude = bblon[i],
                     latitude = bblat[i])$zone
  ptutm <- lonlat2utm(longitude = bblon[i],
                      latitude = bblat[i],
                      zone = zone)
  ang <- ifelse(i == 7, angle2, angle)
  angleadj <- 45 + 0:3 * 90
  eastingadj <- cos(angleadj * pi/180)
  northingadj <- sin(angleadj * pi/180)
  ptnorthing <- (distwidth * eastingadj)
  pteasting <- c(distheight[(i+1)], distheight[(i+1)],distheight[i], distheight[i]) * northingadj
  pteastingrotate <- pteasting * cos(ang * pi/180) - ptnorthing * sin(ang * pi/180)
  ptnorthingrotate <- pteasting * sin(ang * pi/180) + ptnorthing * cos(ang * pi/180)
  cornerlonlat <- utm2lonlat(easting = ptutm$easting + pteastingrotate,
                             northing = ptutm$northing + ptnorthingrotate,
                             zone = zone)
  stnpoly[[i]][['stationName']] <- bbnames[i]
  stnpoly[[i]][['longitude']] <- bblon[i]
  stnpoly[[i]][['latitude']] <- bblat[i]
  stnpoly[[i]][['polyLongitude']] <- cornerlonlat$longitude
  stnpoly[[i]][['polyLatitude']] <- cornerlonlat$latitude
  stnpoly[[i]][['transectName']] <- 'brownsBank'
}

brownsBankStationPolygons <- stnpoly
usethis::use_data(brownsBankStationPolygons, compress = 'xz', overwrite = TRUE)

# for debugging purposes to double check polygon and station polygons
if(plot){
  library(ocedata)
  data("coastlineWorldFine")
  proj <- '+proj=merc'
  fillcol <- 'lightgrey'
  lonlim <- range(c(bblon, startcorner$longitude, endcorner$longitude)) + c(-0.5, 0.5)
  latlim <- range(c(bblat, startcorner$latitude, startcorner$latitude)) + c(-0.5, 0.5)

  #png('00_cabotStraitPolygon.png', width = 6, height = 4, unit = 'in', res = 200, pointsize = 12)
  par(mar = c(3.5, 3.5, 1, 1))
  mapPlot(coastlineWorldFine,
          longitudelim = lonlim,
          latitudelim = latlim,
          col = fillcol,
          proj = proj,
          grid = c(2,1))
  mapPoints(bblon, bblat, pch = 20, col = 'black')
  mapPoints(startlon, startlat, col = 'red', pch = 20)
  mapPoints(endlon, endlat, col = 'red', pch = 20)
  mapPolygon(bbpolyx, bbpolyy)
  lapply(stnpoly, function(k) mapPolygon(k[['polyLongitude']], k[['polyLatitude']], border = 'red'))
  #dev.off()
}
