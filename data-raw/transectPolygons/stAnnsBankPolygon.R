rm(list=ls())
library(usethis)
library(oce)
library(csasAtlPhys)
plot <- FALSE

# st.anns bank stations, STAB 1-6, STAB05.3
lon <- c(-59.85, -59.533, -59.365, -59.195, -59.065, -58.8833, -58.4361, -58.74)
lat <- c(45.825, 46.00, 46.1083, 46.2167, 46.3, 46.4167, 46.7105, 46.5)
nums <- c(unlist(lapply(1:6, function(k) ifelse(k < 10, paste0(0,k), k))), "05.3")
names <- c('LL_01', paste('STAB', nums, sep = '_'))
o <- order(lon)
lon <- lon[o]
lat <- lat[o]
nums <- nums[o] # don't think this is used elsewhere, but for consistency
names <- names[o]

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
stabpolyx <- c(startcorner$longitude, endcorner$longitude, startcorner$longitude[1])
stabpolyy <- c(startcorner$latitude, endcorner$latitude, startcorner$latitude[1])
stAnnsBankPolygon <- list(longitude = stabpolyx,
                          latitude = stabpolyy)
usethis::use_data(stAnnsBankPolygon, compress = 'xz', overwrite = TRUE)

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
stnpoly <- vector(mode = 'list', length = length(lon)-1)
cnt <- 1
for(i in 2:length(lon)){ # LL_01 will be covered in louisbourgPolygon.R
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
  stnpoly[[cnt]][['stationName']] <- names[i]
  stnpoly[[cnt]][['longitude']] <- lon[i]
  stnpoly[[cnt]][['latitude']] <- lat[i]
  stnpoly[[cnt]][['polyLongitude']] <- cornerlonlat$longitude
  stnpoly[[cnt]][['polyLatitude']] <- cornerlonlat$latitude
  stnpoly[[cnt]][['transectName']] <- 'stAnnsBank'
  cnt <- cnt + 1
}
stAnnsBankStationPolygons <- stnpoly
usethis::use_data(stAnnsBankStationPolygons, compress = 'xz', overwrite = TRUE)


# used for debugging purposes
if(plot){
library(ocedata)
data(coastlineWorldFine)
proj <- '+proj=merc'
fillcol <- 'lightgrey'
lonlim <- range(c(lon, startcorner$longitude, endcorner$longitude)) + c(-0.5, 0.5)
latlim <- range(c(lat, startcorner$latitude, startcorner$latitude)) + c(-0.5, 0.5)

#png('00_stAnnsBankPolygon.png', width = 6, height = 4, unit = 'in', res = 200, pointsize = 12)
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
# dev.off()
}
