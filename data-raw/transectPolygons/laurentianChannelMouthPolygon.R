rm(list=ls())
library(usethis)
library(oce)
library(csasAtlPhys)
plot <- FALSE

# across mouth of laurentian channel west to east
lon <- c(-57.6549, -57.4746, -57.3486, -57.25, -57.0267, -56.8077, -56.63, -56.44, -56.14, -56.03)
lat <- c(44.7196, 44.7448, 44.7623, 44.78, 44.8088, 44.8476, 44.89, 44.92, 44.98, 45)

nums <- unlist(lapply(1:10, function(k) ifelse(k < 10, paste0(0,k), k)))
names <- paste('LCM', nums, sep = '_')

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
lcmpolyx <- c(startcorner$longitude, endcorner$longitude, startcorner$longitude[1])
lcmpolyy <- c(startcorner$latitude, endcorner$latitude, startcorner$latitude[1])
laurentianChannelMouthPolygon <- list(longitude = lcmpolyx,
                           latitude = lcmpolyy)
usethis::use_data(laurentianChannelMouthPolygon, compress = 'xz', overwrite = TRUE)

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
        stnpoly[[i]][['transectName']] <- 'laurentianChannelMouth'

}
laurentianChannelMouthStationPolygons <- stnpoly
usethis::use_data(laurentianChannelMouthStationPolygons, compress = 'xz', overwrite = TRUE)

# for debugging purposes to double check polygon and station polygons
if(plot){
library(ocedata)
data("coastlineWorldFine")
proj <- '+proj=merc'
fillcol <- 'lightgrey'
lonlim <- range(c(lon, startcorner$longitude, endcorner$longitude)) + c(-0.5, 0.5)
latlim <- range(c(lat, startcorner$latitude, startcorner$latitude)) + c(-0.5, 0.5)

#png('00_cabotStraitPolygon.png', width = 6, height = 4, unit = 'in', res = 200, pointsize = 12)
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
#dev.off()
}
