rm(list=ls())
library(usethis)
library(oce)
library(csasAtlPhys)
plot <- TRUE

# halifax line stations
# (HL01 - HL07, HL3.3, HL5.5, HL6.3, HL6.7)
hfxlon <- c(-63.450000, -63.317000, -62.883000, -62.451000, -62.098000, -61.733000, -61.393945, -62.7527, -61.8326, -61.6167, -61.5167)
hfxlat <- c(44.400001, 44.267001, 43.883001, 43.479000, 43.183000, 42.850000, 42.531138, 43.7635, 42.9402, 42.7333, 42.6183)
nums <- c(unlist(lapply(1:7, function(k) ifelse(k < 10, paste0(0,k), k))),
          3.3,
          5.5,
          6.3,
          6.7)
hfxnames <- paste('HL', nums, sep = '_')

a <- getTransectAngle(longitude = hfxlon[3:length(hfxlon)], latitude = hfxlat[3:length(hfxlon)])
angle <- a$angle

# want it 4 km either side of of HL1 and HL7, and 4 km beyond it.

# 1. Find new start and end points
HL1lon <- hfxlon[1]
HL1lat <- hfxlat[1]
HL7lon <- hfxlon[7]
HL7lat <- hfxlat[7]
zone <- lonlat2utm(longitude = HL1lon,
                   latitude = HL1lat)$zone
startutm <- lonlat2utm(longitude = HL1lon,
                     latitude = HL1lat, zone = zone)
endutm <- lonlat2utm(longitude = HL7lon,
                     latitude = HL7lat,
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
hlpolyx <- c(startcorner$longitude, endcorner$longitude, startcorner$longitude[1])
hlpolyy <- c(startcorner$latitude, endcorner$latitude, startcorner$latitude[1])

halifaxPolygon <- list(longitude = hlpolyx,
                       latitude = hlpolyy)
usethis::use_data(halifaxPolygon, compress = 'xz', overwrite = TRUE)

# next calculate the station polygons using same methods as above
dist <- NULL
for(i in 1:(length(hfxlon)-1)){
        stndist <- geodDist(longitude1 = hfxlon[i], latitude1 = hfxlat[i],
                            longitude2 = hfxlon[(i+1)], latitude2 = hfxlat[(i+1)])
        dist <- c(dist, stndist)
}
distheight <- dist/2
distheight <- c(head(distheight,1), distheight, tail(distheight,1))
distheight[distheight > 8] <- 8
distwidth <- 8
distheight <- distheight * 1000
distwidth <- distwidth * 1000

# HL_07 was moved at one point and is now slightly southwest of the original
# location. It is far away such that not all occupations will be caught
# so i'll add the second locations
hfxlon <- c(hfxlon, -61.4333)
hfxlat <- c(hfxlat, 42.475)
hfxnames <- c(hfxnames, 'HL_07')
distheight <- c(distheight, distwidth)
stnpoly <- vector(mode = 'list', length = length(hfxlon))
for(i in 1:length(hfxlon)){
        zone <- lonlat2utm(longitude = hfxlon[i],
                           latitude = hfxlat[i])$zone
        ptutm <- lonlat2utm(longitude = hfxlon[i],
                            latitude = hfxlat[i])
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
        stnpoly[[i]][['stationName']] <- hfxnames[i]
        stnpoly[[i]][['longitude']] <- hfxlon[i]
        stnpoly[[i]][['latitude']] <- hfxlat[i]
        stnpoly[[i]][['polyLongitude']] <- cornerlonlat$longitude
        stnpoly[[i]][['polyLatitude']] <- cornerlonlat$latitude
        stnpoly[[i]][['transectName']] <- 'halifaxInshore'

}
halifaxStationPolygons <- stnpoly
usethis::use_data(halifaxStationPolygons, compress = 'xz', overwrite = TRUE)



# plot code for debugging purposes.
if(plot){
        library(ocedata)
        data("coastlineWorldFine")
proj <- '+proj=merc'
fillcol <- 'lightgrey'
lonlim <- range(hfxlon)
latlim <- range(hfxlat)

#png('00_HLpolygon.png', width = 6, height = 4, unit = 'in', res = 200, pointsize = 12)
par(mar = c(3.5, 3.5, 1, 1))
mapPlot(coastlineWorldFine,
        longitudelim = lonlim,
        latitudelim = latlim,
        col = fillcol,
        proj = proj,
        grid = c(2,1))
mapPoints(hfxlon, hfxlat, pch = 20, col = 'black')
mapPoints(HL7lon, HL7lat, col = 'red', pch = 20)
mapPoints(HL1lon, HL1lat, col = 'red', pch = 20)
mapPolygon(c(startcorner$longitude, endcorner$longitude),
           c(startcorner$latitude, endcorner$latitude))
lapply(stnpoly, function(k) mapPolygon(k[['polyLongitude']], k[['polyLatitude']], border = 'red'))
mapPoints(-61.4333, 42.475, pch = 20, col = 'blue')
#dev.off()
}


