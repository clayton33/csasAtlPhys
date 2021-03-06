rm(list=ls())
library(usethis)
library(oce)
library(csasAtlPhys)
plot <- FALSE

# north each channel coordinates
# obtained from on of the cruises
# coordinates from east to west
neclon <- c(-65.7448, -65.8065, -65.8393, -65.8705, -65.9027, -65.9380, -65.9682, -66.0325, -66.0815, -66.1413)
neclat <- c(42.4195, 42.3357, 42.2942, 42.2715, 42.2363, 42.1997, 42.1635, 42.1267, 42.0628, 41.9878)
nums <- unlist(lapply(1:10, function(k) ifelse(k < 10, paste0(0,k), k)))
necnames <- paste('NEC', nums, sep = '_')

a <- getTransectAngle(longitude = neclon, latitude = neclat)
angle <- a$angle


# 1. Find new start and end points
neceastlon <- neclon[1]
neceastlat <- neclat[1]
necwestlon <- neclon[length(neclon)]
necwestlat <- neclat[length(neclon)]
zone <- lonlat2utm(longitude = neceastlon,
                   latitude = neceastlat)$zone
startutm <- lonlat2utm(longitude = necwestlon,
                     latitude = necwestlat, zone = zone)
endutm <- lonlat2utm(longitude = neceastlon,
                     latitude = neceastlat,
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

necpolyx <- c(startcorner$longitude, endcorner$longitude, startcorner$longitude[1])
necpolyy <- c(startcorner$latitude, endcorner$latitude, startcorner$latitude[1])

northeastChannelPolygon <- list(longitude = necpolyx,
                                latitude = necpolyy)
usethis::use_data(northeastChannelPolygon, compress = 'xz', overwrite = TRUE)

# next calculate the station polygons using same methods as above
# since the stations are closer together, i'll be using 4km instead of 8km
dist <- NULL
for(i in 1:(length(neclon)-1)){
        stndist <- geodDist(longitude1 = neclon[i], latitude1 = neclat[i],
                            longitude2 = neclon[(i+1)], latitude2 = neclat[(i+1)])
        dist <- c(dist, stndist)
}
distheight <- dist/2
distheight <- c(head(distheight,1), distheight, tail(distheight,1))
distheight[distheight > 8] <- 8
distwidth <- 8
distheight <- distheight * 1000
distwidth <- distwidth * 1000
stnpoly <- vector(mode = 'list', length = length(neclon))
for(i in 1:length(neclon)){
        zone <- lonlat2utm(longitude = neclon[i],
                           latitude = neclat[i])$zone
        ptutm <- lonlat2utm(longitude = neclon[i],
                            latitude = neclat[i])
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
        stnpoly[[i]][['stationName']] <- necnames[i]
        stnpoly[[i]][['longitude']] <- neclon[i]
        stnpoly[[i]][['latitude']] <- neclat[i]
        stnpoly[[i]][['polyLongitude']] <- cornerlonlat$longitude
        stnpoly[[i]][['polyLatitude']] <- cornerlonlat$latitude
        stnpoly[[i]][['transectName']] <- 'northeastChannel'

}


northeastChannelStationPolygons <- stnpoly
usethis::use_data(northeastChannelStationPolygons, compress = 'xz', overwrite = TRUE)

if(plot){
        library(ocedata)
        data("coastlineWorldFine")
proj <- '+proj=merc'
fillcol <- 'lightgrey'
lonlim <- range(c(neclon, startcorner$longitude, endcorner$longitude)) + c(-0.5, 0.5)
latlim <- range(c(neclat, startcorner$latitude, startcorner$latitude)) + c(-0.5, 0.5)

#png('00_necPolygon.png', width = 6, height = 4, unit = 'in', res = 200, pointsize = 12)
par(mar = c(3.5, 3.5, 1, 1))
mapPlot(coastlineWorldFine,
        longitudelim = lonlim,
        latitudelim = latlim,
        col = fillcol,
        proj = proj,
        grid = c(2,1))
mapPoints(neclon, neclat, pch = 20, col = 'black')
mapPoints(neceastlon, neceastlat, col = 'red', pch = 20)
mapPoints(necwestlon, necwestlat, col = 'red', pch = 20)
mapPolygon(c(startcorner$longitude, endcorner$longitude),
           c(startcorner$latitude, endcorner$latitude))
lapply(stnpoly, function(k) mapPolygon(k[['polyLongitude']], k[['polyLatitude']], border = 'red'))

#dev.off()
}

