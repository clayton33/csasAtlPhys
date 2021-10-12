rm(list=ls())
library(usethis)
library(oce)
library(csasAtlPhys)
plot <- TRUE

# extended halifax line stations, only 8-12, might be more.
hfxlon <- c(-61.339235,
            -61.250627,
            -61.068143,
            -60.906963,
            -60.664633)
hfxlat <- c(42.363113,
            42.253608,
            42.02608,
            41.77687,
            41.414172)

hfxnames <- paste('HL',unlist(lapply(8:12, function(k) ifelse(k < 10, paste0(0,k), k))) , sep = '_')

a <- getTransectAngle(longitude = hfxlon, latitude = hfxlat)
angle <- a$angle

# want it 4 km either side of of HL1 and HL7, and 4 km beyond it.

# 1. Find new start and end points
HL1lon <- hfxlon[1]
HL1lat <- hfxlat[1]
HL7lon <- hfxlon[length(hfxlon)]
HL7lat <- hfxlat[length(hfxlon)]
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

halifaxExtendedPolygon <- list(longitude = hlpolyx,
                       latitude = hlpolyy)
usethis::use_data(halifaxExtendedPolygon, compress = 'xz', overwrite = TRUE)

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
        stnpoly[[i]][['transectName']] <- 'halifaxExtended'

}
halifaxExtendedStationPolygons <- stnpoly
usethis::use_data(halifaxExtendedStationPolygons, compress = 'xz', overwrite = TRUE)



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


