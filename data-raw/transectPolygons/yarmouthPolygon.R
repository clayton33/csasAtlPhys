rm(list=ls())
library(usethis)
library(oce)
library(csasAtlPhys)

# yarmouth, nova scotia (PL01) to portsmouth, new hampshire (PL10)
lon <- c(-70.2724, -70.0098, -69.5574, -69.1064, -68.6643, -68.2124, -67.7534, -67.3023, -66.8513, -66.4003)
lat <- c(43.1567, 43.1863, 43.2579, 43.3283, 43.3986, 43.4689, 43.5392, 43.6096, 43.6799, 43.7502)

nums <- unlist(lapply(10:1, function(k) ifelse(k < 10, paste0(0,k), k)))
names <- paste('YL', nums, sep = '_')

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
ylpolyx <- c(startcorner$longitude, endcorner$longitude, startcorner$longitude[1])
ylpolyy <- c(startcorner$latitude, endcorner$latitude, startcorner$latitude[1])
yarmouthPolygon <- list(longitude = ylpolyx,
                           latitude = ylpolyy)
usethis::use_data(yarmouthPolygon, compress = 'xz', overwrite = TRUE)

# next calculate the station polygons using same methods as above
stnpoly <- vector(mode = 'list', length = length(lon))
for(i in 1:length(lon)){
        zone <- lonlat2utm(longitude = lon[i],
                           latitude = lat[i])$zone
        ptutm <- lonlat2utm(longitude = lon[i],
                            latitude = lat[i])
        dist <- 8 * 1000
        eastingadd <- dist * c(cos((angle + 180) * pi/180), # start
                               cos(angle * pi/180)) # end
        northingadd <- dist * c(sin((angle + 180) * pi/180), # start
                                sin(angle * pi/180)) # end
        cornereasting <- c(dist * cos((angle + 90) * pi/180),
                           dist * cos((angle + 270) * pi/180))
        cornernorthing <- c(dist * sin((angle + 90) * pi/180),
                            dist * sin((angle + 270) * pi/180))

        pteasting <- c(ptutm$easting + eastingadd[1] + cornereasting,
                       ptutm$easting + eastingadd[2] + rev(cornereasting))
        ptnorthing <- c(ptutm$northing + northingadd[1] + cornernorthing,
                        ptutm$northing + northingadd[2] + rev(cornernorthing))
        cornerlonlat <- utm2lonlat(easting = pteasting,
                                   northing = ptnorthing,
                                   zone = zone)
        stnpoly[[i]][['stationName']] <- names[i]
        stnpoly[[i]][['longitude']] <- lon[i]
        stnpoly[[i]][['latitude']] <- lat[i]
        stnpoly[[i]][['polyLongitude']] <- cornerlonlat$longitude
        stnpoly[[i]][['polyLatitude']] <- cornerlonlat$latitude
        stnpoly[[i]][['transectName']] <- 'yarmouth'

}
yarmouthStationPolygons <- stnpoly
usethis::use_data(yarmouthStationPolygons, compress = 'xz', overwrite = TRUE)

# for debugging purposes to double check polygon and station polygons
# library(ocedata)
# data("coastlineWorldFine")
# proj <- '+proj=merc'
# fillcol <- 'lightgrey'
# lonlim <- range(c(lon, startcorner$longitude, endcorner$longitude)) + c(-0.5, 0.5)
# latlim <- range(c(lat, startcorner$latitude, startcorner$latitude)) + c(-0.5, 0.5)
#
# #png('00_cabotStraitPolygon.png', width = 6, height = 4, unit = 'in', res = 200, pointsize = 12)
# par(mar = c(3.5, 3.5, 1, 1))
# mapPlot(coastlineWorldFine,
#         longitudelim = lonlim,
#         latitudelim = latlim,
#         col = fillcol,
#         proj = proj,
#         grid = c(2,1))
# mapPoints(lon, lat, pch = 20, col = 'black')
# mapPoints(eastlon, eastlat, col = 'red', pch = 20)
# mapPoints(westlon, westlat, col = 'red', pch = 20)
# mapPolygon(c(startcorner$longitude, endcorner$longitude),
#            c(startcorner$latitude, endcorner$latitude))
# lapply(stnpoly, function(k) mapPolygon(k[['polyLongitude']], k[['polyLatitude']], border = 'red'))
# #dev.off()
