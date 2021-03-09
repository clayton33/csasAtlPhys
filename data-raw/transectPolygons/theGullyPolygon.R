rm(list=ls())
library(usethis)
library(oce)
library(csasAtlPhys)
plot <- TRUE

# the gully coordinates
# obtained from cruise planning documents
# head and mouth are GULD respectively and west and east shelf are SG respectively
lon <- c(-59, -59.02, -58.9, -58.73)
lat <- c(43.71, 44, 43.79, 43.86)
names <- c('SG_28', 'GULD_03', 'GULD_04', 'SG_23')

a <- getTransectAngle(longitude = lon[c(2,3)], latitude = lat[c(2,3)])
angle <- a$angle

a2 <- getTransectAngle(longitude = lon[c(1,4)], latitude = lat[c(1,4)])
angle2 <- a2$angle


# 1. Find new start and end points
# between head and mouth
startlon <- lon[2]
startlat <- lat[2]
endlon <- lon[3]
endlat <- lat[3]
zone <- lonlat2utm(longitude = startlon,
                   latitude = startlat)$zone
startutm <- lonlat2utm(longitude = startlon,
                       latitude = startlat, zone = zone)
endutm <- lonlat2utm(longitude = endlon,
                     latitude = endlat,
                     zone = zone)
#utm output in m, so do calc in m
dist <- 8 * 1000
angleadj <- 45 + 0:3 * 90
eastingadj <- cos(angleadj * pi/180)
northingadj <- sin(angleadj * pi/180)
ptnorthing <- dist * eastingadj
pteasting <- dist * northingadj
pteastingrotate <- pteasting * cos(angle * pi/180) - ptnorthing * sin(angle * pi/180)
ptnorthingrotate <- pteasting * sin(angle * pi/180) + ptnorthing * cos(angle * pi/180)

# use index 3,4 from head and mouth for final polygon
head <- utm2lonlat(easting = startutm$easting + pteastingrotate,
                           northing = startutm$northing + ptnorthingrotate,
                           zone = zone)
mouth <- utm2lonlat(easting = endutm$easting + pteastingrotate,
                    northing = endutm$northing + ptnorthingrotate,
                    zone = zone)

## leg 2
# 1. Find new start and end points
startlon <- lon[1] #endlonlat$longitude
startlat <- lat[1] #endlonlat$latitude
endlon <- lon[4]
endlat <- lat[4]
zone <- lonlat2utm(longitude = startlon,
                   latitude = startlat)$zone
startutm <- lonlat2utm(longitude = startlon,
                       latitude = startlat, zone = zone)
endutm <- lonlat2utm(longitude = endlon,
                     latitude = endlat,
                     zone = zone)
#utm output in m, so do calc in m
dist <- 8 * 1000
# use the same pteasting and ptnorthing as leg 1
pteastingrotate <- pteasting * cos(angle2 * pi/180) - ptnorthing * sin(angle2 * pi/180)
ptnorthingrotate <- pteasting * sin(angle2 * pi/180) + ptnorthing * cos(angle2 * pi/180)

west <- utm2lonlat(easting = startutm$easting + pteastingrotate,
                   northing = startutm$northing + ptnorthingrotate,
                   zone = zone)
east <- utm2lonlat(easting = endutm$easting + pteastingrotate,
                    northing = endutm$northing + ptnorthingrotate,
                    zone = zone)

polyx <- c(west$longitude[c(4,3)],
           east$longitude[c(2,1)],
           mouth$longitude[4],
           head$longitude[c(4,3)],
           mouth$longitude[3])
polyy <- c(west$latitude[c(4,3)],
           east$latitude[c(2,1)],
           mouth$latitude[4],
           head$latitude[c(4,3)],
           mouth$latitude[3])

theGullyPolygon <- list(longitude = polyx,
                          latitude = polyy)

usethis::use_data(theGullyPolygon, compress = 'xz', overwrite = TRUE)


# next calculate the station polygons using same methods as above.
dist <- NULL
for(i in c(1,3,4)){
  stndist <- geodDist(longitude1 = lon[i], latitude1 = lat[i],
                      longitude2 = lon[(i+1)], latitude2 = lat[(i+1)])
  dist <- c(dist, stndist)
}
distheight <- dist/2
distheight <- c(head(distheight,1), distheight, tail(distheight,1))
distheight[distheight > 8] <- 8
distwidth <- 8
distheight <- 7.8
distheight <- distheight * 1000
distwidth <- distwidth * 1000
stnpoly <- vector(mode = 'list', length = length(lon))
for(i in 1:length(lon)){
  zone <- lonlat2utm(longitude = lon[i],
                     latitude = lat[i])$zone
  ptutm <- lonlat2utm(longitude = lon[i],
                      latitude = lat[i],
                      zone = zone)
  ang <- ifelse(i %in% c(1,4), angle2, angle)
  angleadj <- 45 + 0:3 * 90
  eastingadj <- cos(angleadj * pi/180)
  northingadj <- sin(angleadj * pi/180)
  ptnorthing <- distwidth * eastingadj
  pteasting <- distheight * northingadj
  pteastingrotate <- pteasting * cos(ang * pi/180) - ptnorthing * sin(ang * pi/180)
  ptnorthingrotate <- pteasting * sin(ang * pi/180) + ptnorthing * cos(ang * pi/180)
  cornerlonlat <- utm2lonlat(easting = ptutm$easting + pteastingrotate,
                             northing = ptutm$northing + ptnorthingrotate,
                             zone = zone)
  stnpoly[[i]][['stationName']] <- names[i]
  stnpoly[[i]][['longitude']] <- lon[i]
  stnpoly[[i]][['latitude']] <- lat[i]
  stnpoly[[i]][['polyLongitude']] <- cornerlonlat$longitude
  stnpoly[[i]][['polyLatitude']] <- cornerlonlat$latitude
  stnpoly[[i]][['transectName']] <- 'theGully'
}

theGullyStationPolygons <- stnpoly
usethis::use_data(theGullyStationPolygons, compress = 'xz', overwrite = TRUE)

# for debugging purposes to double check polygon and station polygons
if(plot){
  library(ocedata)
  data("coastlineWorldFine")
  proj <- '+proj=merc'
  fillcol <- 'lightgrey'
  lonlim <- range(c(lon, polyx)) + c(-0.5, 0.5)
  latlim <- range(c(lat, polyy)) + c(-0.5, 0.5)

  #png('00_cabotStraitPolygon.png', width = 6, height = 4, unit = 'in', res = 200, pointsize = 12)
  par(mar = c(3.5, 3.5, 1, 1))
  mapPlot(coastlineWorldFine,
          longitudelim = lonlim,
          latitudelim = latlim,
          col = fillcol,
          proj = proj,
          grid = c(2,1))
  mapPoints(lon, lat, pch = 20, col = 'black')
  #mapText(lon, lat, labels = names, pos = 3)

  #mapPoints(startlon, startlat, col = 'red', pch = 20)
  #mapPoints(endlon, endlat, col = 'red', pch = 20)
  mapPolygon(polyx, polyy)
  lapply(stnpoly, function(k) mapPolygon(k[['polyLongitude']], k[['polyLatitude']], border = 'red'))
  #dev.off()
}
