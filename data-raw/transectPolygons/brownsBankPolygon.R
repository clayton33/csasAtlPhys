rm(list=ls())
library(usethis)
library(oce)
library(csasAtlPhys)

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
stnpoly <- vector(mode = 'list', length = length(bblon))
for(i in 1:length(bblon)){
  zone <- lonlat2utm(longitude = bblon[i],
                     latitude = bblat[i])$zone
  ptutm <- lonlat2utm(longitude = bblon[i],
                      latitude = bblat[i],
                      zone = zone)
  dist <- 8 * 1000

  ang <- ifelse(i == 7, angle2, angle)
  eastingadd <- dist * c(cos((ang + 180) * pi/180), # end
                         cos(ang * pi/180)) # start
  northingadd <- dist * c(sin((ang + 180) * pi/180), # end
                          sin(ang * pi/180)) # start
  cornereasting <- dist * c(cos((ang + 90) * pi/180),
                            cos((ang + 270) * pi/180))
  cornernorthing <- dist * c(sin((ang + 90) * pi/180),
                             sin((ang + 270) * pi/180))

  pteasting <- c(ptutm$easting + eastingadd[2] + cornereasting,
                 ptutm$easting + eastingadd[1] + rev(cornereasting))
  ptnorthing <- c(ptutm$northing + northingadd[2] + cornernorthing,
                  ptutm$northing + northingadd[1] + rev(cornernorthing))

  cornerlonlat <- utm2lonlat(easting = pteasting,
                             northing = ptnorthing,
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

