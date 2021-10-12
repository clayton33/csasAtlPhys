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

# the gully coordinates
# obtained from cruise planning documents
# head and mouth are GULD respectively and west and east shelf are SG respectively
lon <- c(-59.107, -59, -58.9543, -58.9)
lat <- c(44.099, 44.01, 43.88941, 43.79)
names <- c('GUL_01', 'GUL_02', 'GUL_03', 'GUL_04')

dfidx <- data.frame(start = c(1,2,3), end = c(2,3,4))
# go through each row and calculate relevant items
# set some things that are used in every iteration
#utm output in m, so do calc in m
dist <- 8 * 1000
angleadj <- 45 + 0:3 * 90
eastingadj <- cos(angleadj * pi/180)
northingadj <- sin(angleadj * pi/180)
ptnorthing <- dist * eastingadj
pteasting <- dist * northingadj
xleft <- xright <- yleft <- yright <- NULL
bottomlon <- bottomlat <- NULL
for(i in 1:dim(dfidx)[1]){
  k <- dfidx[i, ]
  a <- getTransectAngle(longitude = lon[c(k[['start']], k[['end']])],
                        latitude = lat[c(k[['start']], k[['end']])])
  angle <- a$angle
  startlon <- lon[k[['start']]]
  startlat <- lat[k[['start']]]
  endlon <- lon[k[['end']]]
  endlat <- lat[k[['end']]]
  zone <- lonlat2utm(longitude = startlon,
                     latitude = startlat)$zone
  startutm <- lonlat2utm(longitude = startlon,
                         latitude = startlat, zone = zone)
  endutm <- lonlat2utm(longitude = endlon,
                       latitude = endlat,
                       zone = zone)
  pteastingrotate <- pteasting * cos(angle * pi/180) - ptnorthing * sin(angle * pi/180)
  ptnorthingrotate <- pteasting * sin(angle * pi/180) + ptnorthing * cos(angle * pi/180)
  head <- utm2lonlat(easting = startutm$easting + pteastingrotate,
                     northing = startutm$northing + ptnorthingrotate,
                     zone = zone)
  mouth <- utm2lonlat(easting = endutm$easting + pteastingrotate,
                      northing = endutm$northing + ptnorthingrotate,
                      zone = zone)
  # for debug
  if(plot){
    if(i == 1){
      plot(lon, lat, xlim = range(lon) + c(-0.25, 0.25), ylim = range(lat) + c(-0.25, 0.25))
      text(x = lon, y = lat, labels = 1:length(lon))
      #points(head[['longitude']], head[['latitude']], col = i)
      text(labels = 1:4, x = head[['longitude']], y = head[['latitude']], col = i)
      #points(mouth[['longitude']], mouth[['latitude']], col = i + 1)
      text(labels = 1:4, x = mouth[['longitude']], y = mouth[['latitude']], col = i + 1)
    } else {
      #points(head[['longitude']], head[['latitude']], col = i)
      text(labels = 1:4, x = head[['longitude']], y = head[['latitude']], col = i)
      #points(mouth[['longitude']], mouth[['latitude']], col = i + 1)
      text(labels = 1:4, x = mouth[['longitude']], y = mouth[['latitude']], col = i + 1)
    }
  }
  # save values
  if(i == 1){
    xleft <- c(xleft,
               head[['longitude']][3],
               mouth[['longitude']][2])
    xright <- c(xright,
                head[['longitude']][4],
                mouth[['longitude']][4])
    yleft <- c(yleft,
               head[['latitude']][3],
               mouth[['latitude']][2])
    yright <- c(yright,
                head[['latitude']][4],
                mouth[['latitude']][4])
  }
  if(i == 3){
    xleft <- c(xleft,
               head[['longitude']][2],
               mouth[['longitude']][2])
    xright <- c(xright,
                head[['longitude']][4],
                mouth[['longitude']][1])
    yleft <- c(yleft,
               head[['latitude']][2],
               mouth[['latitude']][2])
    yright <- c(yright,
                head[['latitude']][4],
                mouth[['latitude']][1])
  }
  # save values for inferring bottom outline
  df <- data.frame(easting = c(startutm$easting, endutm$easting),
                   northing = c(startutm$northing, endutm$northing))
  lm <- lm(northing ~ easting, data = df)
  nstns <- 16
  dd <- diff(df$easting)/(nstns + 1) # 10 points but have to add 1 for spacing
  eastingnew <- (dd * 1:nstns) + startutm$easting
  northingnew <- predict(lm, newdata = data.frame(easting = eastingnew))
  bottomlonlat <- utm2lonlat(easting = eastingnew, northing = northingnew, zone = zone)
  bottomlon <- c(bottomlon, bottomlonlat$lon)
  bottomlat <- c(bottomlat, bottomlonlat$lat)
}

polyx <- c(xleft, rev(xright))
polyy <- c(yleft, rev(yright))

theGullyNewPolygon <- list(longitude = polyx,
                        latitude = polyy)

usethis::use_data(theGullyNewPolygon, compress = 'xz', overwrite = TRUE)


# next calculate the station polygons using same methods as above.
dist <- NULL
for(i in 1:length(lon)){
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
  if(i %in% (length(lon) + c(-1, 0))){
    k <- dfidx[(length(lon) - 1), ]
  }
  if(i %in% 1:2) {
    k <- dfidx[1, ]
  }
  a <- getTransectAngle(longitude = lon[c(k[['start']], k[['end']])],
                        latitude = lat[c(k[['start']], k[['end']])])
  ang <- a$angle
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
  stnpoly[[i]][['transectName']] <- 'theGullyNew'
}

theGullyNewStationPolygons <- stnpoly
usethis::use_data(theGullyNewStationPolygons, compress = 'xz', overwrite = TRUE)

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

# create a transect definition for the gully new
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
gullyTransectDef <- list(info = data.frame(start_latitude = startLatitude,
                                           start_longitude = startLongitude,
                                           yaxis_max = 300), # need to think about yaxis_max
                         bottom_outline = bottomOutline)

save(gullyTransectDef, file = './data-raw/transects/gullyNewTransectDefinition.RData')

