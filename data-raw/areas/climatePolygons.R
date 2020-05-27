library(usethis)

load('data-raw/areas/polygons/Climate_Area_Polygons.RData')
climatePolygons <- data.frame(climate_poly, stringsAsFactors = FALSE)
names(climatePolygons) <- tolower(names(climatePolygons))
usethis::use_data(climatePolygons, compress = "xz", overwrite = TRUE)

areaFile <- 'data-raw/areas/polygons/Polygon_Areas.csv'
polyareas <- read.csv(areaFile, header = FALSE)
names(polyareas) <- c('area', 'polyArea')
climatePolygonArea <- data.frame(polyareas, region = rep('ScotiaFundy', dim(polyareas)[1]), stringsAsFactors = FALSE)
usethis::use_data(climatePolygonArea, compress = 'xz', overwrite = TRUE)
