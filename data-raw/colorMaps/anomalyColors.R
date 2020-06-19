library(usethis)

anomalyColourMap <- read.csv('data-raw/colorMaps/colormapDefn.csv')
anomalyCols <- rgb(red = anomalyColourMap$R,
                   green = anomalyColourMap$G,
                   blue = anomalyColourMap$B)
anomalyBreaks <- c(anomalyColourMap$LowerRange, anomalyColourMap$UpperRange[length(anomalyColourMap$UpperRange)])

anomalyColors <- list(colors = anomalyCols,
                       breaks = anomalyBreaks)
usethis::use_data(anomalyColors, compress = "xz", overwrite = T)
