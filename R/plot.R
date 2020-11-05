#' @title Plot annual time series
#'
#' @author Chantelle Layton
#'
#' @description Creates a time-series of annual time series in the format that is used
#' for
#'
#' @param x a vector indicating the times of observations
#' @param y a vector indicating the observations
#' @param xlim Optional limit for x axis
#' @param ylim Optional limit for y axis
#' @param xlab Logical indicating whether to label the x axis
#' @param ylabel Logical indicating whether to label the y axis
#' @param yaxs Logical indicating whether to have a labelled y-axis
#' @param plotSd Logical indicating whether to add +/- 0.5 standard deviation lines on plot of
#' the annual anomaly over the defined climatology period.
#' @param climatologyYears Vector of length two indicating the climatology years, ignored if
#' `plotSd = FALSE`.
#' @param plotPoints Logical indicating whether or not to plot points on top of line
#' @param plotRunningAvg Logical indicating whether or not to plot a 5 year running average
#' @param plotLmTrend Logical indicating whether or not to print out results of a linear regression
#' on the plot
#' @param lmResults A `lm` format object, if `plotLmTrend = FALSE`, ignored.
#' @param plotClimatologyMean Logical indicating whether or not to print out the climatology mean.
#' @param climatologyMean A numeric value, if `plotClimatologyMean = FALSE`, ignored.
#'
#' @details The current format of the figure is for the 2019 research document. Any changes in the future
#' will be reflected in the code with comments, and here.
#'
#' \itemize{
#' \item{2019 - Annual anomalies are plotted with a grey line and a 5-year running mean plotted over top in black.
#' Thick dashed horizonal lines of the standard deviation of the annual climatology are added. Code has
#' been written to ensure that at least one century is labelled.}
#' }
#'
#' @importFrom stats lm
#' @importFrom stats sd
#' @importFrom stats coef
#' @importFrom graphics plot
#' @importFrom graphics abline
#' @importFrom graphics axis
#' @importFrom graphics lines
#' @importFrom graphics mtext
#' @importFrom graphics points
#' @importFrom graphics legend
#'
#' @export
#'


plotAnnualAnomaly <- function(x, y, xlim, ylim, xlab = TRUE, climatologyYears, ylabel = TRUE,
                                    plotSd = TRUE, yaxs = TRUE, plotPoints = TRUE, plotRunningAvg = TRUE,
                              plotLmTrend = FALSE, lmResults,
                              plotClimatologyMean = FALSE, climatologyMean){
  is.even <- function(x) x %% 2 == 0
  # ylabel
  L <- '['
  R <- ']'
  ylab <- getAnomalyLabel(item = 'temperatureAnomaly', sep = "")
  # xlim
  # set it if not given
  xlimGiven <- !missing(xlim)
  if(!xlimGiven){
    xlim <- range(x, na.rm = TRUE)
  }
  # ylim
  # set it if not given
  ylimGiven <- !missing(ylim)
  if(!ylimGiven){
    ylim <- range(y, na.rm = TRUE)
  }

  plot(x = x,
       y = y, col = 'white',
       lwd = 0.6,
       type = 'n',
       xlim = xlim, ylim = ylim,
       xaxt = 'n', yaxt = 'n',
       xlab = '', ylab = '')
  #...)
  # x-axis, x-axis labels
  xat <- seq(round(xlim[1], digits = -1), round(xlim[2], digits = -1), 10) # tick every decade
  centuries <- c(1800, 1900, 2000)
  okcentury <- centuries %in% xat
  centuryIdx <- unlist(lapply(centuries[okcentury], function(k) which(k == xat)))
  # if centuryIdx[1] is even, start xlabel idx at 2, if odd, at 1
  xlabels <- seq(ifelse(is.even(centuryIdx[1]), 2, 1), length(xat), 2) # label every second decade, make sure centuries are labelled
  axis(side = 1, at = xat, labels = FALSE)
  if(xlab){
    #axis(side = 1, at = xat[xlabels], labels = xat[xlabels])
    axis(side = 1, at = xat, labels = xat) # try this for now
  }
  # y-axis, y-axis labels, y-axis label
  if(yaxs){
    {if(diff(ylim) < 20){ # kind of bad logic
      yat <- seq(round(ylim[1], digits = 0), round(ylim[2], digits = 0), 1) # label every one
    } else {
      yat <- pretty(ylim)
    }
    }
    axis(side = 2, at = yat, labels = yat)
  }

  if(ylabel) mtext(text = ylab, side = 2, line = 2, cex = 4/5)
  # grid
  # ugh, function grid() not working when nx and ny given
  abline(v = xat, col = 'lightgray', lty = 'dotted')
  if(yaxs) abline(h = yat, col = 'lightgray', lty = 'dotted')
  # replot initial lines so its over the grid
  if(plotPoints){
    lines(x = x, y = y, col = 'black')
    points(x = x, y = y, pch = 21, col = 'black', bg = 'white')
  } else {
    lines(x = x, y = y, col = 'black', lty = 2)
  }
  # add additional stuff to plot
  # climatology standard deviation
  if(plotSd){
    okclim <- x >= climatologyYears[1] & x <= climatologyYears[2]
    aasd <- sd(y[okclim], na.rm = TRUE)
    #{if(diff(ylim) > 20) { # commented out this if else statement 20200402
    cmean <- mean(y[okclim], na.rm = TRUE)
    abline(h = cmean, lty = 1, lwd = 2)
    abline(h = (aasd/2) + cmean, lty = 2, lwd = 2)
    abline(h = (-aasd/2) + cmean, lty = 2, lwd = 2)
    #} else {
    #  abline(h = 0)
    #  abline(h = aasd/2, lty = 2, lwd = 2)
    #  abline(h = -aasd/2, lty = 2, lwd = 2)
    #}
    #}
  }

  # replot initial lines so its over the grid and sd lines
  if(plotPoints){
    lines(x = x, y = y, col = 'black')
    points(x = x, y = y, pch = 21, col = 'black', bg = 'white')
  } else {
    lines(x = x, y = y, col = 'black', lty = 2)
  }

  # 5 year running mean filter
  if(plotRunningAvg){
    aafilt <- stats::filter(y, rep(0.2, 5), method = 'convolution', sides = 2)
    lines(x = x, y = aafilt, lwd = 2)
  }
  if(plotLmTrend){
    slope <- sprintf('%.1f', round(unname(coef(lmResults)[2]) * 100, 1))
    slope <- ifelse(Sys.getenv('LANG') == 'fr', gsub('\\.', ',', slope), slope) # comma instead of period for french
    unit <- bquote(degree * 'C')
    yearlab <- gettext('years', domain = 'R-csasAtlPhys')
    legendText <- bquote(.(slope) * .(unit) * ' / 100 ' * .(yearlab))
    legend('bottomright', legend = legendText, bty = 'n', cex = 1.2)
  }
  if(plotClimatologyMean){
    unit <- bquote(degree * 'C')
    meanlab <- gettext('Mean', domain = 'R-csasAtlPhys')
    climMean <- sprintf('%.2f', climatologyMean)
    legend('bottomleft', legend = bquote(.(meanlab)*' = ' * .(climMean) * .(unit)), bty = 'n')
  }
}


#' @title Plot monthly bar plots
#'
#' @description This plotting method for air temperature data plots bar plots
#' for monthly anomalies and is colour coded based on normalized monthly
#' anomalies
#'
#' @param df a data.frame containing at least year, month, anomaly, and normalizedAnomaly.
#' @param ylim a vector of length 2 indicating the range for the y-axis
#' @param years vector of length two indicating the range of years for climatology
#' @param plotYear a vector indicating which years to plot. Can be a vector of length one, indicating
#' to plot only one year, or of length two, indicating a range of years to plot
#' @param yearLabel logical expression indicating whether or not to label the plotYears
#' @param yearLabelSide which side the yearLabel should be placed, (1 = bottom, 3 = top)
#' @param drawPalette logical expression indicating whether or not to draw a palette
#' @param xaxt logical expression indicating whether or not to label x-axis
#' @param ylabel logical exptression indicating whether or not to label y-axis labels
#' @param mar vector of length 4 indicating the margins of the plot
#'
#' @author Chantelle Layton
#'
#' @importFrom graphics barplot
#' @importFrom graphics box
#' @importFrom graphics par
#' @importFrom grDevices rgb
#' @importFrom oce colormap
#' @importFrom oce drawPalette
#'
#' @export

plotMonthlyBar <- function(df, ylim, years, plotYear, yearLabel = FALSE, yearLabelSide, drawPalette = TRUE, xaxt = TRUE, ylabel = TRUE, mar){
  is.even <- function(x) x %% 2 == 0
  # ylabel
  L <- '['
  R <- ']'
  ylab <- getAnomalyLabel('temperatureAnomaly')

  # The blue-red colormap with ranges -3.5 to +3.5 standard deviations
  rgbcol <- c(c(157,0,0),c(230,0,0),c(255,51,51),c(255,102,102),c(255,151,151),c(255,202,202),c(255,255,255),
              c(255,255,255),c(202,202,255),c(151,151,255),c(102,102,255),c(51,51,255),c(0,0,230),c(0,0,157))
  rgbcol <- matrix(rgbcol, nrow=length(rgbcol)/3, ncol=3, byrow=TRUE)
  rgbcol <- rgbcol[seq(nrow(rgbcol),1,-1),]

  # Make the RGB colors, but don't make a colorRamp
  anomalyCols <- rgb(red=rgbcol[,1], green=rgbcol[,2], blue=rgbcol[,3], maxColorValue = 255)
  anomalyBreaks <- seq(-3.5, 3.5, 0.5)



  # ylim
  # set it if not given
  ylimGiven <- !missing(ylim)
  if(!ylimGiven){
    ylim <- range(df[['anomaly']], na.rm = TRUE)
  }

  # subset the data based on plotYear
  dfs <- df[df[['year']] %in% plotYear, ]
  # add any missing months
  expectedYearMonth <- do.call('rbind',lapply(plotYear, function(k) data.frame(year = rep(k, 12), month = as.numeric(1:12))))
  expectedData <- dfs[nrow(dfs) + 1:nrow(expectedYearMonth), !names(dfs) %in% c('year', 'month')]
  expectedDf <- cbind(expectedYearMonth, expectedData)
  okaddexpected <- !mapply(function(year,month) month %in% dfs[['month']][dfs[['year']] == year],
                           expectedDf[['year']],
                           expectedDf[['month']])
  dfu <- rbind(dfs, expectedDf[okaddexpected, ])
  # create colour map based on normalized values
  cm <- colormap(z = dfu[['normalizedAnomaly']], breaks = anomalyBreaks, col = anomalyCols)
  # now plot
  if(drawPalette){
    drawPalette(colormap = cm, zlab = ' ', cex = 1)
  }
  marGiven <- !missing(mar)
  if(!marGiven){
    mar <- c(3.5, 3.5, 1, 1)
  }
  par(mar=mar)
  barplot(dfu[['anomaly']], col = cm$zcol,
          ylim = ylim, xlim = c(0, length(dfu[['anomaly']])),
          axisnames = FALSE, axes = FALSE,
          space = 0, offset = 0, width = 1, # to tighten up bars
          xaxs = 'i')

  # axes
  # x-axis
  # label every third month
  monthTick <- 1:nrow(dfu)
  monthAt <- seq(1, nrow(dfu), 2)
  monthLabels <- substring(month.abb[dfu[['month']]], 1, 1)[monthAt]
  {if(xaxt){ # if xaxt is not given
    axis(1, at = monthTick - 0.5, labels = FALSE) # subtract 0.5 from monthAt to be center of bar
    mtext(text = monthLabels, side = 1, line = 0.5, at = monthAt - 0.5, cex = 3/5)

  } else {
    axis(1, at = monthTick - 0.5, labels = FALSE)
  }}
  if(yearLabel){
    if(length(plotYear) == 1){
      mtext(text = plotYear, side = yearLabelSide, line = 1)
    } else {
      yrs <- plotYear[1]:plotYear[2]
      mtext(text = '|', side = yearLabelSide, at = 12 * (seq(1, length(yrs)-1,1)), cex = 3/5, line = 1.5)
      mtext(text = yrs, side = yearLabelSide, at = 6 + 12 * seq(0, length(yrs) - 1, 1), cex = 3/5, line = 1.5)
      #mtext(text = plotYear[2], side = yearLabelSide, at = 18, cex = 4/5)
    }
  }
  abline(v = monthAt- 0.5, col = 'lightgrey', lty = 2)
  if(length(plotYear) == 2) {
    yrs <- plotYear[1]:plotYear[2]
    abline(v = 12 * (seq(1, length(yrs)-1,1)))
  }
  #axis(3, at = monthAt - 0.5, labels = FALSE)
  # y - axis
  # regardless of ylim, label every second value
  yat <- seq(ylim[1], ylim[2], 2)
  ylabels <- yat
  axis(2, at = yat, labels = ylabels)
  abline(h = yat, col = 'lightgrey', lty = 2)
  if(ylabel){mtext(text = ylab, side = 2, line = 2, cex = 4/5)}

  # make it pretty
  abline(h = 0)
  box()
  barplot(dfu[['anomaly']], col = cm$zcol,
          ylim = ylim, xlim = c(0, length(dfu[['anomaly']])),
          axisnames = FALSE, axes = FALSE,
          space = 0, offset = 0, width = 1, # to tighten up bars
          add = TRUE)
  if(drawPalette) mtext(side = 4, text = getAnomalyLabel('normalizedAnomaly'), line = 4, cex = 4/5)
}

#' @title Plot annual stacked bar plot
#'
#' @description This function will plot a stacked bar plot that has negative and
#' positive values. It also has the option to plot the average of values provided.
#'
#' @param x a vector indicating what the columns represent for \code{z}
#' @param y a vector indicating what the rows represent for \code{z}
#' @param z a matrix with the columns being the x-axis and the rows being items
#' that wish to be stacked.
#' @param plotAverage a logical value indicating whether or not to add the average value
#' from matrix z.
#' @param ylab1 name for the y-axis on side 2, default is \code{NULL}
#' @param ylab2 name for the y-axis on side 4 if \code{plotAverage = TRUE}, ignored otherwise, default is
#' \code{NULL}.
#' @param ylim1 limits for the y-axis on side 2, if not supplied, it will be inferred from \code{z}
#' @param ylim2 limits for the y-axis on side 4 if \code{plotAverage = TRUE}, ignored otherwise. If not
#' supplied, it will be inferred from the data.
#' @param ncol supplied to legend, the number of columns in which to set the legend items, default is 1.
#'
#' @author Chantelle Layton
#'
#' @importFrom viridis viridis
#' @importFrom graphics legend
#'
#' @export
#'

plotStackedBarplot <- function(x, y, z, plotAverage = TRUE, ylab1 = NULL, ylab2 = NULL,
                               ylim1, ylim2, ncol = 1){
  mround <- function(x, base) {base * round(x/base)}
  is.even <- function(x) x %% 2 == 0
  # check to see if ylim1 is given
  if(missing(ylim1)){
    ylim1 <- range(z, na.rm = TRUE)
  }
  # have to split up the matrix into negative and positive
  zPos <- z
  zPos[zPos < 0] <- 0
  zPos[is.na(zPos)] <- 0
  zNeg <- z
  zNeg[zNeg > 0] <- 0
  zNeg[is.na(zNeg)] <- 0

  cexaxis <- 0.8
  # first do the positive values
  bppos <- barplot(zPos, ylim = ylim1, col = viridis(n = 6), xaxt = 'n',
                   #legend = y, args.legend = list(x = 'topleft', bty = 'n'), border = NA,
                   cex.axis = cexaxis)
  # need to set up the nice xaxis labels using some information from the bar plot
  # this will be used again below when actually labelling the x-axis, but some information
  # is needed to do dotted guidelines
  bp <- c(bppos, bppos[length(bppos)] + mean(diff(bppos)))
  # need to do some fudging when close to decade ending
  stackedx <- c(x, x[length(x)] + 1)
  xlim <- mround(range(stackedx),5)
  if(diff(xlim) > 50){
    xat <- seq(round(xlim[1], digits = -1), round(xlim[2], digits = -1), 10) # tick every decade
    centuries <- c(1800, 1900, 2000)
    okcentury <- centuries %in% xat
    centuryIdx <- unlist(lapply(centuries[okcentury], function(k) which(k == xat)))
    # if centuryIdx[1] is even, start xlabel idx at 2, if odd, at 1
    xlabels <- seq(ifelse(is.even(centuryIdx[1]), 2, 1), length(xat), 2) # label every second decade, make sure centuries are labelled
    okIdxLab <- unlist(lapply(xat[xlabels], function(k) which(k == stackedx)))
    okIdxAt <- unlist(lapply(xat, function(k) which(k == stackedx)))
  } else {
    xat <- pretty(xlim)
    okIdxAt <- okIdxLab <- unlist(lapply(xat, function(k) which(k == stackedx)))
  }

  # horizontal and vertical guidelines
  hline <- pretty(ylim1)
  # positive will include zero
  abline(h = hline[hline >= 0], lty = 'dotted', col = 'lightgrey')
  abline(v = bp[okIdxAt], lty = 'dotted', col = 'lightgrey')


  # negative values
  bpneg <- barplot(zNeg, ylim = ylim1, col = viridis(n = 6), xaxt = 'n',
                   border = NA, cex.axis = cexaxis, add = TRUE)
  axis(side = 1, at = bp[okIdxAt], labels = FALSE)
  axis(side = 1, at = bp[okIdxLab], labels = stackedx[okIdxLab])

  abline(h = hline[hline < 0], lty = 'dotted', col = 'lightgrey')
  abline(v = bp[okIdxAt], lty = 'dotted', col = 'lightgrey')
  # add neg bar plot again to get things on top of guidelines
  bpneg <- barplot(zNeg, ylim = ylim1, col = viridis(n = 6), xaxt = 'n',
                   border = NA, add = TRUE, cex.axis = cexaxis)

  # add the bar plot back on top to get things on top of the guidelines
  barplot(zPos, ylim = ylim1, col = viridis(n = 6), xaxt = 'n',
          legend = y, args.legend = list(x = 'topleft', bty = 'n', ncol = ncol), border = NA, add = TRUE,
          cex.axis = cexaxis)
  box()

  if(plotAverage){
    par(new = TRUE)
    mtanom <- apply(z, 2, mean, na.rm = TRUE)
    # check if ylim2 given
    if(missing(ylim2)){
      ylim2 <- range(mtanom, na.rm = TRUE)
    }
    usr <- par('usr')
    xr <- (usr[2] - usr[1]) / 27
    xlim <- c(usr[1] + xr, usr[2] - xr)
    plot(bp[1:(length(bp)-1)], as.vector(mtanom), type = 'l',
         ylim = ylim2 , xlim = xlim, lwd = 1,
         axes = FALSE, bty = 'n', xlab = '', ylab = '')
    axis(side = 4, cex.axis = cexaxis)
    legend('topright', lty = 1, col = 'black',
           bty = 'n',
           legend = gettext('average', domain = 'R-csasAtlPhys'))
    box()
  }
  if(!is.null(ylab1)) {mtext(side = 2, text = ylab1, line = 2)}
  if(plotAverage & !is.null(ylab2)) mtext(side = 4, text = ylab2, line = 2)
}

#' @title Plot station locations
#'
#' @description Plots station locations at the top of plots using the points function
#'
#' @param distance a numerical vector indicating the distance
#' @param plabel a numerical vector indication the placement of the labels
#' @param cex a numerical value indicating the magnification
#' @param pch a numerical value indicating the symbol
#' @param col a character string indicating the color
#'
#' @author Chantelle Layton
#'
#' @importFrom graphics par
#' @importFrom graphics points
#' @export
plotStationLocations <- function(distance, plabel, cex = 9/10, pch = 25, col = 'black'){
  par(xpd = NA)
  points(distance, rep(plabel, length(distance)), pch = pch, bg = col, col = col, cex = cex)
  par(xpd = FALSE)
}
