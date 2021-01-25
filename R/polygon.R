#' @title Make a transect polygon
#'
#' @description This function takes coordinates for a polygon and will complete it for plotting purposes by
#' extending the limit of the bottom of the polygon by some numeric amount.
#'
#' @param x,y vectors containg the coordinates of the verticies of the polygon
#' @param yadj a numeric value indicating the amount to increase the maximum `y` value.
#'
#' @return a list of `xpoly` and `ypoly` values
#'
#' @author Chantelle Layton
#' @export

transectPoly <- function(x, y, yadj = 50){
  xpoly <- c(x[1], x, max(x), max(x), x[1], x[1])
  ypoly <- c(y[1], y, y[length(y)], max(y) + yadj, max(y) + yadj, y[1])
  return(list(xpoly = xpoly, ypoly = ypoly))
}
