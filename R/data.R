#' Scotian shelf longitude latitude grid.
#'
#' Scotian shelf longitude latitude grid used for optimal estimation analysis (oax)
#'
#' @format a data frame with un-named columns, but they are as follows,
#' \code{latitude}, \code{longitude}, \code{depth}, ...
#'
"summerGroundfishLonLatGrid"

#' Scotian shelf utm grid.
#'
#' Scotian shelf utm grid used for optimal estimation analysis (oax)
#'
#' @format a data frame with un-named columns, but they are as follows,
#' \code{latitude}, \code{longitude}, \code{depth}, ...
#'
"summerGroundfishUtmGrid"

#' Climate area climatologies
#'
#' Select climatologies for specified climate areas on the scotian shelf
#' associated with annual reporting. Those that are analyzed yearly are
#' \tabular{ll}{
#' \strong{area number} \tab \strong{area name} \cr
#' 05 \tab Misaine Bank \cr
#' 12 \tab Emerald Basin \cr
#' 24 \tab Lurcher Shoal \cr
#' 26 \tab Georges Basin\cr
#' 28 \tab Eastern Georges Bank \cr
#' 66 \tab Cabot Strait
#' }
#' @format a list of data frames that are named by their area, for
#' example, 05 is an element in a list named \code{area05}, and so on for
#' the areas defined above. Each element in the list has a data frame
#' which has \code{depth}, \code{month}, \code{temperature_ltm},
#' \code{salinity_ltm}, and \code{sigmat_ltm}.
"areaClimatologies1980to2010"

#' Station 2 monthly data
#'
#' Station 2 monthly and vertically averaged data for the entire timeseries.
#'
#' @format a data frame with columns \code{year}, \code{month}, \code{depth},
#' \code{temperature}, \code{salinity}, \code{sigmaTheta}
"station2monthlyData"

#' Station 2 climatology
#'
#' Station 2 1980 to 2010 climatology
#'
#' @format a data frame with columns \code{month}, \code{depth},
#' \code{temperature_ltm}, \code{salinity_ltm}, \code{sigmaTheta_ltm}
"station2Climatology1980to2010"

#' Prince 5 monthly data
#'
#' Prince 5 monthly and vertically averaged data for the entire timeseries.
#'
#' @format a data frame with columns \code{year}, \code{month}, \code{depth},
#' \code{temperature}, \code{salinity}, \code{sigmaTheta}
"prince5monthlyData"


#' Prince 5 climatology
#'
#' Prince 5 1980 to 2010 climatology
#'
#' @format a data frame with columns \code{Month}, \code{Depth}, \code{Avg_Temperature},
#' \code{Std_Temperature}, \code{Years_Temperature}, \code{Avg_Salinity}, \code{Std_Salinity},
#' \code{Years_Salinity}, \code{Avg_Sigmat}, \code{Std_Sigmat}, \code{Years_Sigmat}.
"prince5Climatology1980to2010"

#' Prince 5 standard depth bins
#'
#' Prince 5 standard depth bins and tolerance definitions for vertical averaging
#'
#' @format a data frame with columns \code{bin} and \code{tolerance}
"prince5depthBins"

#' Transect standard depth bins
#'
#' Transect standard depth bins and tolerances definitions for verticaly averaging
#' @format a data frame with columns \code{bin} and \code{tolerance}
"transectDepthBins"
