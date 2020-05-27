#' Summer groundfish longitude latitude grid.
#'
#' Summer groundfish longitude latitude grid used for optimal estimation analysis (oax)
#'
#' @format a data frame with un-named columns, but they are as follows,
#' \code{latitude}, \code{longitude}, \code{depth}, ...
#'
"summerGroundfishLonLatGrid"

#' Summer groundfish utm grid.
#'
#' Summer groundfish utm grid used for optimal estimation analysis (oax)
#'
#' @format a data frame with un-named columns, but they are as follows,
#' \code{latitude}, \code{longitude}, \code{depth}, ...
#'
"summerGroundfishUtmGrid"

#' Summer groundfish optimal estimation results
#'
#' Summer groundfish optimal estimation results from 1970 to present
#'
#' @format a data frame with columns \code{year}, \code{latitude}, \code{longitude},
#' \code{depth}, \code{temperature}, \code{temperature_error}, \code{salinity},
#' \code{salinity_error}
"summerGroundfishOaxResults"

#' Summer groundfish optimal estimation climatology
#'
#' Summer groundfish optimal estimation climatology for 1980 to 2010.
#'
#' @format a data frame with columns \code{latitude}, \code{longitude}, \code{depth},
#' \code{temperature}, \code{temperature_error}, \code{salinity}, \code{salinity_error},
#' \code{sigmat}, \code{sigmat_error}.
"summerGroundfishClimatology1980to2010"

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

#' Climate area monthly timeseries
#'
#' Select monthly timeseries for specified climate areas on the scotian shelf
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
#' which has \code{year}, \code{month}, \code{depth}, \code{temperature},
#' \code{temperatureAnomaly}, \code{salinity}, and \code{salinityAnomaly}.
"areaMonthlyTimeseries"

#' Climate area polygons
#'
#' Defined climate area polygons for all four Fisheries and Oceans Canada
#' Atlantic regions, as follows
#' \tabular{ll}{
#' \strong{abbreviation} \tab \strong{region} \cr
#' ScotiaFundy \tab Maritimes \cr
#' Gulf \tab Gulf and Quebec \cr
#' NfldLab \tab Newfoundland and Labrador
#' }
#' @format a data frame with \code{area}, \code{latitude}, \code{longitude}, and \code{region}.
"climatePolygons"

#' Climate area polygon areas
#'
#' Defined climate area polygon areas for only the Maritime region (ScotiaFundy)
#' with units \code{km^2}. Details on how the areas were calculated is not known
#' at this time.
#'
#' @format a data frame with \code{area}, \code{polyArea} and \code{region}.
"climatePolygonArea"


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
