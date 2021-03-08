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

#' Summer groundfish cold intermediate layer (cil) grid.
#'
#' Summer groundfish cold intermediate layer (cil) grid, it is a subset of the summerGroundfishLonLatGrid
#'
#' @format a data frame \code{latitude} and \code{longitude}.
#'
"summerGroundfishCilGrid"

#' Summer groundfish optimal estimation ctd input
#'
#' Summer groundfish optimal estimation ctd input from 1970 to present. The data was put
#' input ctd objects from the input files for use of plotting multiple years of oax results
#'
#' @format a list of ctd objects
"summerGroundfishOaxCtdInput"


#' Summer groundfish optimal estimation results
#'
#' Summer groundfish optimal estimation results from 1970 to present
#'
#' @format a data frame with columns \code{year}, \code{latitude}, \code{longitude},
#' \code{depth}, \code{temperature}, \code{temperature_error}, \code{salinity},
#' \code{salinity_error}
"summerGroundfishOaxResults"

#' Summer groundfish optimal estimation results to use for cold intermediate layer calculations
#'
#' Summer groundfish optimal estimation results from 1972 to present. This is different than
#' summerGroundfishOaxResults as the analysis was done differently. Due to low vertical resolution
#' in the earlier years, data was combined to get better results for e.g 1972 includes data from 1970
#' to 1974. Data from 1990 to 2017 is all high resolution data, so it is only the data from that year.
#' This data is already subsetted to the CIL grid, except for 2019, which is the full OAX grid.
#'
#' @format a data frame with columns \code{year}, \code{latitude}, \code{longitude},
#' \code{depth}, \code{temperature}, \code{salinity},
"summerGroundfishOaxResultsForCIL"


#' Summer groundfish optimal estimation climatology
#'
#' Summer groundfish optimal estimation climatology for 1980 to 2010.
#'
#' @format a data frame with columns \code{latitude}, \code{longitude}, \code{depth},
#' \code{temperature}, \code{temperature_error}, \code{salinity}, \code{salinity_error},
#' \code{sigmat}, \code{sigmat_error}.
"summerGroundfishClimatology1980to2010"

#' Summer groundfish NAFO region boundaries for oax grid
#'
#' Summer groundfish NAFO region boundaries based on the grid that is used for the
#' OAX analysis.
#'
#' @format a list that contains each NAFO region within the oax grid, 4vn, 4vs,
#' 4w, 4x, and 4v. Each list element contains \code{longitude} and \code{latitude}.
"summerGroundfishOaxNafoPolygons"

#' Summer groundfish map plot limits
#'
#' Various limits for plotting summer groundfish maps.
#'
#' @format a list of \code{limits}, \code{contourLevelLimits}, and \code{contourLevelLimits},
#' each having defined items for \code{temperature}, \code{temperatureAnomaly}, \code{salinity},
#' and \code{salinityAnomaly}.
"summerGroundfishPlotLimits"

#' Winter groundfish longitude latitude grid.
#'
#' Winter groundfish longitude latitude grid used for optimal estimation analysis (oax)
#'
#' @format a data frame with un-named columns, but they are as follows,
#' \code{latitude}, \code{longitude}, \code{depth}, ...
#'
"winterGroundfishLonLatGrid"

#' Winter groundfish utm grid.
#'
#' Winter groundfish utm grid used for optimal estimation analysis (oax)
#'
#' @format a data frame with un-named columns, but they are as follows,
#' \code{latitude}, \code{longitude}, \code{depth}, ...
#'
"winterGroundfishUtmGrid"

#' Winter groundfish optimal estimation results
#'
#' Winter groundfish optimal estimation results from 1970 to present
#'
#' @format a data frame with columns \code{year}, \code{latitude}, \code{longitude},
#' \code{day_number}, \code{depth}, \code{temperature}, \code{temperature_error}, \code{salinity},
#' \code{salinity_error}
"winterGroundfishOaxResults"

#' Winter groundfish optimal estimation ctd input
#'
#' Winter groundfish optimal estimation ctd input from 2008 to present. The data was put
#' input ctd objects from the input files for use of plotting multiple years of oax results
#'
#' @format a list of ctd objects
"winterGroundfishOaxCtdInput"

#' Winter groundfish optimal estimation climatology
#'
#' Winter groundfish optimal estimation climatology for 1980 to 2010.
#'
#' @format a data frame with columns \code{latitude}, \code{longitude}, \code{day_number}, \code{depth},
#' \code{temperature}, \code{temperature_error}, \code{salinity}, \code{salinity_error},
#' \code{sigmat}, \code{sigmat_error}.
"winterGroundfishClimatology1980to2010"

#' Winter groundfish map plot limits
#'
#' Various limits for plotting summer groundfish maps.
#'
#' @format a list of \code{limits}, \code{contourLevelLimits}, and \code{contourLevelLimits},
#' each having defined items for \code{temperature}, \code{temperatureAnomaly}, \code{salinity},
#' and \code{salinityAnomaly}.
"winterGroundfishPlotLimits"

#' Snow crab longitude latitude grid.
#'
#' Snow crab longitude latitude grid used for optimal estimation analysis (oax)
#'
#' @format a data frame with un-named columns, but they are as follows,
#' \code{latitude}, \code{longitude}, \code{depth}, ...
#'
"snowCrabLonLatGrid"

#' Snow crab utm grid.
#'
#' Snow crab utm grid used for optimal estimation analysis (oax)
#'
#' @format a data frame with un-named columns, but they are as follows,
#' \code{latitude}, \code{longitude}, \code{depth}, ...
#'
"snowCrabUtmGrid"

#' Snow crab optimal estimation results
#'
#' Snow crab optimal estimation results from 1970 to present
#'
#' @format a data frame with columns \code{year}, \code{latitude}, \code{longitude},
#' \code{depth}, \code{temperature}, \code{temperature_error}, \code{salinity},
#' \code{salinity_error}
"snowCrabOaxResults"


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

#' Density gradient monthly climatology 1981 to 2010
#'
#' Relevant parameters used to calculate the density gradient across the scotian shelf
#'
#' @format a data.frame that has the climatology monthly average for areas 1 to 29
#' of the Scotian-Fundy polygons. Variables include the value at 0m and 50m for temperature,
#' salinity, and sigmaTheta, and the number of profiles used in the calculation. Also, the
#' salinity and sigmaTheta gradient, as well as the number of profiles used for the gradient calculations.
"densityGradientMonthlyClimatology1981to2010"

#' Density gradient weighted anomaly
#'
#' Previously calculated anomaly values for variables associated with density gradient analysis.
#'
#' @format a data.frame that has year and the weighted anomaly, weighted anomaly standard deviation, and number
#' of regions for variables at 0m and 50m for salinity, temperature, and sigmaTheta, along with the salinity
#' and sigma theta gradient
"densityGradientWeightedAnomaly"

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

#' Area standard depth bins
#'
#' Area standard depth bins and tolerances definitions for vertically averaging
#' @format a data frame with columns \code{bin} and \code{tolerance}
"areaDepthBins"


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

#' Station 2 polygon
#'
#' Polygon defining the bounds to look for data along the fixed Atlantic Zone Monitoring Program
#' station, Station 2. It was carried over from previous employee.
#'
#' @format a list containing \code{longitude} and \code{latitude}
"station2Polygon"

#' Station 2 standard depth bins
#'
#' Station 2 depth bins and tolerances definitions for vertically averaging
#' @format a data frame with columns \code{bin} and \code{tolerance}
"station2DepthBins"

#' Station plot limits
#'
#' Various limits for plotting station 2 section plots.
#'
#' @format a list of \code{limits}, \code{contourLevelLimits}, and \code{contourLevelLimits},
#' each having defined items for \code{temperature}, \code{temperatureAnomaly}, \code{salinity},
#' \code{salinityAnomaly}, \code{sigmaTheta}, \code{sigmaThetaAnomaly}.
"station2PlotLimits"

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
#' Transect standard depth bins and tolerances definitions for vertically averaging
#' @format a data frame with columns \code{bin} and \code{tolerance}
"transectDepthBins"

#' Transect distance bins
#'
#' Transect distance bin for the 1981 to 2010 climatology definitions for horizontal averaging
#' @format a list with the transect name, each with a data frame with columns \code{bin} and
#' \code{tolerance}. Only one transect has separate definitions based on the season.
"transectDistanceBins"

#' Transect plot limits
#'
#' Various limits for plotting transects.
#'
#' @format a list of \code{limits}, \code{contourLevelLimits}, and \code{contourLevelLimits},
#' each having defined items for \code{temperature}, \code{temperatureAnomaly}, \code{salinity},
#' \code{salinityAnomaly}, \code{sigmaTheta}, \code{sigmaThetaAnomaly}.
"transectPlotLimits"

#' Transect 1980 to 2010 climatology
#'
#' Core azmp transect 1980 to 2010 climatology.
#'
#' @format a list which contains a list for each transect that has the \code{transecAbbreviation},
#' \code{transectFullname}, \code{season}, \code{program}, and \code{climatology}, the climatology has
#' column names of \code{distanceBin}, \code{depthBin}, \code{temperature}, \code{temperatureSd},
#' \code{temperatureNobs}, \code{salinity}, \code{salinitySd}, \code{salinityNobs}, \code{sigmaTheta},
#' \code{sigmaThetaSd}, \code{sigmaThetaNobs}.
"transectClimatology1980to2010"

#' Transect definitions
#'
#' Transect definitions has some important information pertaining to plotting
#' many of the transects associated with the AZMP program. Some is leftover from what used to be used
#' when plotting in MATLAB, but there are some items of importance, so everything was saved.
#'
#' @format a list which contains a list for each transect that has \code{info}, that has a handful
#' of other variables which is too lengthy to list here, but two that are of importance are
#' \code{start_longitude} and \code{start_latitude}, \code{bottom_outline}, the bathymetry for plotting,
#' and then other information that is not currently used, \code{bottom_mask}, \code{xgrid}, and \code{ygrid}.
"transectDefinitions"

#' Sea surface temperature in-situ for Halifax site
#'
#' Monthly average sea surface temperature for in-situ Halifax site
#' that is located at the wharf at the Maritime museum in the Halifax harbour. The monthly averages
#' are only up to a certain year, the purpose of this file being included is that the
#' historical data is not available anywhere.
#'
#' @format a data frame with columns \code{year}, \code{month}, \code{temperature}
"sstInSituHalifax"

#' Sea surface temperature in-situ for St.Andrews site
#'
#' Monthly average sea surface temperature for in-situ St.Andrews site
#' that is located off the wharf at the St.Andrews Biological Station. The monthly averages
#' are only up to a certain year, the purpose of this file being included is that the
#' historical data is not available anywhere.
#'
#' @format a data frame with columns \code{year}, \code{month}, \code{temperature}
"sstInSituStAndrews"

#' Sea surface temperature in-situ polygons for two sites
#'
#' Longitude and latitude polygon definitions that were manually created to help facilitate
#' finding data.
#'
#' @format a list of length two, both of which have the station name and the longitude and latitude
"sstInSituPolygons"

#' NAFO zones for Maritimes regions
#'
#' NAFO zone definitions that have been subset to those only concerned with in the Maritimes
#' region. The shapefile was downloaded from the NAFO website.
#'
#' @format a list for each NAFO zone, where each zone has \code{zoneName}, \code{longitude}, and
#' \code{latitude}
"nafoZones"

#' Anomaly colours
#'
#' Color definitions for anomaly plots. Note that the breaks are one unit longer than the color
#' map. This is fine when supplying it to oce's colorMap function
#'
#' @format a list of \code{colors} and \code{breaks}
"anomalyColors"

#' Browns bank polygon
#'
#' Polygon defining the bounds to look for data along the core Atlantic Zone Monitoring Program
#' line, browns bank. It was constructed by finding the angle between the first point, and the sixth
#' station, and then the sixth station to the seventh station and the line has a leg at the end. The bounding
#' box was calculated to be 8 km wide.
#'
#' @format a list of \code{longitude} and \code{latitude}
"brownsBankPolygon"

#' Browns bank station polygon's
#'
#' Polygons defining bounds to look for data at each station along the core Atlantic Zone Monitoring Program
#' line, browns bank. Defining bounds are determined by station spacing. If the spacing is less than 8km,
#' then it is modified to half of the distance between the adjacent spacing. Note that this mean that the spacing
#' on either side can be uneven.
#'
#' @format a list which contains \code{stationName}, \code{longitude}, \code{latitude}, \code{polyLongitude},
#' \code{polyLatitude} for each station.
"brownsBankStationPolygons"

#' Halifax polygon
#'
#' Polygon defining the bounds to look for data along the core Atlantic Zone Monitoring Program
#' line, halifax. It was constructed by finding the angle between the first point and the last point
#' of the line, and then calculating the bounding box which is 8 km wide.
#'
#' @format a list of \code{longitude} and \code{latitude}
"halifaxPolygon"

#' Halifax station polygon's
#'
#' Polygons defining bounds to look for data at each station along the core Atlantic Zone Monitoring Program
#' line, Halifax. Defining bounds are determined by station spacing. If the spacing is less than 8km,
#' then it is modified to half of the distance between the adjacent spacing. Note that this mean that the spacing
#' on either side can be uneven.
#'
#' @format a list which contains \code{stationName}, \code{longitude}, \code{latitude}, \code{polyLongitude},
#' \code{polyLatitude} for each station.
"halifaxStationPolygons"

#' North east channel polygon
#'
#' Polygon defining the bounds to look for data along the core Atlantic Zone Monitoring Program
#' line, north east channel. It was constructed by finding the angle between the first point and the last point
#' of the line, and then calculating the bounding box which is 8 km wide.
#'
#' @format a list of \code{longitude} and \code{latitude}
"northeastChannelPolygon"

#' North east channel station polygon's
#'
#' Polygons, a 4km box, defining bounds to look for data at each station along the core Atlantic Zone Monitoring Program
#' line, north east channel. Defining bounds are determined by station spacing. If the spacing is less than 8km,
#' then it is modified to half of the distance between the adjacent spacing. Note that this mean that the spacing
#' on either side can be uneven.
#'
#' @format a list which contains \code{stationName}, \code{longitude}, \code{latitude}, \code{polyLongitude},
#' \code{polyLatitude} for each station.
"northeastChannelStationPolygons"

#' Louisbourg polygon
#'
#' Polygon defining the bounds to look for data along the core Atlantic Zone Monitoring Program
#' line, louisbourg. It was constructed by finding the angle between the first point and the last point
#' of the line, and then calculating the bounding box which is 8 km wide.
#'
#' @format a list of \code{longitude} and \code{latitude}
"louisbourgPolygon"

#' Louisbourg channel station polygon's
#'
#' Polygons defining bounds to look for data at each station along the core Atlantic Zone Monitoring Program
#' line, Louisbourg. Defining bounds are determined by station spacing. If the spacing is less than 8km,
#' then it is modified to half of the distance between the adjacent spacing. Note that this mean that the spacing
#' on either side can be uneven.
#'
#' @format a list which contains \code{stationName}, \code{longitude}, \code{latitude}, \code{polyLongitude},
#' \code{polyLatitude} for each station.
"louisbourgStationPolygons"

#' Cabot strait polygon
#'
#' Polygon defining the bounds to look for data along the core Atlantic Zone Monitoring Program
#' line, Cabot strait. It was constructed by finding the angle between the first point and the last point
#' of the line, and then calculating the bounding box which is 8 km wide.
#'
#' @format a list of \code{longitude} and \code{latitude}
"cabotStraitPolygon"

#' Cabot strait station polygon's
#'
#' Polygons defining bounds to look for data at each station along the core Atlantic Zone Monitoring Program
#' line, Cabot Strait. Defining bounds are determined by station spacing. If the spacing is less than 8km,
#' then it is modified to half of the distance between the adjacent spacing. Note that this mean that the spacing
#' on either side can be uneven.
#'
#' @format a list which contains \code{stationName}, \code{longitude}, \code{latitude}, \code{polyLongitude},
#' \code{polyLatitude} for each station.
"cabotStraitStationPolygons"

#' St.Anns Bank polygon
#'
#' Polygon defining the bounds to look for data along the core Atlantic Zone Monitoring Program
#' line, St.Anns Bank. It was constructed by finding the angle between the first point and the last point
#' of the line, and then calculating the bounding box which is 8 km wide.
#'
#' @format a list of \code{longitude} and \code{latitude}
"stAnnsBankPolygon"

#' St.Anns Bank station polygon's
#'
#' Polygons defining bounds to look for data at each station along the core Atlantic Zone Monitoring Program
#' line, St.Anns Bank. Defining bounds are determined by station spacing. If the spacing is less than 8km,
#' then it is modified to half of the distance between the adjacent spacing. Note that this mean that the spacing
#' on either side can be uneven.
#'
#' @format a list which contains \code{stationName}, \code{longitude}, \code{latitude}, \code{polyLongitude},
#' \code{polyLatitude} for each station.
"stAnnsBankStationPolygons"

#' Portsmouth polygon
#'
#' Polygon defining the bounds to look for data along the core Atlantic Zone Monitoring Program
#' line, Portsmouth. It was constructed by finding the angle between the first point and the last point
#' of the line, and then calculating the bounding box which is 8 km wide.
#'
#' @format a list of \code{longitude} and \code{latitude}
"portsmouthPolygon"

#' Portsmouth station polygon's
#'
#' Polygons defining bounds to look for data at each station along the core Atlantic Zone Monitoring Program
#' line, Portsmouth. Defining bounds are determined by station spacing. If the spacing is less than 8km,
#' then it is modified to half of the distance between the adjacent spacing. Note that this mean that the spacing
#' on either side can be uneven.
#'
#' @format a list which contains \code{stationName}, \code{longitude}, \code{latitude}, \code{polyLongitude},
#' \code{polyLatitude} for each station.
"portsmouthStationPolygons"

#' Yarmouth polygon
#'
#' Polygon defining the bounds to look for data along the core Atlantic Zone Monitoring Program
#' line, Yarmouth. It was constructed by finding the angle between the first point and the last point
#' of the line, and then calculating the bounding box which is 8 km wide.
#'
#' @format a list of \code{longitude} and \code{latitude}
"yarmouthPolygon"

#' Yarmouth station polygon's
#'
#' Polygons defining bounds to look for data at each station along the core Atlantic Zone Monitoring Program
#' line, Yarmouth. Defining bounds are determined by station spacing. If the spacing is less than 8km,
#' then it is modified to half of the distance between the adjacent spacing. Note that this mean that the spacing
#' on either side can be uneven.
#'
#' @format a list which contains \code{stationName}, \code{longitude}, \code{latitude}, \code{polyLongitude},
#' \code{polyLatitude} for each station.
"yarmouthStationPolygons"

#' Laurentian Channel Mouth polygon
#'
#' Polygon defining the bounds to look for data along the core Atlantic Zone Monitoring Program
#' line, Laurentian Channel Mouth. It was constructed by finding the angle between the first point and the last point
#' of the line, and then calculating the bounding box which is 8 km wide.
#'
#' @format a list of \code{longitude} and \code{latitude}
"laurentianChannelMouthPolygon"

#' Laurentian Channel Mouth station polygon's
#'
#' Polygons defining bounds to look for data at each station along the core Atlantic Zone Monitoring Program
#' line, Laurentian Channel Mouth. Defining bounds are determined by station spacing. If the spacing is less than 8km,
#' then it is modified to half of the distance between the adjacent spacing. Note that this mean that the spacing
#' on either side can be uneven.
#'
#' @format a list which contains \code{stationName}, \code{longitude}, \code{latitude}, \code{polyLongitude},
#' \code{polyLatitude} for each station.
"laurentianChannelMouthStationPolygons"

#' St.Pierre Bank polygon
#'
#' Polygon defining the bounds to look for data along the core Atlantic Zone Monitoring Program
#' line, St.Pierre Bank. It was constructed by finding the angle between the first point and the last point
#' of the line, and then calculating the bounding box which is 8 km wide.
#'
#' @format a list of \code{longitude} and \code{latitude}
"stPierreBankPolygon"

#' St.Pierre Bank station polygon's
#'
#' Polygons defining bounds to look for data at each station along the core Atlantic Zone Monitoring Program
#' line, St.Pierre. Defining bounds are determined by station spacing. If the spacing is less than 8km,
#' then it is modified to half of the distance between the adjacent spacing. Note that this mean that the spacing
#' on either side can be uneven.
#'
#' @format a list which contains \code{stationName}, \code{longitude}, \code{latitude}, \code{polyLongitude},
#' \code{polyLatitude} for each station.
"stPierreBankStationPolygons"

#' Sable Island Bank polygon
#'
#' Polygon defining the bounds to look for data along the core Atlantic Zone Monitoring Program
#' line, Sable Island Bank. It was constructed by finding the angle between the first point and the last point
#' of the line, and then calculating the bounding box which is 8 km wide.
#'
#' @format a list of \code{longitude} and \code{latitude}
"sableIslandBankPolygon"

#' Sable Island Bank station polygon's
#'
#' Polygons defining bounds to look for data at each station along the core Atlantic Zone Monitoring Program
#' line, Sable Island Bank. Defining bounds are determined by station spacing. If the spacing is less than 8km,
#' then it is modified to half of the distance between the adjacent spacing. Note that this mean that the spacing
#' on either side can be uneven.
#'
#' @format a list which contains \code{stationName}, \code{longitude}, \code{latitude}, \code{polyLongitude},
#' \code{polyLatitude} for each station.
"sableIslandBankStationPolygons"

#' Roseway polygon
#'
#' Polygon defining the bounds to look for data along the core Atlantic Zone Monitoring Program
#' line, Roseway. It was constructed by finding the angle between the first point and the last point
#' of the line, and then calculating the bounding box which is 8 km wide.
#'
#' @format a list of \code{longitude} and \code{latitude}
"rosewayPolygon"

#' Roseway station polygon's
#'
#' Polygons defining bounds to look for data at each station along the core Atlantic Zone Monitoring Program
#' line, Roseway. Stations include RL_00, RL_02 : RL_08, RL_01 is omitted as it is not along the transect.
#' Defining bounds are determined by station spacing. If the spacing is less than 8km,
#' then it is modified to half of the distance between the adjacent spacing. Note that this mean that the spacing
#' on either side can be uneven.
#'
#' @format a list which contains \code{stationName}, \code{longitude}, \code{latitude}, \code{polyLongitude},
#' \code{polyLatitude} for each station.
"rosewayStationPolygons"

#' La Have Bank polygon
#'
#' Polygon defining the bounds to look for data along the core Atlantic Zone Monitoring Program
#' line, La Have Bank. It was constructed by finding the angle between the first point and the last point
#' of the line, and then calculating the bounding box which is 8 km wide.
#'
#' @format a list of \code{longitude} and \code{latitude}
"laHavePolygon"

#' La Have Bank station polygon's
#'
#' Polygons defining bounds to look for data at each station along the core Atlantic Zone Monitoring Program
#' line, La Have Bank. Defining bounds are determined by station spacing. If the spacing is less than 8km,
#' then it is modified to half of the distance between the adjacent spacing. Note that this mean that the spacing
#' on either side can be uneven.
#'
#' @format a list which contains \code{stationName}, \code{longitude}, \code{latitude}, \code{polyLongitude},
#' \code{polyLatitude} for each station.
"laHaveBankStationPolygons"


