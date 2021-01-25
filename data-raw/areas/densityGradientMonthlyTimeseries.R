library(usethis)
path <- 'data-raw/areas/densityGradientClimatology'
ltm2 <- read.table(paste(path,'TSDLongTermMeans.txt', sep = '/'))
names(ltm2) <- c('area', 'month',
                 'temperature0m', 't0mprofiles', 'temperature50m', 't50mprofiles',
                 'salinity0m', 's0mprofiles', 'salinity50m', 's50mprofiles',
                 'sigmaTheta0m', 'st0mprofiles', 'sigmaTheta50m', 'st50mprofiles')
ltm <- read.table(paste(path, 'Long_Term_Means.txt', sep = '/'))
names(ltm) <- c('area', 'month', 'salinityGradient', 'sigmaThetaGradient', 'nprofiles')
na <- ltm == -99.00
ltm[na] <- NA

# merge the two data sets together
ltmall <- merge(ltm2, ltm, by = c('area', 'month'))

# rename some of the variables
look <- c('t0mprofiles', 't50mprofiles', 's0mprofiles', 's50mprofiles', 'st0mprofiles', 'st50mprofiles', 'nprofiles')
replace <- c('temperature0mNprofiles', 'temperature50mNprofiles', 'salinity0mNprofiles', 'salinity50mNprofiles', 'sigmaTheta0mNprofiles', 'sigmaTheta50mNprofiles', 'gradientNprofiles')
dfnames <- data.frame(look = look,
                      replace = replace)
ok <- apply(dfnames, 1, function(k) which(names(ltmall) == k[['look']]))
names(ltmall)[ok] <- dfnames[['replace']]

densityGradientMonthlyClimatology1981to2010 <- ltmall
usethis::use_data(densityGradientMonthlyClimatology1981to2010, compress = "xz", overwrite = T)

