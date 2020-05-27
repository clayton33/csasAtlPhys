library(usethis)
path <- 'data-raw/areas/monthlyTimeseries'
files <- list.files(path = path,
                    pattern = 'Area.*\\.rda')

areaMonthlyTimeseries <- vector(mode = 'list', length = length(files))
areaNames <- vector(length = length(files))
for(i in 1:length(files)){
  load(paste(path, files[i], sep = '/'))
  # each file has a variable named 'monthlyts'
  areaNames[i] <- tolower(gsub('^(Area\\d+).*', '\\1', files[i]))
  areaMonthlyTimeseries[[i]] <- monthlyts
}
names(areaMonthlyTimeseries) <- areaNames

usethis::use_data(areaMonthlyTimeseries, compress = "xz", overwrite = T)
