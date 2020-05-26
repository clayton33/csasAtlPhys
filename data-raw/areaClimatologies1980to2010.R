path <- 'data-raw'
files <- list.files(path = path,
                    pattern = 'Area\\d+_1981-2010\\.RData')


areaClimatologies1980to2010 <- vector(mode = 'list', length = length(files))
areaNames[i] <- vector(length = length(files))
for(i in 1:length(files)){
  load(paste(path, files[i], sep = '/'))
  # each file has a variable named 'ltm'
  names(ltm) <- tolower(names(ltm)) # do this for sanity
  areaNames[i] <- tolower(gsub('^(Area\\d+)_.*', '\\1', files[i]))
  areaClimatologies1980to2010[[i]] <- ltm
}
names(areaClimatologies1980to2010) <- areaNames

usethis::use_data(areaClimatologies1980to2010, compress = "xz", overwrite = T)
