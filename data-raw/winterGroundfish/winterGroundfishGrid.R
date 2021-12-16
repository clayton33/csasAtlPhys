# load the lon lat grid
llgridFile <- 'data-raw/winterGroundfish/oegrid_ll.txt'
winterGroundfishLonLatGrid <- read.table(llgridFile)
# name some things, and keep certain columns
grid <- winterGroundfishLonLatGrid
names(grid) <- c('y', 'x','unkn1', 'depth', 'unkn2', 'unkn3', 'unkn4', 'unkn5')
grid <- grid[ ,names(grid) %in% c('x', 'y', 'depth')]
# get full x, y rectangular grid for input into barnes, note this can only be done with lon lat grid
uxy <- unique(grid[ , names(grid) %in% c('x', 'y')])
xseq <- seq(min(uxy[['x']]), max(uxy[['x']]), 0.2)
yseq <- seq(min(uxy[['y']]), max(uxy[['y']]), 0.2)
fullgrid <- expand.grid(x = xseq, y = yseq)
#   2. define grid standard depths and tolerances
gridStandardDepths <- c(seq(0, 100, 10),
                        seq(125, 250, 25),
                        300,
                        seq(400, 1000, 100))
gridTolerance <- c(head(diff(gridStandardDepths), 1), diff(gridStandardDepths))
#   3. append a depth threshold and find which points are the bottom depth
tolerance <- bottom <- vector(length = dim(grid)[1])
nbtm <- 0
for(i in 1:dim(uxy)[1]){
  xlook <- uxy[['x']][i]
  ylook <- uxy[['y']][i]
  ok <- which(grid[['x']] == xlook & grid[['y']] == ylook)
  # set the tolerance
  tol <- gridTolerance[match(grid[['depth']][ok], gridStandardDepths)]
  tol[is.na(tol)] <- 5 # for bottom depth, only use +/- 5m
  tolerance[ok] <- tol
  # find which index is bottom
  isBottom <- rep(FALSE, length(ok))
  okBottom <- which.max(grid[['depth']][ok])
  bottomDepth <- grid[['depth']][ok][okBottom]
  if(bottomDepth %in% gridStandardDepths){
    bottom[ok] <- isBottom
    nbtm <- nbtm + 1
  } else {
    isBottom[okBottom] <- TRUE
    bottom[ok] <- isBottom
  }

}
winterGroundfishGrid <- data.frame(grid, tolerance = tolerance, isBottom = bottom)
winterGroundfishStandardDepths <- gridStandardDepths
winterGroundfishXg <- xseq
winterGroundfishYg <- yseq

usethis::use_data(winterGroundfishGrid, compress = "xz", overwrite = T)
usethis::use_data(winterGroundfishStandardDepths, compress = "xz", overwrite = T)
usethis::use_data(winterGroundfishXg, compress = 'xz', overwrite = T)
usethis::use_data(winterGroundfishYg, compress = 'xz', overwrite = T)

