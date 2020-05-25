#' Get the present time, in a stated timezone
#'
#' @param tz String indicating the desired timezone. The default is
#' to use UTC, which is used very commonly in oceanographic work. To
#' get the local time, use `tz=""` or `tz=NULL`,
#'
#' @examples
#' presentTime() # UTC
#' presentTime("") # the local timezone
#'
#' @return A [POSIXct()]-style object holding the present time, in the
#' indicated timezone.
#' @author Dan Kelley
#' @keywords internal
#' @export
#'
## Since I'm using similar conventions to oce, I'm using some similar code
## presentTime() is a function that is not exported, so I'll add it here for use
## NOTE: we need to define this here so [setClass()] knows about it;
## NOTE: having it in NAMESPACE is not sufficient.
presentTime <- function(tz="UTC")
{
  t <- Sys.time()
  if (!is.null(tz) && nchar(tz) > 0)
    attr(t, "tzone") <- tz
  t
}


