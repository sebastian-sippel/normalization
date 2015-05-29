# --------------------------------------------------------------------------------
# Functions to normalize raster data: MAKE THIS INTERNAL FUNCTIONS!
# --------------------------------------------------------------------------------
#' @keywords internal
#' @export
calculate.mean.grid.cell <- function(x) {
  if (any(is.na(x))) {
    return(NA)
  } else {
    return(mean(x))
  }
}


#' @keywords internal
#' @export
calculate.sd.grid.cell <- function(x) {
  if (any(is.na(x))) {
    return(NA)
  } else {
    return(sd(x))
  }
}

