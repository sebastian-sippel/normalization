# --------------------------------------------------------------------------------
# Functions to normalize raster data: MAKE THIS INTERNAL FUNCTIONS!
# --------------------------------------------------------------------------------
calculate.mean.grid.cell <- function(x) {
  if (any(is.na(x))) {
    return(NA)
  } else {
    return(mean(x))
  }
}

calculate.sd.grid.cell <- function(x) {
  if (any(is.na(x))) {
    return(NA)
  } else {
    return(sd(x))
  }
}

