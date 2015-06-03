# Sebastian Sippel, 15.02.2015

# Function to determine trend component with SSA:
require(spectral.methods)

#' @title A function to estimate nonlinear trends
#' @export
#' @description Estimate the nonlinear trend components of a time series using Singular Spectrum Analysis. A wrapper function around filterTSeriesSSA (spectral.methods)
#' @usage SSA.detrend(data, borders.wl, M, n.comp)
#' @param data A numeric vector (e.g. a time series), from which the trend component is to be estimated
#' @param borders.wl See ?filterTSeriesSSA
#' @param M See ?filterTSeriesSSA
#' @param n.comp See ?filterTSeriesSSA
#' @details 
#' This function calculates the nonlinear trend component. Currently, no padding is implemented, but will come soon.
#' @return A character vector of length(data) that contains the trend components.
SSA.detrend <- function(data, borders.wl, M, n.comp) {
  if(any(is.na(data)) | length(which(diff(data) == 0)) > 10) {
    return(rep(NA, length(data))) 
  } else {
    # subtract seasonal cycle and trend and return:
    ssa.object <- filterTSeriesSSA(series=data, borders.wl = borders.wl, plot.spectra=F, M=M, n.comp=n.comp)
    #ts.return = c(data - (ssa.object$dec.series[1,]))
    ts.return = c(ssa.object$dec.series[1,])
    return(ts.return)
  }
}

# Function to estimate the linear trend components:
#' @title A function to estimate linear trends
#' @export
#' @description Estimates a linear trend.
#' @usage linear.detrend(data)
#' @param data A numeric vector (e.g. a time series), from which the trend component is to be estimated
#' @details 
#' This function calculates a linear trend component to use for spatio-temporal normalization.
#' @return A character vector of length(data) that contains the trend components.
linear.detrend <- function(data) {
  if (any(is.na(data))) {
    return(rep(NA, length(data)))
  } else {
    return(lm(data ~ c(1:length(data)))$fitted.values)
  }
}


