# Sebastian Sippel, 15.02.2015

# Function to determine trend component with SSA:
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
linear.detrend <- function(data) {
  if (any(is.na(data))) {
    return(rep(NA, length(data)))
  } else {
    return(lm(data ~ c(1:length(data)))$fitted.values)
  }
}