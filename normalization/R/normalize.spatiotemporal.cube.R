# Normalization of a dataset ("normalization toolbox"):
# Sebastian Sippel
# 09.02.2015

# INPUT
# data: 3-dimensional array of the form longiutde-latitude-time
# data.trend (optional): 3-dimensional array with trend components in the form longitude-latitude-time or NA
# SUBTRENDSD: logical, should trend components be removed before SD estimates are computed?
# TRENDCOR: logical, should trend components in the out-of-base period be corrected?
# ref.idx: Ref. period given as indices of the temporal dimension of the spatio-temporal data cube

# OUTPUT:
# list with three elements:
# $data.original: original data array (3D-array)
# $data.norm: normalized array using the conventionally applied normalization
# $data.norm.cor: normalized array using the proposed correction


#' @title A function to normalize a spatio-temporal data cube
#' @export
#' @description Normalization of a spatio-temporal data cube based on a reference period and correction for induced artefacts.
#' @usage normalize.spatiotemporal.cube(data, data.trend=NA, SUBTRENDSD=F, TRENDCOR=F, ref.idx = c(1,30))
#' @param data 3-dimensional array of the form longiutde-latitude-time
#' @param data.trend (optional): 3-dimensional array with trend components in the form longitude-latitude-time or NA
#' @param SUBTRENDSD Logical, should the trend be subtracted before computing the standard deviation in the reference period?
#' @param TRENDCOR Logical, should the normalization correct for trends in the out-of-base period?
#' @param ref.idx Numeric Vector of indices that specify the reference period (in time)
#' @details 
#' This function returns a normalized spatiotemporal data cube that can be compared across space and time, given that prerequisites are fulfilled (Gaussian data, stationarity in the reference period).
#' @return List with three elements: 
#' $data.original: original data array (3D-array)
#' $data.norm: normalized array using the conventionally applied normalization
#' $data.norm.cor: normalized array using the proposed correction
#' @references Sippel et al, (2015) An accurate quantification of climate variability and extremes. Submitted.
#' @author Sebastian Sippel
normalize.spatiotemporal.cube <- function(data, data.trend=NA, SUBTRENDSD=F, TRENDCOR=F, ref.idx = c(1,30)) {
  
  # Dimensionality of the data:
  dim.data = dim(data)
  dim.data.reshape = dim.data[1] * dim.data[2]
  
  # indices inside and outside the reference period:
  # idx.ref = c(start.ref.idx:end.ref.idx)
  idx.ref = ref.idx
  idx.noref = which(!(c(1:dim.data[3]) %in% idx.ref))
  
  
  # make data and data.trend to matrix, so that sapply can be used:
  data.reshape = apply(data, 3, rbind)
  
  # calculate mean and SD estimates:
  mean.estimate = sapply(1:dim.data.reshape, FUN=function(x) calculate.mean.grid.cell(data.reshape[x,idx.ref]))
  
  # calculate SD estimates:
  if (SUBTRENDSD == F) {
    sd.estimate = sapply(1:dim.data.reshape, FUN=function(x) calculate.sd.grid.cell(data.reshape[x,idx.ref]))
  } else if (SUBTRENDSD == T) {
    data.trend.reshape = apply(data.trend, 3, rbind)
    sd.estimate = sapply(1:dim.data.reshape, FUN=function(x) calculate.sd.grid.cell(data.reshape[x,idx.ref] - data.trend.reshape[x,idx.ref]))
  }
  
  # normalize reshaped data:
  data.norm.reshape = sapply(1:dim.data.reshape, FUN=function(x) return((data.reshape[x,] - mean.estimate[x]) / sd.estimate[x]) )
  data.norm.reshape = aperm(data.norm.reshape, perm=c(2,1))
  
  # transform tau distribution back to normal:
  data.norm.reshape.ref.cor = transform.tau.to.normal(tau=data.norm.reshape[,idx.ref], ref.period.length=length(idx.ref))
  
  # transform t-distribution back to normal:  
  if (TRENDCOR == F) {
    data.norm.reshape.obase.cor = transform.noncentral.t.to.normal(data=data.norm.reshape[,idx.noref], delta=0, df=(length(idx.ref)-1), anom.cor = T)
  } else if (TRENDCOR == T) {
    # define and normalize trend:
    data.trend.reshape = apply(data.trend, 3, rbind)
    data.trend.norm.reshape = aperm(a=sapply(X=1: dim.data.reshape, FUN=function(x) (data.trend.reshape[x,] - mean.estimate[x])/sd.estimate[x] ), perm=c(2,1))
    # correct out-of-base period, including trend:
    data.norm.reshape.obase.cor = aperm(a=sapply(1:dim.data.reshape, FUN=function(x) transform.noncentral.t.to.normal(data=data.norm.reshape[x,idx.noref], delta=data.trend.norm.reshape[x,idx.noref], df=(length(idx.ref)-1), anom.cor=T)), perm=c(2,1))
  }
  
  # put arrays together and reshape:
  data.original = array(data=data, dim=dim.data)
  data.norm = array(data=data.norm.reshape, dim=dim.data)
  data.norm.cor = array(data=cbind(data.norm.reshape.ref.cor,data.norm.reshape.obase.cor), dim=dim.data)
  
  # create list to return stuff:
  list.out = list(data.original, data.norm, data.norm.cor)
  names(list.out) <- c("data.original", "data.norm", "data.norm.cor")
  
  return(list.out)
}  

