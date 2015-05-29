### Normalization of spatio-temporal data cubes:
# Author:   Sebastian Sippel
# Date:     09.02.2015

# Description: This script is to standardize a spatio-temporal data cube, without inducing spatial artefacts!
# Prerequisites: The correction approach is only valid for i.i.d. Gaussian variables,
#                 i.e. no autocorrelation, independence, constant variance!
# Trend estimation: Trends are estimated:
#                   - for a correction of trends in the out-of-base period with the non-central t-distribution
#                   - trends in the reference period can be removed before computing the standard deviation (optional)

# CAUTIONARY NOTE:
# This standardization procedure removes spatial artefacts induced by reference period normalization
# for Gaussian variables.
# However, since the reference and out-of-base period are corrected differently, this induces
# inhomogeneities into single time series. Thus, the standardization correction should
# only be applied, if structures across time series (i.e. spatial variability, 
# number of grid cells experiencing extremes, etc.) are to be investigated! 
# The number of spatial replicates needs to be sufficiently large.


# R-packages required:
require(spectral.methods)   # Singular-Spectrum Analysis to remove trends
library(normalization)


# ----------------------------------------------------------------------------
# 1. Input dataset:
# ----------------------------------------------------------------------------
# generate a spatio-temporal data-cube with various dimension:
xdim = 100
ydim = 100
tdim = 60
  
data.cube.original = array(data=rnorm(n=xdim*ydim*tdim, mean=0, sd=1), dim=c(xdim, ydim, tdim))

# ALTERNATIVE: Load your own data!

# ----------------------------------------------------------------------------
# 2. Estimate trend components per grid cell:
# ----------------------------------------------------------------------------
# estimate trend components using SSA (or any other method): 
data.cube.trend = apply(X=data.cube.original, MARGIN=c(1,2), FUN=SSA.detrend, borders.wl=list(c(31,Inf)), M=45, n.comp=6)
# swap array dimension to original ones:
data.cube.trend = aperm(a=data.cube.trend, perm=c(2,3,1))


# ---------------------------------------------------------------------------
# 3. normalization of the data cube and correction:
# ---------------------------------------------------------------------------
# normalize each grid cell based on a reference period and apply correction in one function:
data.cube.norm = normalize.spatiotemporal.cube(data=data.cube.original, data.trend=data.cube.trend, SUBTRENDSD=F, TRENDCOR=F, ref.idx = c(1:30))

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

# -----------------------------------------------------------------------------
# 4. Detection of extremes in the data cube
# -----------------------------------------------------------------------------
data.original =     apply(X=data.cube.norm$data.original, MARGIN=c(3), FUN=function(x) length(which(x > 2)))
data.norm =         apply(X=data.cube.norm$data.norm, MARGIN=c(3), FUN=function(x) length(which(x > 2)))
data.norm.cor =     apply(X=data.cube.norm$data.norm.cor, MARGIN=c(3), FUN=function(x) length(which(x > 2)))

plot(data.norm, type='l', col="red")
lines(data.original, col="black")
lines(data.norm.cor, col="darkblue")


# -----------------------------------------------------------------------------
# 5. "Spatial" variability in the dataset
# -----------------------------------------------------------------------------
data.original.var =     apply(X=data.cube.norm$data.original, MARGIN=c(3), FUN=function(x) var(c(x)))
data.norm.var =         apply(X=data.cube.norm$data.norm, MARGIN=c(3), FUN=function(x) var(c(x)))
data.norm.cor.var =     apply(X=data.cube.norm$data.norm.cor, MARGIN=c(3), FUN=function(x) var(c(x)))

plot(data.norm.var, type='l', col="red", ylim=c(0.9,1.15))
lines(data.original.var, col="black")
lines(data.norm.cor.var, col="darkblue")


