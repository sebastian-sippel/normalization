# transform tau to normal distribution based on Thompson 1935!
transform.tau.to.normal <- function(tau, ref.period.length = 15) {
  
  # get tau derived from biased variance estimator:
  tau = tau / (sqrt((ref.period.length - 1) / ref.period.length))
  
  # transform tau to t-dist (automatically corrects for anomalies):
  t = tau * sqrt(ref.period.length - 2) / sqrt((ref.period.length - 2) + 1 - tau^2)
  
  # transform t-distribution to Gaussian distribution:
  norm.val = transform.noncentral.t.to.normal(data=t, df=ref.period.length - 2, delta = 0)
  return(norm.val)
}

