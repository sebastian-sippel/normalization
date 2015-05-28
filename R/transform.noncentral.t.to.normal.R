# Sebastian Sippel
# 28.05.2015

transform.noncentral.t.to.normal <- function(data, delta = 0, df, anom.cor = F) {
  if (anom.cor == F) {
    prob.t = pt(q=data, df=df, ncp=delta)
  } else if (anom.cor == T) {
    var.cor = sqrt(1 + 1/ (df+1))
    prob.t = pt(q=data / var.cor, df=df, ncp=delta / var.cor)
  }
  normal.equivalent = qnorm(p=prob.t) + delta  # changed and checked on 20.03.2015
  return(normal.equivalent)
}

