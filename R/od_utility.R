# utility functions for optimal design

# checks if information matrix is invertible
# returns 1 if invertible and 0 if not
# can optimize easily for 2 dim
checkMinv = function(M) {

  if (class(try(solve(M),silent=T))[1]!="matrix")
    return(0)
  else
    return(1)
}

# model selector
# returns gradient function
grad_selector = function(model) {

  if (model == "Logistic")
    grad_fun = grad.logistic
  else if (model == "Logistic quadratic")
    grad_fun = grad.logistic.quad
  else if (model == "Logistic cubic")
    grad_fun = grad.logistic.cubic
  else if (model == "Logistic fractional polynomial")
    grad_fun = grad.logistic.fp
  else if (model == "Weibull")
    grad_fun = grad.weibull
  else if (model == "Log-logistic")
    grad_fun = grad.loglogistic
  else if (model == "Mixture multistage")
    grad_fun = grad.mix2
  else if (model == "Box-Cox Weibull")
    grad_fun = grad.boxcoxweibull
  else if (model == "Hill")
    grad_fun = grad.hill
  else if (model == "Multistage 1")
    grad_fun = grad.multi1
  else if (model == "Multistage 2")
    grad_fun = grad.multi2
  else if (model == "Multistage 3")
    grad_fun = grad.multi3
  else if (model == "Probit")
    grad_fun = grad.probit
  else if (model == "Log-probit")
    grad_fun = grad.logprobit
  else if (model == "4 parameter log-logistic")
    grad_fun = grad.loglogistic4

  return(grad_fun)
}
