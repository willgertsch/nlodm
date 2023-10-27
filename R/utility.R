# utility functions

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
  else
    stop('Model not supported')

  return(grad_fun)
}

# input switching for using predefined or custom model
# will always default to grad_fun if supplied
get_grad = function(model, grad_fun) {

  if (!is.null(grad_fun) & is.function(grad_fun))
    return(grad_fun)
  else if (!is.null(model))
    return(grad_selector(model))
  else
    stop('gradient not defined')
}

# select bmd gradient function
get_bmd_grad = function(model, risk_type) {

  if (is.null(model)) {
    stop("Specify a model")
  }
  if (model == "Logistic") {
    grad_fun = grad.logistic

    if (risk_type == "added") {
      bmd_grad = bmdgrad.logistic.add
    }
    else if (risk_type == "extra")
      bmd_grad = bmdgrad.logistic.extra
  }
  else if (model == "Weibull") {
    grad_fun = grad.weibull

    if (risk_type == "added") {
      bmd_grad = bmdgrad.weibull.add
    }
    else if (risk_type == "extra") {
      bmd_grad = bmdgrad.weibull.extra
    }
  }
  else if (model == "Log-logistic") {
    grad_fun = grad.loglogistic

    if (risk_type == "added") {
      bmd_grad = bmdgrad.loglogistic.add
    }
    else if (risk_type == "extra") {
      bmd_grad = bmdgrad.loglogistic.extra
    }
  }
  else if (model == "Hill") {
    grad_fun = grad.hill

    if (risk_type == "added") {
      bmd_grad = bmdgrad.hill.add
    }
    else if (risk_type == "extra") {
      bmd_grad = bmdgrad.hill.extra
    }
  }

  return(bmd_grad)
}

# enhanced solve() function that adds 1E-5 to diagonal if matrix is singular
ridge_solve = function(M) {

  if (checkMinv(M))
    return(M)
  else {
    Mridge = M + diag(1e-5, nrow(M))
    return(solve(Mridge))
  }
}
