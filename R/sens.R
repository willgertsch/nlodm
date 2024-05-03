# sensitivity function

# general function
# z: independent variable
# grad: gradient function
# dPsi: derivative of the objective function wrt M
# M: information matrix
# theta: model parameters
# binary_response: set to true to adjust for non-constant variance in binomial response
# dr_fun: dose response function for use with binary response function
sens = function(z, grad, dPsi, M, theta, param, binary_response, dr_fun) {

  dg = grad(z, theta)
  dM = dPsi(M, param)
  if (binary_response) {
    p = dr_fun(z, theta)
    v = 1/(p * (1 - p))

  }
  else
    v = 1

  y = v * t(dg) %*% dM %*% dg - sum(diag(M %*% dM))
  return(y)
}
