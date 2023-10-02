# sensitivity function

# general function
# z: independent variable
# grad: gradient function
# dPsi: derivative of the objective function wrt M
# M: information matrix
# theta: model parameters
sens = function(z, grad, dPsi, M, theta, param) {

  dg = grad(z, theta)
  dM = dPsi(M, param)
  y = t(dg) %*% dM %*% dg - sum(diag(M %*% dM))
  return(y)
}
