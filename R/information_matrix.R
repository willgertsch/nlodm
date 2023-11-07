# information matrix
# x: array of design points
# w: array of weights
# theta: array of parameter values
# most general case
# grad_fun: gradient function to use
M.nonlinear = function(x, w, theta, grad_fun) {

  IM = 0
  for (i in 1:length(x)) {
    IM_i = w[i] * grad_fun(x[i], theta) %*% t(grad_fun(x[i],theta))
    IM = IM + IM_i
  }
  IM
}

# return a list of information matrices
# treats theta as a matrix
M.nonlinear.list = function(x, w, theta, grad_fun) {

  M.list = list()
  p = nrow(theta)
  for (i in 1:p) {
    M.list[[i]] = M.nonlinear(x, w, theta[i, ], grad_fun)
  }

  return(M.list)
}

# information matrices for finding exact designs
# general non-linear
M.nonlinear.exact = function(x, theta, grad_fun) {

  IM = 0
  for (i in 1:length(x)) {
    IM_i = grad_fun(x[i], theta) %*% t(grad_fun(x[i],theta))
    IM = IM + IM_i
  }
  IM
}
