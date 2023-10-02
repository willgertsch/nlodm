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
