# function for constructing objective function for use with algorithms
# M_fun: information matrix function
# obj_fun: objective function of information matrix
# theta: parameter values to pass through to M_fun
# par: other parameters, such as c values for c objective
obj_fun_factory = function(grad_fun, obj_fun, theta, param, prior_weights = c(1)) {

  # these are used in interface function
  force(grad_fun)
  force(theta)
  force(param)

  # if theta is vector, convert to matrix
  if (is.vector(theta))
    theta = matrix(theta, nrow = 1)

  # interface called by optimization software
  # return this function
  function(vars, ...) {
    # distinguish between points and weights
    pts = length(vars)/2
    x = vars[1:pts]
    w = vars[(pts+1):(2*pts)]

    # check weight constraint
    s = sum(w, na.rm = T) # na.rm needed to fix if statement error
    if (s > 1) # constraint implementation
      return(-Inf)

    M_fun = M.nonlinear # always using general nonlinear matrix

    # average over prior theta values
    obj_value = 0
    p = nrow(theta)
    for (i in 1:p) {
      obj_value = obj_value +
        obj_fun(M_fun(x, w, theta[i, ], grad_fun), param) * prior_weights[i]
    }


    # deal with missing
    if (is.na(obj_value))
      return(-Inf)
    else
      return(obj_value)
  }

}
