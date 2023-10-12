# function for constructing objective function for use with algorithms
# M_fun: information matrix function
# obj_fun: objective function of information matrix
# theta: parameter values to pass through to M_fun
# par: other parameters, such as c values for c objective
obj_fun_factory = function(grad_funs, obj_funs, thetas, params) {

  # these are used in interface function
  force(grad_funs)
  force(thetas)
  force(params)

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
    # call objective functions
    obj_vals = list()
    for (i in 1:length(grad_funs)) {
      obj_vals[i] = obj_fun(M_fun(x, w, thetas[i], grad_funs[i]), params[i])
    }


    # deal with missing
    # vectorize
    if (is.na(obj_value))
      return(-Inf)
    else
      return(obj_value)
  }

}
