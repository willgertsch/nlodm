# function for constructing objective function for use with algorithms
# M_fun: information matrix function
# obj_fun: objective function of information matrix
# theta: parameter values to pass through to M_fun
# par: other parameters, such as c values for c objective
# exact: T or F to return function for exact designs
# binary_response: set to true to adjust for non-constant variance in binomial response
# dr_fun: dose response function for use with binary response
obj_fun_factory = function(grad_fun, obj_fun, theta, param, prior_weights = c(1),
                           exact = F, binary_response, dr_fun) {

  # these are used in interface function
  force(grad_fun)
  force(theta)
  force(param)

  # construct interface called by optimization software
  if (exact) {
    function(vars, ...) {
      pts = length(vars)
      x = vars

      M_fun = M.nonlinear.exact # use exact nonlinear matrix

      # average over prior theta values and parameter values
      obj_value = 0
      p = nrow(theta)
      for (i in 1:p) {
        obj_value = obj_value +
          obj_fun(M_fun(x, theta[i, ], grad_fun, binary_response, dr_fun), as.vector(param[i, ])) * prior_weights[i]
      }

      # deal with missing
      if (is.na(obj_value))
        return(-Inf)
      else
        return(obj_value)
    }
  }
  else {
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

      # average over prior theta values and parameter values
      obj_value = 0
      p = nrow(theta)
      for (i in 1:p) {
        obj_value = obj_value +
          obj_fun(M_fun(x, w, theta[i, ], grad_fun, binary_response, dr_fun), as.vector(param[i, ])) * prior_weights[i]
      }


      # deal with missing
      if (is.na(obj_value))
        return(-Inf)
      else
        return(obj_value)
    }
  }


}
