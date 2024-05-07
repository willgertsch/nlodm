# function for constructing objective function for use with algorithms
# multi-objective: construct function that returns a vector
# M_fun: information matrix function
# obj_fun: objective function of information matrix
# theta: parameter values to pass through to M_fun
# params: other parameters, such as c values for c objective
# binary_responses: boolean vector telling where a binary response should be assumed
# dr_funs: list of dose response functions
multi_obj_fun_factory = function(grad_funs, obj_funs, thetas, params,
                                 binary_responses, dr_funs) {

  # these are used in interface function
  force(grad_funs)
  force(thetas)
  force(params)

  # check dimensions of the problem are correct
  d = length(obj_funs)
  if (length(grad_funs) !=  d)
    stop("number of objective functions doesn't match number of gradient functions")
  else if (length(thetas) != d)
    stop("number of theta arrays doesn't match number of objectives")
  else if (length(params) != d)
    stop("number of additional parameter vectors doesn't match number of objectives")

  # interface called by optimization software: nsga2
  # return this function
  function(vars, nobj = d, ...) {

    #browser()
    # distinguish between points and weights
    pts = length(vars)/2
    x = vars[1:pts]
    w = vars[(pts+1):(2*pts)]

    # check weight constraint
    s = sum(w, na.rm = T) # na.rm needed to fix if statement error
    if (s > 1) # constraint implementation
      return(rep(Inf, nobj))

    M_fun = M.nonlinear # always using general nonlinear matrix

    # call objective functions
    # note the sign change is needed because nsga2R minimizes
    obj_vals = numeric(nobj)
    for (i in 1:length(grad_funs)) {
      obj_vals[i] = -obj_funs[[i]](M_fun(x, w, thetas[[i]], grad_funs[[i]], binary_responses[i], dr_funs[[i]]), params[[i]])
    }

    # deal with missing
    # vectorize
    nas = is.na(obj_vals)
    obj_vals[nas] = Inf

    return(obj_vals)
  }

}
