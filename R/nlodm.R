# main function for finding optimal designs
# model: model to be used, this will select from model library
# user can implement a custom model using the grad_fun option
# grad_fun: supply gradient function of model
# obj: name of objective: A or D or bmd
# theta: either a vector of parameter values or matrix for Bayesian designs
# bound: design space is bounded [0, bound]
# pts: number of design points
# algorithm: metaheuristic to use
# swarm: size of swarm
# iter: max number of iterations
# seed:
# bmd_type: either 'added' or 'extra
# risk: risk increase for bmd
# lambda: weighting parameter for bmd designs
# c: c vector for c-optimal designs
# exact: T or F, exact design will be found
# exact_digits: combine doses up to this number of places
# binary_responses: set to true to adjust for non-constant variance in binomial response
# dr_fun: dose response function for use with binary response function
nlodm = function(
    model = NULL,
    grad_fun,
    obj,
    theta,
    prior_weights = c(1),
    bound,
    pts,
    algorithm,
    swarm,
    iter,
    seed,
    bmd_type = 'added',
    risk = 0.1,
    lambda = 0.5,
    c = NULL,
    exact = F,
    exact_digits = 4,
    binary_response = F,
    dr_fun = NULL
) {

  if (binary_response & is.null(dr_fun))
    stop('nlodm: design for a binary response requires a dose response function be specified for the variance adjustment.')


  # get gradient function
  grad_fun = get_grad(model, grad_fun)


  # if theta is vector, convert to matrix
  if (is.vector(theta))
    theta = matrix(theta, nrow = 1)
  # same with c
  if (is.vector(c))
    c = matrix(c, nrow = 1)


  theta_dims = dim(theta)
  c_dims = dim(c)

  # input checking
  # design objective
  if (obj == "D") {
    obj_fun = obj.D
    param = matrix(nrow = theta_dims[1], ncol = theta_dims[2])
  }
  else if (obj == "A") {
    obj_fun = obj.A
    param = matrix(nrow = theta_dims[1], ncol = theta_dims[2])
  }
  else if (obj == 'bmd') {
    bmd_grad = get_bmd_grad(model, bmd_type)
    obj_fun = obj.bmd

    # deal with case of multiple theta values
    # c is computed for each value of theta
    # lambda remains constant
    if (theta_dims[1] == 1)
      c = matrix(bmd_grad(risk, theta), nrow = 1)
    if (theta_dims[1] > 1)
      c = t(apply(theta, 1, function(theta_i) bmd_grad(risk, theta_i)))

    #param = c(lambda, c)
    param = cbind(rep(lambda, theta_dims[1]), c)
  }
  else if (obj == 'c') {

    # if c is a single row, duplicate for all theta
    # error if c has more rows than theta
    # take first value only if not enough values supplied
    if (c_dims[1] == 1)
    {
      cat('nlodm: Only 1 value of c vector supplied. This will be duplicated for all theta.\n')
      param = matrix(rep(c, theta_dims[1]), nrow = theta_dims[1], byrow = T)
    }
    else if (c_dims[1] > theta_dims[1])
      stop('more c values supplied than values of theta')
    else if (c_dims[1] > 1 & c_dims[1] < theta_dims[1])
    {
      cat('nlodm: More than one 1 c vector supplied, but number still does not match number of supplied theta. First c vector only will be used.\n')
      param = matrix(rep(c[1, ], theta_dims[1]), nrow = theta_dims[1], byrow = T)
    }
    else
      param = c

    obj_fun = obj.c
  }
  else if (obj == 'c_e') {
    param = c
    obj_fun = obj.c_e
  }
  else
    stop("Objective not supported")


  # objective function
  obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param, prior_weights,
                              exact, binary_response, dr_fun)

  # set up variable bounds
  if (exact)
    rangeVar = matrix(rep(c(0, bound), pts), nrow = 2)
  else
    rangeVar = matrix(c(rep(c(0, bound), pts), rep(c(0,1), pts)), nrow = 2)

  # algorithm options
  control = list(numPopulation = swarm, maxIter = iter)

  # number of design points
  if (exact)
    numVar = pts
  else
    numVar = 2 * pts

  # find design
  result = metaheuristicOpt::metaOpt(
    obj_fun_M,
    optimType = "MAX",
    algorithm = algorithm,
    numVar = numVar,
    rangeVar,
    control,
    seed = seed
  )

  # process results and return
  if (exact) {

    # merge doses that are the same to exact_digits decimal places
    # table to get counts at each dose
    x = as.data.frame(table(round(result$result, exact_digits)))

    design = list(
      x = as.numeric(levels(x$Var1))[x$Var1],
      n = x$Freq,
      obj_value = result$optimumValue
    )

    return(list(design = design, raw = result))

  }
  else {
    # check optimality
    vars = result$result
    x = vars[1:pts]
    w = vars[(pts+1):(2*pts)]

    # collapse doses if weights are small
    x = x[w > 1e-5]
    w = w[w > 1e-5]
    result$result = c(x, w)


    #M = M.nonlinear(x, w, theta, grad_fun)
    M.list = M.nonlinear.list(x, w, theta, grad_fun, binary_response, dr_fun)

    # add ridge if c_e objective
    if (obj == 'c_e')
      M.list = lapply(M.list, function(M) M + diag(1e-5, nrow(M)))

    # generate equivalence theorem plot
    problem = list(bound = bound, obj = obj, theta = theta, param = param)
    p = plot_sens(x, w, problem, M.list, grad_fun, prior_weights, binary_response, dr_fun)

    # format results for return
    # order and round doses and weights
    design = list(
      x = signif(x[order(x)], 3),
      w = round(w[order(x)], 3),
      obj_value = result$optimumValue
    )

    return(list(design = design, plot = p, raw = result))
  }



}
