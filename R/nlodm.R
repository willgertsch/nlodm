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
  lambda = 0.5
  ) {

  # get gradient function
  grad_fun = get_grad(model, grad_fun)

  # input checking
  # design objective
  if (obj == "D") {
    obj_fun = obj.D
    param = c()
  }
  else if (obj == "A") {
    obj_fun = obj.A
    param = c()
  }
  else if (obj == 'bmd') {
    bmd_grad = get_bmd_grad(model, bmd_type)
    obj_fun = obj.bmd
    c = bmd_grad(risk, theta)
    param = c(lambda, c)
  }
  else
    stop("Objective not supported")

  # objective function
  obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param, prior_weights)

  # set up variable bounds
  rangeVar = matrix(c(rep(c(0, bound), pts), rep(c(0,1), pts)), nrow = 2)

  # algorithm options
  control = list(numPopulation = swarm, maxIter = iter)

  # find design
  result = metaheuristicOpt::metaOpt(
    obj_fun_M,
    optimType = "MAX",
    algorithm = algorithm,
    numVar = 2 * pts,
    rangeVar,
    control,
    seed = seed
  )

  # check optimality
  vars = result$result
  x = vars[1:pts]
  w = vars[(pts+1):(2*pts)]

  # collapse doses if weights are small
  x = x[w > 1e-5]
  w = w[w > 1e-5]
  result$result = c(x, w)

  M = M.nonlinear(x, w, theta, grad_fun)
  problem = list(bound = bound, obj = obj, theta = theta, param = param)
  p = plot_sens(x, w, problem, M, grad_fun)

  return(list(result = result, plot = p))

}
