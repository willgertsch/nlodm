# find multi-objective designs
# supports both compound and pareto designs
# grad_funs: list of gradient functions
# obj_funs: list of design criteria
# thetas: list of model parameter vectors
# params: list of param vectors
# binary_response: boolean vector telling where a binary response should be assumed
# dr_funs: list of dose response functions
multi_obj = function(grad_funs, obj_funs, thetas, params, type = 'pareto',
                     weights = NULL, bound, pts,
                     swarm = 50, maxiter = 300, verbose = T, exact = F,
                     binary_responses, dr_funs = NULL) {
#browser()
  # if (binary_responses & is.null(dr_fun))
  #   stop('multi_obj: design for a binary response requires a dose response function be specified for the variance adjustment.')

  if (type == 'compound') {

    return(NULL)
  }
  else if (type == 'pareto') {

    # construct multi-objective function
    multi_obj_fun = multi_obj_fun_factory(grad_funs, obj_funs, thetas, params,
                                          binary_responses, dr_funs)


    # call optimizer
    #browser()
    # result = ecr::ecr(
    #   fitness.fun = multi_obj_fun,
    #   minimize = c(F, F, F),
    #   n.objectives = length(obj_funs),
    #   n.dim = 2*pts,
    #   lower = rep(0, 2*pts),
    #   upper = c(rep(bound, pts), rep(1, pts)),
    #   representation = 'float',
    #   mu = 100, # number of individuals in population
    #   lambda = 1, # create 1 offspring,
    #   p.recomb = 0.7,
    #   p.mut = 0.3,
    #   survival.strategy = 'plus',
    #   log.pop = T
    #
    # )

    # change bounds and number of points if exact design
    if (exact) {
      varNo = pts
      lowerBounds = rep(0, pts)
      upperBounds = rep(bound, pts)
    }
    else {
      varNo = pts * 2
      lowerBounds = rep(0, 2*pts)
      upperBounds = c(rep(bound, pts), rep(1, pts))
    }

    if (verbose) {
      result = nsga2R::nsga2R(
        fn = multi_obj_fun,
        varNo = varNo,
        objDim = length(obj_funs),
        lowerBounds = rep(0, 2*pts),
        upperBounds = c(rep(bound, pts), rep(1, pts)),
        generations = maxiter,
        mprob = 0.2,
        popSize = swarm,
        cprob = 0.8
      )
    }
    else {
      # suppress output
      capture.output(
        result <- nsga2R::nsga2R(
          fn = multi_obj_fun,
          varNo = pts * 2,
          objDim = length(obj_funs),
          lowerBounds = rep(0, 2*pts),
          upperBounds = c(rep(bound, pts), rep(1, pts)),
          generations = maxiter,
          mprob = 0.2,
          popSize = swarm,
          cprob = 0.8
        ),
        file = nullfile()
      )
    }
  }
  else
    stop('Design type not supported')

  return(result)

}
