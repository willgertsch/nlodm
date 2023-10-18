# find multi-objective designs
# supports both compound and pareto designs
multi_obj = function(grad_funs, obj_funs, thetas, params, type = 'pareto',
                     weights = NULL, bound, pts,
                     swarm = 50, maxiter = 300, verbose = T) {

  if (type == 'compound') {

  }
  else if (type == 'pareto') {

    # construct multi-objective function
    multi_obj_fun = multi_obj_fun_factory(grad_funs, obj_funs, thetas, params)


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

    if (verbose) {
      result = nsga2R::nsga2R(
        fn = multi_obj_fun,
        varNo = pts * 2,
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
        result = nsga2R::nsga2R(
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


    return(result)

  }
  else
    stop('Design type not supported')

}
