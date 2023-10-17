# find multi-objective designs
# supports both compound and pareto designs
multi_obj = function(grad_funs, objs, thetas, params, type = 'pareto',
                     weights = NULL, algorithm = 'NSGA-III', bound, pts,
                     swarm = 50, maxiter = 300) {

  if (type == 'compound') {

  }
  else if (type == 'pareto') {

    # construct multi-objective function
    multi_obj_fun = multi_obj_fun_factory(grad_funs, obj_funs, thetas, params)


    # call optimizer
    browser()
    result = ecr::ecr(
      fitness.fun = multi_obj_fun,
      minimize = c(F, F, F),
      n.objectives = length(obj_funs),
      n.dim = 2*pts,
      lower = rep(0, 2*pts),
      upper = c(rep(bound, pts), rep(1, pts)),
      representation = 'float',
      mu = 100, # number of individuals in population
      lambda = 1, # create 1 offspring,
      p.recomb = 0.7,
      p.mut = 0.3,
      survival.strategy = 'plus',
      log.pop = T

    )

    return(result)

  }
  else
    stop('Design type not supported')

}
