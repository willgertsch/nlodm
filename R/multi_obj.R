# find multi-objective designs
# supports both compound and pareto designs
multi_obj = function(models, grad_funs, objs, thetas, type = 'pareto',
                     weights = NULL) {


  if (type == 'compound') {

  }
  else if (type == 'pareto') {

    # construct multi-objective function

  }
  else
    stop('Design type not supported')

}
