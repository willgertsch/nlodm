test_that("basic multi_obj", {

  grad_funs = list(grad.logistic, grad.loglogistic, grad.weibull)
  obj_funs = list(obj.A, obj.D, obj.D)
  thetas = list(
    matrix(c(-1.710, 0.09703), nrow=1),
    matrix(c(0.02461, -2.390, 1), nrow=1),
    matrix(c(0.05307, .99, 0.04929), nrow=1)
  )
  params = list(
    c(),
    c(),
    c()
  )

  #library(ecr)
  result = multi_obj(
    grad_funs = grad_funs,
    objs = obj_funs,
    thetas = thetas,
    params = params,
    type = 'pareto',
    algorithm = 'NSGA-III',
    bound = 30,
    pts = 3,
    swarm = 50,
    maxiter = 300
  )
})
