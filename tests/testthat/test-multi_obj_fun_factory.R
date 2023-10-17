test_that("multiobjective function", {

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
  test_multi_fun = multi_obj_fun_factory(grad_funs, obj_funs, thetas, params)

  # uniform 3 point design
  x = c(0.1, 15, 30)
  w = c(1/3, 1/3, 1/3)
  vars = c(x, w)

  test_obj_vals = test_multi_fun(vars)

  true_obj_vals = numeric(3)
  obj_fun1 = obj_fun_factory(grad_funs[[1]], obj_funs[[1]], thetas[[1]], params[[1]])
  obj_fun2 = obj_fun_factory(grad_funs[[2]], obj_funs[[2]], thetas[[2]], params[[2]])
  obj_fun3 = obj_fun_factory(grad_funs[[3]], obj_funs[[3]], thetas[[3]], params[[3]])
  true_obj_vals[1] = obj_fun1(vars)
  true_obj_vals[2] = obj_fun2(vars)
  true_obj_vals[3] = obj_fun3(vars)

  # compare
  expect_equal(true_obj_vals, test_obj_vals)

})

test_that('factory works with optimizer', {

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
  test_multi_fun = multi_obj_fun_factory(grad_funs, obj_funs, thetas, params)

  pts = 3
  bound = 30

  res = ecr::ecr(
    fitness.fun = test_multi_fun,
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

})
