test_that("basic multi_obj", {

  grad_funs = list(grad.logistic, grad.loglogistic)
  obj_funs = list(obj.A, obj.D)
  thetas = list(
    matrix(c(-1.710, 0.09703), nrow=1),
    matrix(c(0.02461, -2.390, 1), nrow=1)
  )
  params = list(
    c(),
    c()
  )

  result = multi_obj(
    grad_funs = grad_funs,
    obj_funs = obj_funs,
    thetas = thetas,
    params = params,
    type = 'pareto',
    bound = 30,
    pts = 3,
    swarm = 50,
    maxiter = 100,
    verbose = F
  )

  expect_equal(length(result), 16)

  # test extraction function
  pareto_data = extract_nsga2R(result)
  expect_equal(ncol(pareto_data), 8)

  # plot
  p = plot_pareto2d(pareto_data, c("logistic A", "log-logistic D"))
  expect_equal(length(p), 9) # ggplot objects are lists of length 9

})

test_that("multi obj BMD", {

  grad_funs = list(grad.loglogistic, grad.loglogistic)
  obj_funs = list(obj.D, obj.c)
  bmd_grad = get_bmd_grad("Log-logistic", 'added')
  theta = c(0.02461, -2.390, 1)
  thetas = list(
    theta,
    theta
  )
  params = list(
    c(),
    bmd_grad(0.1, theta)
  )

  result = multi_obj(
    grad_funs = grad_funs,
    obj_funs = obj_funs,
    thetas = thetas,
    params = params,
    type = 'pareto',
    bound = 30,
    pts = 3,
    swarm = 50,
    maxiter = 100,
    verbose = F
  )

  pareto_data = extract_nsga2R(result)
  p = plot_pareto2d(pareto_data, c("D", "c"))

  expect_equal(ncol(pareto_data), 8)
  expect_equal(length(p), 9)
})
