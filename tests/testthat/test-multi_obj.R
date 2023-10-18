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

  #
  # # plot Pareto front
  # library(ggplot2)
  # ggplot(d,
  #        aes(x = `logistic A`, y = `log-logistic D`)) +
  #   geom_point(color = "blue", size = 2) +
  #   theme_bw() +
  #   labs(title = "")

})
