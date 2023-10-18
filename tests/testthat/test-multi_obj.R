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
    maxiter = 100
  )

  # result
  #
  # # process results
  # library(dplyr)
  # d = result$objectives %>%
  #   as.data.frame() %>%
  #   bind_cols(as.data.frame(result$parameters)) %>%
  #   filter(result$paretoFrontRank == 1) %>%
  #   distinct()
  #
  # colnames(d) = c('logistic A', "log-logistic D", 'x1', 'x2', 'x3',
  #                 'w1', 'w2','w3')
  #
  # # plot Pareto front
  # library(ggplot2)
  # ggplot(d,
  #        aes(x = `logistic A`, y = `log-logistic D`)) +
  #   geom_point(color = "blue", size = 2) +
  #   theme_bw() +
  #   labs(title = "")

})
