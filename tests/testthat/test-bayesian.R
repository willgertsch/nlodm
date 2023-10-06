test_that("basic bayesian D logistic", {

  thetam = matrix(c(-1.710, 0.09703, -.5, 0.09703), nrow = 2, byrow = 2)
  prior_weights = c(0.5, 0.5)
  out = nlodm(
    model = NULL,
    grad_fun = grad.logistic,
    obj = 'D',
    theta = thetam,
    prior_weights = prior_weights,
    bound = 10,
    pts = 2,
    algorithm = 'PSO',
    swarm = 20,
    iter = 100,
    seed = 1234
  )

  expect_equal(length(out$result$result), 4)
  expect_equal(length(out$plot), 9) # ggplot objects are lists of length 9

})
