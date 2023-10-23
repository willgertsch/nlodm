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

  expect_equal(length(out$design$x), 2)
  expect_equal(length(out$raw$result), 4)
  expect_equal(length(out$plot), 9) # ggplot objects are lists of length 9

})

test_that('Bayesian D logistic from prior', {

  set.seed(1234)
  N = 20
  theta0 = rnorm(N, -1.710, .3)
  theta1 = rnorm(N, 0.09703, .01)

  # plot
  # step = 30/1000
  # xvals = seq(0, 30, step)
  # plot(1/(1 + exp(-theta0[1] - theta1[1]*xvals)) ~ xvals)
  # for (i in 2:N)
  #   lines(1/(1 + exp(-theta0[i] - theta1[i]*xvals)) ~ xvals)

  thetam = cbind(theta0, theta1)
  prior_weights = rep(1/N, N)

  out = nlodm(
    model = NULL,
    grad_fun = grad.logistic,
    obj = 'D',
    theta = thetam,
    prior_weights = prior_weights,
    bound = 30,
    pts = 4,
    algorithm = 'PSO',
    swarm = 20,
    iter = 100,
    seed = 1234
  )
  #out$plot


  expect_equal(length(out$raw$result), 4)
  expect_equal(length(out$plot), 9) # ggplot objects are lists of length 9

  # compare to non-bayesian and is different
  # out2 = nlodm(
  #   model = NULL,
  #   grad_fun = grad.logistic,
  #   obj = 'D',
  #   theta = thetam[1,],
  #   prior_weights = c(1),
  #   bound = 30,
  #   pts = 4,
  #   algorithm = 'PSO',
  #   swarm = 20,
  #   iter = 100,
  #   seed = 1234
  # )
  # out2$plot


})

test_that('Bayesian D logistic from prior N=100', {

  set.seed(1234)
  N = 100
  theta0 = rnorm(N, -1.710, .3)
  theta1 = rnorm(N, 0.09703, .01)

  # plot
  # step = 30/1000
  # xvals = seq(0, 30, step)
  # plot(1/(1 + exp(-theta0[1] - theta1[1]*xvals)) ~ xvals)
  # for (i in 2:N)
  #   lines(1/(1 + exp(-theta0[i] - theta1[i]*xvals)) ~ xvals)

  thetam = cbind(theta0, theta1)
  prior_weights = rep(1/N, N)

  out = nlodm(
    model = NULL,
    grad_fun = grad.logistic,
    obj = 'D',
    theta = thetam,
    prior_weights = prior_weights,
    bound = 30,
    pts = 4,
    algorithm = 'PSO',
    swarm = 20,
    iter = 100,
    seed = 1234
  )
  #out$plot

  expect_equal(length(out$raw$result), 4)
  expect_equal(length(out$plot), 9) # ggplot objects are lists of length 9



})
