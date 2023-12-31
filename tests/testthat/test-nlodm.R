# tests for main function
test_that('logistic_D', {
  out = nlodm(
    model = NULL,
    grad_fun = grad.logistic,
    obj = 'D',
    theta = c(0,1),
    bound = 10,
    pts = 2,
    algorithm = 'PSO',
    swarm = 20,
    iter = 100,
    seed = 1234
  )
  #out$result
  #out$plot

  expect_equal(length(out$design$x), 2)
  expect_equal(length(out$raw$result), 4)
  expect_equal(length(out$plot), 9) # ggplot objects are lists of length 9
})

test_that('loglogistic_D', {
  out = nlodm(
    model = NULL,
    grad_fun = grad.loglogistic,
    obj = 'D',
    theta = c(0.02461, -2.390, 1),
    bound = 30,
    pts = 3,
    algorithm = 'PSO',
    swarm = 30,
    iter = 400,
    seed = 1234
  )
  #out$result
  #out$plot

  expect_equal(length(out$design$x), 3)
  expect_equal(length(out$raw$result), 6)
  expect_equal(length(out$plot), 9) # ggplot objects are lists of length 9
})

test_that('bmd log-logistic', {
  out = nlodm(
    model = 'Log-logistic',
    grad_fun = grad.loglogistic,
    obj = 'bmd',
    theta = c(0.02461, -2.390, 1),
    bound = 30,
    pts = 3,
    algorithm = 'DE',
    swarm = 50,
    iter = 500,
    seed = 1234,
    bmd_type = 'added',
    risk = 0.1,
    lambda = 0.5
  )
  #out$result
  #out$plot

  expect_equal(length(out$design$x), 3)
  expect_equal(length(out$raw$result), 6)
  expect_equal(length(out$plot), 9) # ggplot objects are lists of length 9
})

test_that('A log-logistic', {
  out = nlodm(
    model = 'Log-logistic',
    grad_fun = grad.loglogistic,
    obj = 'A',
    theta = c(0.02461, -2.390, 1),
    bound = 30,
    pts = 3,
    algorithm = 'DE',
    swarm = 50,
    iter = 500,
    seed = 1234,
    lambda = 0.5
  )
  #out$result
  #out$plot

  expect_equal(length(out$design$x), 3)
  expect_equal(length(out$raw$result), 6)
  expect_equal(length(out$plot), 9) # ggplot objects are lists of length 9
})

test_that('D Weibull', {
  out = nlodm(
    model = 'Weibull',
    grad_fun = grad.weibull,
    obj = 'D',
    theta = c(0.05307, .99, 0.04929),
    bound = 30,
    pts = 3,
    algorithm = 'PSO',
    swarm = 30,
    iter = 300,
    seed = 1234
  )
  #out$result
  #out$plot

  expect_equal(length(out$design$x), 3)
  expect_equal(length(out$raw$result), 6)
  expect_equal(length(out$plot), 9) # ggplot objects are lists of length 9
})
