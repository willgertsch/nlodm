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

  expect_equal(length(out$result$result), 4)
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

  expect_equal(length(out$result$result), 6)
  expect_equal(length(out$plot), 9) # ggplot objects are lists of length 9
})
