test_that("get_grad", {
  expect_type(get_grad('Logistic', grad.logistic), 'closure')
  expect_type(get_grad(NULL, grad.logistic), 'closure')
  expect_type(get_grad('Logistic', NULL), 'closure')
  expect_error(get_grad(NULL, NULL))
})

test_that('ridge_solve', {

  M = matrix(c(1,0,0,1,0,0,0,0,1), nrow = 3)
  Minv = ridge_solve(M)
  expect_equal(checkMinv(Minv), 1)
})
