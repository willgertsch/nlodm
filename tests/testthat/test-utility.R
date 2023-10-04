test_that("get_grad", {
  expect_type(get_grad('Logistic', grad.logistic), 'closure')
  expect_type(get_grad(NULL, grad.logistic), 'closure')
  expect_type(get_grad('Logistic', NULL), 'closure')
  expect_error(get_grad(NULL, NULL))
})
