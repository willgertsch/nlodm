test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that('uniform_design_eff_D_loglogistic', {

  # uniform design vs D optimal design

  eff = compute_eff(
    model = NULL,
    grad_fun = grad.loglogistic,
    objective = 'D',
    theta = c(0.02461, -2.390, 1),
    d1 = c(0.1, 15, 30),
    d2 = c(0.1, 3.8, 30),
    w1 = c(1/3, 1/3, 1/3),
    w2 = c(1/3, 1/3, 1/3)
  )
  expect_gt(eff, 0.5)
})
