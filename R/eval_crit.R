# function for evaluating design criteria
# useful for testing
# x: design points
# w: weights
# grad_fun: gradient function of the model
# obj_fun: design criteria function
# theta: vector of parameter values (not supporting pseudo-Bayes currently)
# param: extra parameters for c-optimal designs etc.
# binary_response: boolean whether or not a binary response
# dr_fun: dose response function to be used with binary response variance adjustment
eval_crit = function(x, w, grad_fun, obj_fun, theta, param, binary_response = F, dr_fun = NULL) {


  if (binary_response & is.null(dr_fun))
    stop('eval_crit: design for a binary response requires a dose response function be specified for the variance adjustment.')

  if (length(x) != length(w))
    stop("eval_crit: x and w are not the same length.")

  if (sum(w) > 1) {
    warning("eval_crit: sum of w is > 1, renormalizing happened")
    S = sum(w)
    w = w/S
  }

  if (sum(x==0)>0)
    warning('eval_crit: some design points are 0, this may cause issues for models with log terms.')

  # if theta is vector, convert to matrix
  if (is.vector(theta))
    theta = matrix(theta, nrow = 1)
  # same with c
  if (is.vector(c))
    c = matrix(c, nrow = 1)

  # construct objective function
  # exact and prior weights at defaults
  obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param,
                              prior_weights = c(1), exact = F,
                              binary_response, dr_fun)

  # return objective function value for the design
  obj_fun_M(c(x, w))

}
