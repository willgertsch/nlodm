# compute the efficiency of design 1 compared to design 2
# model: string name of pre-defined model
# grad_fun: gradient function for custom model
# theta: local parameter values
# objective: D or A currently
# d1: design points for design 1
# d2: design points for design 2
# w1: weights for design 1
# w2: weights for design 2
compute_eff = function(
    model = NULL,
    grad_fun,
    theta,
    objective,
    d1,
    d2,
    w1,
    w2
) {


  # select gradient function
  #grad_fun = grad_selector(model)

  if (objective == "D")
    obj_fun = obj.D
  else if (objective == "A")
    obj_fun = obj.A

  # define objective function
  param = c()
  obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param)

  # compute and return efficiencies
  if (objective == "D")
    (exp(obj_fun_M(c(d1, w1)))/exp(obj_fun_M(c(d2, w2))))^(1/length(theta))
  else if (objective == "A")
    obj_fun_M(c(d2, w2))/obj_fun_M(c(d1, w1))
}
