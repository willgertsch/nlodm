# utility functions

# checks if information matrix is invertible
# returns 1 if invertible and 0 if not
# can optimize easily for 2 dim
checkMinv = function(M) {

  if (class(try(solve(M),silent=T))[1]!="matrix")
    return(0)
  else
    return(1)
}

# model selector
# returns gradient function
grad_selector = function(model) {

  if (model == "Logistic")
    grad_fun = grad.logistic
  else if (model == "Logistic quadratic")
    grad_fun = grad.logistic.quad
  else if (model == "Logistic cubic")
    grad_fun = grad.logistic.cubic
  else if (model == "Logistic fractional polynomial")
    grad_fun = grad.logistic.fp
  else if (model == "Weibull")
    grad_fun = grad.weibull
  else if (model == "Log-logistic")
    grad_fun = grad.loglogistic
  else if (model == "Mixture multistage")
    grad_fun = grad.mix2
  else if (model == "Box-Cox Weibull")
    grad_fun = grad.boxcoxweibull
  else if (model == "Hill")
    grad_fun = grad.hill
  else if (model == "Multistage 1")
    grad_fun = grad.multi1
  else if (model == "Multistage 2")
    grad_fun = grad.multi2
  else if (model == "Multistage 3")
    grad_fun = grad.multi3
  else if (model == "Probit")
    grad_fun = grad.probit
  else if (model == "Log-probit")
    grad_fun = grad.logprobit
  else if (model == "4 parameter log-logistic")
    grad_fun = grad.loglogistic4
  else
    stop('Model not supported')

  return(grad_fun)
}

# input a string name of a design criterion and get an objective function
get_obj = function(obj) {

  if (obj == 'D')
    return(obj.D)
  else if (obj == 'A')
    return(obj.A)
  else if (obj == 'c')
    return(obj.c)
  else if (obj == 'c_e')
    return(obj.c_e)
  else if (obj == 'bmd')
    return(obj.bmd)
  else
    stop('get_obj: Objective not defined.')
}


# input switching for using predefined or custom model
# will always default to grad_fun if supplied
get_grad = function(model, grad_fun) {

  if (!is.null(grad_fun) & is.function(grad_fun))
    return(grad_fun)
  else if (!is.null(model))
    return(grad_selector(model))
  else
    stop('gradient not defined')
}

# select bmd gradient function
get_bmd_grad = function(model, risk_type) {

  if (is.null(model)) {
    stop("Specify a model")
  }
  if (model == "Logistic") {
    grad_fun = grad.logistic

    if (risk_type == "added") {
      bmd_grad = bmdgrad.logistic.add
    }
    else if (risk_type == "extra")
      bmd_grad = bmdgrad.logistic.extra
  }
  else if (model == "Weibull") {
    grad_fun = grad.weibull

    if (risk_type == "added") {
      bmd_grad = bmdgrad.weibull.add
    }
    else if (risk_type == "extra") {
      bmd_grad = bmdgrad.weibull.extra
    }
  }
  else if (model == "Log-logistic") {
    grad_fun = grad.loglogistic

    if (risk_type == "added") {
      bmd_grad = bmdgrad.loglogistic.add
    }
    else if (risk_type == "extra") {
      bmd_grad = bmdgrad.loglogistic.extra
    }
  }
  else if (model == "Hill") {
    grad_fun = grad.hill

    if (risk_type == "added") {
      bmd_grad = bmdgrad.hill.add
    }
    else if (risk_type == "extra") {
      bmd_grad = bmdgrad.hill.extra
    }
  }

  return(bmd_grad)
}

# enhanced solve() function that adds 1E-5 to diagonal if matrix is singular
ridge_solve = function(M) {

  if (checkMinv(M))
    return(M)
  else {
    Mridge = M + diag(1e-5, nrow(M))
    return(solve(Mridge))
  }
}

# convert raw text input to a vector of parameter values
# pulling this out into its own function because input checking could be complex
# useful in multiple places where there is text input
process_theta = function(text) {
  as.numeric(strsplit(text, ",")[[1]])
}

# function that displays latex formulas for models in app
model_display = function(model) {

  if (model == "Hill")
    "$$ P(d) = \\theta_1 + \\frac{(\\theta_2 - \\theta_2 \\theta_1)}{1 + \\exp(-\\theta_3 - \\theta_4\\log (d))} $$"
  # else if (model == "Gamma") # don't know how to do this => Elvis' paper reparameterizes
  #   "$$ P(d) = $$"
  else if (model == "Logistic")
    "$$ P(d) = \\frac{1}{1 + \\exp(-\\theta_1 - \\theta_2 d)} $$"
  else if (model == "Logistic quadratic")
    "$$P(d) = \\frac{1}{1 + \\exp(-\\theta_1 - \\theta_2 d - \\theta_3 d^2)}$$"
  else if (model == "Logistic cubic")
    "$$P(d) = \\frac{1}{1 + \\exp(-\\theta_1 - \\theta_2 d - \\theta_3 d^2 - \\theta_4 d^3)}$$"
  else if (model == "Log-logistic")
    "$$ P(d) = \\theta_1 +  \\frac{1-\\theta_1}{1 + \\exp(-\\theta_2- \\theta_3 \\log d)}$$"
  else if (model == "Log-probit")
    "$$ P(d) = \\theta_1 + (1 - \\theta_1) \\Phi(\\theta_2 + \\theta_3 \\log(d))$$"
  else if (model == "Probit")
    "$$P(d)=\\Phi(\\theta_1 + \\theta_2 d)$$"
  else if (model == "Quantal linear")
    "$$P(d) = \\theta_1 + (1-\\theta_1)(1-\\exp(-\\theta_2 d))$$"
  else if (model == "Weibull")
    "$$P(d) = \\theta_1 + (1-\\theta_1)(1-\\exp(-\\theta_3 d^{\\theta_2}))$$"
  else if (model == "Multistage 1")
    "$$P(d) = \\theta_1 + (1-\\theta_1)(1 - \\exp(-\\theta_2 d))$$"
  else if (model == "Multistage 2")
    "$$P(d) = \\theta_1 + (1-\\theta_1)(1 - \\exp(-\\theta_2 d - \\theta_3 d^2))$$"
  else if (model == "Multistage 3")
    "$$P(d) = \\theta_1 + (1-\\theta_1)(1 - \\exp(-\\theta_2 d - \\theta_3 d^2 - \\theta_4 d^3))$$"
  else if (model == "Probit")
    "$$P(d) = \\phi(\\theta_1 + \\theta_2 d)$$"
  else if (model == "Log-probit")
    "$$P(d) = \\theta_1 + (1-\\theta_1) \\phi(\\theta_2 + \\theta_3 * \\log(d))$$"
  else if (model == "4 parameter log-logistic")
    "$$P(d) = \\theta_1 + \\frac{(\\theta_4 - \\theta_1)}{1 + \\exp(\\theta_2(\\log(d)-\\log(\\theta_3)))}$$"
  else
    "Model not supported"

}

# displays example local theta values
# model: string from model selector
# returns: string to be displayed by Shiny ui element
display_example_param = function(model) {

  if (model == "Hill")
    "EX: \\(\\theta\\) = (0.02201, 0.9034, -2.132, 1)"
  else if (model == "Logistic")
    "EX: \\(\\theta\\) = (-1.710, 0.09703)"
  else if (model == "Logistic quadratic")
    "EX: \\(\\theta\\) = (-2.52, 0.26, -0.006)"
  else if (model == "Logistic cubic")
    "EX: \\(\\theta\\) = (-3.91, 1.56, -0.18, 0.004)"
  else if (model == "Logistic fractional polynomial") {
    "EX: \\(\\theta\\) = ()"
  }
  else if (model == "Log-logistic")
    "EX: \\(\\theta\\) = (0.02461, -2.390, 1)"
  else if (model == "Log-probit")
    "EX: \\(\\theta\\) = (0.02051, -1.237, 0.5173)"
  else if (model == "Probit")
    "EX: \\(\\theta\\) = (-1.051, 0.05948)"
  else if (model == "Quantal linear")
    "EX: \\(\\theta\\) = (0.05307, 0.04929)"
  else if (model == "Weibull")
    "EX: \\(\\theta\\) = (0.05307, .99, 0.04929)"
  else if (model == "Multistage 1")
    "EX: \\(\\theta\\) = (0.05307, 0.04929)"
  else if (model == "Multistage 2")
    "EX: \\(\\theta\\) = (0.05307, 0.04929, 0)"
  else if (model == "Multistage 3")
    "EX: \\(\\theta\\) = (0.05307, 0.04929, 0, 0)"
  else if (model == '4 parameter log-logistic')
    'EX: \\(\\theta\\) = (0.084950, -11.093067 , 12.157339, 0.913343)'
  else
    "EX: \\(\\theta\\) = "


}

# plotting function for dose response model
# model: string name of dose response model
# theta: vector of model parameter values
# limit: dose limit, will control how much of the dose response function is shown
# log_dose: if true transform the x-axis
# returns: a ggplot of the dose response function
plot_response = function(model, theta, limit, log_dose = F) {

  # generate dose levels
  x = seq(0.01, limit, length.out=100)

  # add additional resolution close to 0 to help with log transform
  x = c(x, seq(.001, .1, length.out=20))

  # compute response using appropriate model function
  if (model == "Logistic")
    y = 1/(1 + exp(-theta[1] - theta[2]*x))
  else if (model == "Log-logistic")
    y = theta[1] + (1-theta[1])/(1+exp(-theta[2]-theta[3]*log(x)))
  else if (model == "Weibull")
    y = theta[1] + (1 - theta[1])*(1 - exp(-theta[3]*x^theta[2]))
  else if (model == "Multistage 1")
    y = theta[1] + (1-theta[1])*(1-exp(-theta[2]*x))
  else if (model == "Multistage 2")
    y = theta[1] + (1-theta[1])*(1-exp(-theta[2]*x - theta[3]*x^2))
  else if (model == "Multistage 3")
    y = theta[1] + (1-theta[1])*(1-exp(-theta[2]*x - theta[3]*x^2 - theta[4]*x^3))
  else if (model == "Hill")
    y = theta[1] + (theta[2]-theta[2]*theta[1])/(1+exp(-theta[3]-theta[4]*log(x)))
  else if (model == "Logistic quadratic")
    y = 1/(1 + exp(-theta[1] - theta[2]*x - theta[3]*x^2))
  else if (model == "Logistic cubic")
    y = 1/(1 + exp(-theta[1] - theta[2]*x - theta[3]*x^2 - theta[4]*x^3))
  else if (model == "Logistic fractional polynomial") {
    powers = c(0, theta[4], theta[5])
    eta = theta[1] + theta[2]*H(2, x, powers) + theta[4]*H(3, x, powers)
    y = 1/(1+exp(-eta))
  }
  else if (model == "Mixture multistage")
    y = theta[6]*(1-exp(-theta[1]-theta[2]*x-theta[3]*x^2)) + (1-theta[6])*(1-exp(-theta[1]-theta[4]*x - theta[5]*x^2))
  else if (model == "Probit")
    y = pnorm(theta[1] + theta[2]*x)
  else if (model == "Log-probit")
    y = theta[1] + (1-theta[1])*pnorm(theta[2] + theta[3]*log(x))
  else if (model == "4 parameter log-logistic")
    y = theta[1] + (theta[4] - theta[1])/(1 + exp(theta[2]*(log(x)-log(theta[3]))))
  else
    y = x

  # plot
  # scaling dose
  if (log_dose) {
    xlabel = "log dose"
    dose = log(x)
  }
  else {
    xlabel = 'dose'
    dose = x
  }

  p = ggplot2::ggplot(mapping = ggplot2::aes(y = y, x = dose)) +
    ggplot2::geom_line(color = "red") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Dose response") +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab("P(dose)")


  return(p)
}
