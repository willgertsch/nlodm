grad.logistic = function(x, theta) {

  eta = theta[1] + theta[2] * x
  sigma = exp(eta)/(1 + exp(eta))^2
  grad = sigma * c(1, x)
  return(grad)
}

grad.logistic.quad = function(x, theta) {

  eta = theta[1] + theta[2] * x + theta[3] * x^2
  sigma = exp(eta)/(1 + exp(eta))^2
  grad = sigma * c(1, x, x^2)
  return(grad)
}

grad.logistic.cubic = function(x, theta) {
  eta = theta[1] + theta[2] * x + theta[3] * x^2 + theta[4] * x^3
  sigma = exp(eta)/(1 + exp(eta))^2
  grad = sigma * c(1, x, x^2, x^3)
  return(grad)
}

# 2nd degree fractional polynomial predictor
grad.logistic.fp = function(x, theta) {

  # theta4 and theta5 are power paramters in this model
  powers = c(0, theta[4], theta[5])

  # x1 is the 2nd term in the polynomial
  x1 = H(2, x, powers)
  x2 = H(3, x, powers)
  eta = theta[1] + theta[2] * x1 + theta[3] * x2
  sigma = exp(eta)/(1 + exp(eta))^2
  grad = sigma * c(1, x1, x2)
  return(grad)

}

# mixture of two multistage models
# Razzaghi (2002) in Envirometrics
grad.mix2 = function(x, theta) {

  # identify parameters
  a = theta[1]
  b = theta[2]
  c = theta[3]
  d = theta[4]
  f = theta[5]
  g = theta[6]

  # gradient components
  d1 = g*exp(-a-b*x-c*x^2) + (1-g)*exp(-a-d*x-f*x^2)
  d2 = g*x*exp(-a-x*(b+c*x))
  d3 = g * x^2 * exp(-a-x*(b+c*x))
  d4 = (1 - g)*x*exp(-a-x*(d+f*x))
  d5 = (1 - g) *x^2 * exp(-a-x*(d+f*x))
  d6 = exp(-a-d*x-f*x^2) - exp(-a-b*x-c*x^2)
  grad = c(d1, d2, d3, d4, d5, d6)
  return(grad)
}

# Box-Cox Weibull model from Backhaus et al (2000)
# P(x) = 1 - exp( -exp(theta1 + theta2 * (x^theta3 - 1)/theta3) )
grad.boxcoxweibull = function(x, theta) {

  # identify parameters
  a = theta[1]
  b = theta[2]
  c = theta[3]

  # gradient components
  d1 = exp(-exp(a + b*(x^c-1)/c) + a + b*(x^c-1)/c)
  d2 = (x^c - 1)*exp(-exp(a + b*(x^c-1)/c) + a + b*(x^c-1)/c)/c
  d3 = (b*x^c * log(x)/c - b*(x^c-1)/c^2) * exp(-exp(a + b*(x^c-1)/c) + a + b*(x^c-1)/c)
  grad = c(d1, d2, d3)
  return(grad)

}

# Weibull model
# using version found in BMDS
# P[dose] = g + (1 - g) * (1 - exp(-b * dose^a))
grad.weibull = function(x, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]

  g1 = exp(-b * x^a)
  g2 = -b * (g - 1) * x^a * log(x) * exp(-b * x^a)
  g3 = (g - 1) * x^a * (-exp(-b * x^a))
  return(c(g1, g2, g3))
}

# log-logistic model
# using version from BMDS
# P[dose] = g + (1 - g)/(1 + exp(-a - b * Log(dose)))
grad.loglogistic = function(x, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]

  g1 = 1/(exp(a) * x^b + 1)
  g2 = (1-g)*exp(-a-b*log(x))/(exp(-a-b*log(x)) + 1)^2
  g3 = (1-g)*log(x)*exp(-a-b*log(x))/(exp(-a-b*log(x))+1)^2
  return(c(g1, g2, g3))
}

# Hill model
# P(d) = g + (v - v*g) / (1 + exp(-a-b*log(d)))
grad.hill = function(x, theta) {

  g = theta[1]
  v = theta[2]
  a = theta[3]
  b = theta[4]

  g1 = 1 - exp(a) * v * x^b / (exp(a) * x^b + 1)
  g2 = - exp(a) * (g-1) * x^b / (exp(a)*x^b + 1)
  g3 = - exp(a) * (g-1) * v * x^b / (exp(a) * x^b + 1)^2
  g4 = - exp(a)*(g-1)*v*x^b*log(x) / (exp(a) * x^b + 1)^2
  return(c(g1, g2, g3, g4))

}

# multistage 1
# P(d) = g + (1-g)*(1-exp(-a*d))
grad.multi1 = function(x, theta) {

  g = theta[1]
  a = theta[2]

  g1 = exp(-a * x)
  g2 = -(g-1)*x*exp(-a*x)
  return(c(g1, g2))
}

# multistage 2
# P(d) = g + (1-g)*(1-exp(-a*d - b*d^2))
grad.multi2 = function(x, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]

  g1 = exp(-a*x - b*x^2)
  g2 = (1-g)*x*exp(-a*x - b*x^2)
  g3 = (1-g)*x^2*exp(-a*x - b*x^2)
  return(c(g1, g2, g3))
}

# multistage 3
# P(d) = g + (1-g)*(1-exp(-a*d - b*d^2 - c*d^3))
grad.multi3 = function(x, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]
  c = theta[4]

  g1 = exp(-a*x - b*x^2 - c*x^3)
  g2 = x*(1-g)*exp(-a*x - b*x^2 - c*x^3)
  g3 = x^2*(1-g)*exp(-a*x - b*x^2 - c*x^3)
  g4 = x^3*(1-g)*exp(-a*x - b*x^2 - c*x^3)
  return(c(g1, g2, g3, g4))
}

# probit
# P(d) = phi(a + bx)
grad.probit = function(x, theta) {

  a = theta[1]
  b = theta[2]

  g1 = 1/sqrt(2*pi)*exp(-(a+b*x)^2/2)
  g2 = 1/sqrt(2*pi)*exp(-(a+b*x)^2/2) * x
  return(c(g1, g2))
}

# log probit
# P(d) = g + (1-g) * phi(a + b*log(d))
grad.logprobit = function(x, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]

  g1 = 1 - pnorm(a + b * log(x))
  g2 = (1-g) * 1/sqrt(2*pi)*exp(-(a+b*log(x))^2/2)
  g3 = (1-g) * 1/sqrt(2*pi)*exp(-(a+b*log(x))^2/2) * log(x)
  return(c(g1, g2, g3))
}

# 4 parameter logistic
# P(x) = g + (c - g)/(1 + exp(-a - b * log(x)))
grad.loglogistic4 = function(x, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]
  c = theta[4]

  g1 = 1 / (exp(a) * x^b + 1)
  g2 = (c - g) * exp(-a-b*log(x)) / (exp(-a-b*log(x)) + 1)^2
  g3 = (c - g) * exp(-a-b*log(x)) * log(x) / (exp(-a-b*log(x)) + 1)^2
  g4 = 1 / (exp(-a) * x^(-b) + 1)
  return(c(g1, g2, g3, g4))
}
