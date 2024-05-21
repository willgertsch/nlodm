# dose response functions

# 4 parameter log-logistic model
f.loglogistic4 = function(x, theta) {

  y = theta[1] + (theta[4] - theta[1])/(1 + exp(theta[2]*(log(x)-log(theta[3]))))
  return(y)
}

# 3 parameter log-logistic model
# BMDS parameterization
f.loglogistic3.bmds = function(x, theta) {
  suppressWarnings(theta[1] + (1-theta[1])/(1+exp(-theta[2]-theta[3]*log(x))))
}

f.weibull3.bmds = function(x, theta) {
  g = theta[1]
  a = theta[2]
  b = theta[3]
  g + (1 - g)*(1 - exp(-b * x^a))
}

f.hill.bmds = function(x, theta) {

  g = theta[1]
  v = theta[2]
  a = theta[3]
  b = theta[4]

  g + (v - v*g)/(1+exp(-a-b*log(x)))
}

f.multi1.bmds = function(x, theta) {
  g = theta[1]
  a = theta[2]

  g + (1 - g)*(1-exp(-a*x))
}

f.logistic.bmds = function(x, theta) {
  a = theta[1]
  b = theta[2]

  1 / (1 + exp(-a - b * x))
}

f.logprobit.bmds = function(x, theta) {
  g = theta[1]
  a = theta[2]
  b = theta[3]

  g + (1 - g) * pnorm(a + b * log(x))
}

f.probit.bmds = function(x, theta) {

  a = theta[1]
  b = theta[2]

  pnorm(a + b * x)
}
