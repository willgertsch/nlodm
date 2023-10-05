bmdgrad.logistic.add = function(r, theta) {

  beta0 = theta[1]
  beta1 = theta[2]

  g1 = r/(beta1 * (exp(beta0) + r))
  g2 = -log(-(exp(beta0) * (r - 1))/(exp(beta0) +r)) / beta1^2
  return(c(g1, g2))
}

bmdgrad.logistic.extra = function(r, theta) {

  beta0 = theta[1]
  beta1 = theta[2]

  g1 = (exp(beta0) + 1)*r*(exp(beta0)*(r+1)+r-1)/(beta1*(exp(beta0)*r+r-1)*(exp(beta0)*(r+1)+r))
  g2 = - log(- (exp(beta0)*(exp(beta0)*r+r-1))/(exp(beta0)*(r+1)+r))/beta1^2
  return(c(g1, g2))

}

bmdgrad.weibull.add = function(r, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]

  g1 = r * (-log((g+r-1)/(g-1))/b)^(1/(a-1)) / (a*b*(g-1)*(g+r-1))
  g2 = -(log(-(log((g+r-1)/(g-1)))/b) * (-(log((g+r-1)/(g-1)))/b)^(1/a))/(a^2)
  g3 = -(-log((g+r-1)/(g-1))/b)^(1/a)/(a * b)
  return(c(g1, g2, g3))
}

bmdgrad.weibull.extra = function(r, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]

  g1 = 0
  g2 = - log(-log(1-r)/b)*(-log(1-r)/b)^(1/a) / a^2
  g3 = - (-log(1-r)/b)^(1/a) / (a*b)
  return(c(g1, g2, g3))
}

bmdgrad.loglogistic.add = function(r, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]

  g1 = exp(-a/b)*(-r/(g+r-1))^(1/(b+1)) / (b*r)
  g2 = -exp(-a/b)*(-r/(g+r-1))^(1/b) / b
  g3 = exp(-a/b)*(-r/(g+r-1))^(1/b) * (a - suppressWarnings(log(-r/(g+r-1)))) / b^2
  return(c(g1, g2, g3))
}

bmdgrad.loglogistic.extra = function(r, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]

  g1 = 0
  g2 = -exp((log(r/(1-r))-a)/b)/b
  g3 = exp(-a/b) * (r/(1-r))^(1/b) * (a - log(r/(1-r))) / b^2
  return(c(g1,g2,g3))
}

bmdgrad.hill.add = function(r, theta) {

  g = theta[1]
  v = theta[2]
  a = theta[3]
  b = theta[4]

  t1 = exp(-a/b)
  t2 = ((g*r*v-g*v-r+v)/(r-g*r*v))^(-1/b)
  t3 = r*(g*v-1)-g*v+v

  g1 = (v-1)*v*t1*t2/(b*(g*v-1)*t3)
  g2 = -(g-1)*t1*t2/(b*(g*v-1)*t3)
  g3 = -t1*t2/b
  g4 = -(-a-log((g*r*v-g*v-r+v)/(r*(1-g*v))))*(exp((-a-log((g*r*v-g*v-r+v)/(r-g*r*v)))/(b)))/(b^2)
  return(c(g1, g2, g3, g4))

}

bmdgrad.hill.extra = function(r, theta) {

  g = theta[1]
  v = theta[2]
  a = theta[3]
  b = theta[4]

  t1 = exp(-a/b)
  t2 = (-(g*v+r-v)/(r))^(1/b)
  t3 = b*((g-1)*v+r)

  g1 = v*t1*t2/t3
  g2 = (g-1)*t1*t2/t3
  g3 = supressWarnings(-exp((log(-(g*v+r-v)/r)-a)/b)/b)
  g4 = t1*t2 * (a - log(-(g*v+r-v)/(r))) / (b^2)
  return(c(g1,g2,g3,g4))

}
