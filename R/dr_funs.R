# dose response functions

# 4 parameter log-logistic model
f.loglogistic4 = function(x, theta) {

  y = theta[1] + (theta[4] - theta[1])/(1 + exp(theta[2]*(log(x)-log(theta[3]))))
  return(y)
}

# 3 parameter log-logistic model
# BMDS parameterization
f.loglogistic3.bmds = function(x, theta) {
  theta[1] + (1-theta[1])/(1+exp(-theta[2]-theta[3]*log(x)))
}
