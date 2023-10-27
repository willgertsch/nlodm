# D optimality
# maximize logdetM
obj.D = function(M, param) {
  suppressWarnings(log(det(M)))
}

# A optimality
# minimize trM^-1
obj.A = function(M, param) {

  # check if matrix is invertible
  if (!checkMinv(M))
    return(-Inf)
  else
    return(-sum(diag(solve(M))))
}

# BMD optimality
obj.bmd = function(M, param) {

  lambda = param[1]
  c = param[-1]
  if (!checkMinv(M))
    return(-Inf)
  else {
    Minv = solve(M)
    Dval = suppressWarnings(log(det(M)))
    Cval = -suppressWarnings(log(t(c) %*% Minv %*% c)) # note the sign flip
    p = length(c)
    return(lambda * Cval + (1 - lambda)/p * Dval)
  }
}

# c-optimality
# minimize Var(c'theta) = c'M^1c
obj.c = function(M, param) {

  c = param
  if (!checkMinv(M))
    return(-Inf)
  else {
    Minv = solve(M)
    Cval = -suppressWarnings(log(t(c) %*% Minv %*% c)) # note sign flip
    return(Cval)
  }

}
