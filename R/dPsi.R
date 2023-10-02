# derivatives of objective functions with respect to information matrix
# matrix singularity is already checked here
# M: information matrix
dPsi.D = function(M, param) {
  Minv = solve(M)
  return(Minv)
}

dPsi.A = function(M, param) {
  Minv = solve(M)
  Minv2 = Minv %*% Minv
  return(Minv2)
}

# compound D and c
# see Atkinson book p389
dPsi.CD = function(M, param) {

  Minv = solve(M)
  p = nrow(M)
  lambda = param[1]
  c = param[-1]
  num = Minv %*% c %*% t(c) %*% Minv
  denom = c(t(c) %*% Minv %*% c)

  return((1 - lambda)/p * Minv + lambda/denom * num)
}
