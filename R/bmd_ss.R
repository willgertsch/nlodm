# functions for BMD sample size computation

# N: total sample size
# theta: model parameters
# x: dose levels
# w: sample size allocations
sim_data = function(N, theta, x, w) {

  # construct data
  n = ceiling(N*w)

  # dose response function
  # parameterization with ED50 as a parameter
  # b = theta[1]
  # c = theta[2]
  # e = theta[3]
  #p = c + (1-c)/(1+exp(b*(log(x)-log(e))))

  # BMDS/ToxicR parameterization
  # see https://github.com/NIEHS/ToxicR/blob/main/R/dicho_functions.R
  g = 1 / (1 + exp(-theta[1]))
  a = theta[2]
  b = theta[3]
  p = g + (1 - g) * (1 / (1 + exp(-a - b * log(x))))

  # sample
  events = rbinom(length(n), n, p)

  data.frame(
    dose = x,
    n = n,
    events = events,
    p = p,
    phat = events/n
  )

}

# compute length of BMD interval from simulated data
# for the drc/bmd package, this is the delta method interval
# for the ToxicR package, this is a profile likelihood interval
compute_BMD_CI_length = function(dat) {
  # using drc and bmd package
  # mod = drm(events/n ~ dose, weights = n, data = dat, fct = LL.3u(), type = 'binomial')
  # bmd_out = bmd(mod, bmr=0.1, backg=coef(mod)[2], display = F)
  # bmd_CI_length = 2*(bmd_out[1] - bmd_out[2])

  mod = single_dichotomous_fit(
    D = dat$dose,
    Y = dat$events,
    N = dat$n,
    model_type = 'log-logistic',
    fit_type = 'mle',
    BMR = 0.1,
    alpha = 0.025
  )

  bmd_CI_length = as.numeric(mod$bmd[3] - mod$bmd[2])
  bmd_CI_length
}
