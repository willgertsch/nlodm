# functions for two-stage designs
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

  mod = ToxicR::single_dichotomous_fit(
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

# simulate data from two-stage design and return confidence interval length
# BMR fixed at 0.1 and using extra risk for now
# N1: sample size in first stage
# N2: sample size in second stage
# theta: true parameter values to simulate from
# design: how to assign x and w for stage 2
# x_init: doses for stage 1
# w_init: weights for stage 1
# max_dose: maximum dosage allowed in the experiment
sim_2stage = function(N1, N2, theta, design, x_init, w_init, max_dose) {



  # simulate data from stage 1
  stage1_dat = sim_data(N1, theta, x_init, w_init)

  # fit model and get parameter estimates based on stage 1 data
  mod = single_dichotomous_fit(
    D = stage1_dat$dose,
    Y = stage1_dat$events,
    N = stage1_dat$n,
    model_type = 'log-logistic',
    fit_type = 'mle',
    BMR = 0.1,
    alpha = 0.025
  )
  theta_mod = mod$parameters

  # convert from ToxicR parameterization to one used in my design code
  g = 1 / (1 + exp(-theta_mod[1]))
  a = theta_mod[2]
  b = theta_mod[3]
  theta_nlodm = c(g, a, b)

  # find design for stage 2
  if (design == 'D') {
    # find the D-optimal design
    # print('Finding D-optimal design...')
    out = nlodm(
      grad_fun = grad.loglogistic,
      obj = "D",
      theta = theta_nlodm,
      bound = max_dose,
      pts = 3,
      algorithm = 'DE',
      swarm = 100,
      iter = 500,
      seed = NULL,
      binary_response = T,
      dr_fun = f.loglogistic3.bmds
    )

  }
  else if (design == 'c') {
    # find the c-optimal design for estimating the benchmark dose
    #print('Finding c-optimal design...')
    bmd_grad = get_bmd_grad("Log-logistic", 'extra')
    c = matrix(bmd_grad(0.1, theta_nlodm), nrow = 1)
    out = nlodm(
      grad_fun = grad.loglogistic,
      obj = "c",
      theta = theta_nlodm,
      bound = max_dose,
      pts = 3,
      algorithm = 'DE',
      swarm = 100,
      iter = 500,
      seed = NULL,
      binary_response = T,
      dr_fun = f.loglogistic3.bmds,
      c = c
    )

  }
  else if (design == 'stage1') {
    # repeat the same design as in stage 1
  }
  else if (design == 'Wang2013') {
    # use method inspired by Wang et al 2013

  }
  else {
    stop('sim_2stage: design not supported')
  }

  if (design == 'stage1') {
    x = x_init
    w = w_init
  }
  else {
    x = out$design$x
    w = out$design$w
  }


  # simulate data from stage 2
  stage2_dat = sim_data(N2, theta, x, w)

  # combine stage 1 and stage 2 data
  dat = rbind(stage1_dat, stage2_dat)

  # fit model and get CI
  CI_length = compute_BMD_CI_length(dat)

  # return
  return(CI_length)
}

# sample_sizes: vector of N1, sample size for stage 1
# ss_ratio: ratio of N1/N2, 1 implies equal sample sizes
# Nsim: number of simulations for each sample size
# theta: true parameter values to simulate from
# design: design type in stage 2
# max_dose: maximum dosage allowed
# x_init, w_init, initial design for stage 1
sim_2stage_CIs = function(sample_sizes, ss_ratio = 1, Nsim, theta, design, max_dose, x_init, w_init) {

  result = matrix(data = NA, ncol = 4, nrow = length(sample_sizes)*Nsim)
  j = 1
  for (k in 1:length(sample_sizes)) {
    cat('Running simulation for sample size', sample_sizes[k], '\n')
    for (i in 1:Nsim) {
      # simulate data
      #dat = sim_data(sample_sizes[k], theta, x, w)

      # get confidence interval length
      #CI_length = as.numeric(try(suppressWarnings(compute_BMD_CI_length(dat))))

      CI_length = as.numeric(try(sim_2stage(
        N1 = sample_sizes[k],
        N2 = sample_sizes[k] * ss_ratio,
        design = design,
        theta = theta,
        x_init = x_init,
        w_init = w_init,
        max_dose = max_dose
      )))

      # save to result
      result[j, ] = c(sample_sizes[k], sample_sizes[k]*ss_ratio, i, CI_length)
      j = j + 1
    }
  }

  result = as.data.frame(result)
  colnames(result) = c("N1", "N2", "sim", "CI.length")
  result

}



# simulate confidence interval lengths for selected sample sizes
simulate_CIs = function(sample_sizes, Nsim, theta, x, w) {
  #browser()
  result = matrix(data = NA, ncol = 3, nrow = length(sample_sizes)*Nsim)
  j = 1
  for (k in 1:length(sample_sizes)) {
    cat('Running simulation for sample size', sample_sizes[k], '\n')
    for (i in 1:Nsim) {
      # simulate data
      dat = sim_data(sample_sizes[k], theta, x, w)

      # get confidence interval length
      CI_length = as.numeric(try(suppressWarnings(compute_BMD_CI_length(dat))))

      # save to result
      result[j, ] = c(sample_sizes[k], i, CI_length)
      j = j + 1
    }
  }

  result = as.data.frame(result)
  colnames(result) = c("N", "sim", "CI.length")
  result
}

# N: sample size for stage 2
# max_dose: maximum dose value
# G: number of grid points
# dat_stage1: data from first stage of design
wang_stage2 = function(N, max_dose, G, dat_stage1) {

  #browser()
  # fit model
  mod = single_dichotomous_fit(
    D = dat_stage1$dose,
    Y = dat_stage1$events,
    N = dat_stage1$n,
    model_type = 'log-logistic',
    fit_type = 'mle',
    BMR = 0.1,
    alpha = 0.025
  )

  # discretize dose range
  # how to match to stage 1 data?
  # they don't bother with this
  # they just used an evenly spaced grid
  dose_grid = seq(0.001, max_dose, length.out = G)

  # greedy algorithm loops
  # add a design point
  dat_stage12 = dat_stage1
  for (k in 1:N) {
    # consider each grid point
    cat('Choosing point', k, 'of', N, '\n')
    design_criteria = numeric(G)
    ys = numeric(G) # simulated response values
    for (g in 1:G) {

      # simulate response at point
      # should we be doing this multiple times?
      p = predict(mod, new_doses = dose_grid[g])$Y
      ys[g] = rbinom(1, 1, p)
      # add point to dataset
      test_dat = rbind(
        dat_stage12,
        c(dose_grid[g], 1, ys[g], NA, NA)
      )

      # evaluate design criteria
      # bootstrapping in original, but we use profile likelihood
      design_criteria[g] = compute_BMD_CI_length(test_dat)
    }

    # save best grid point
    best_dose = dose_grid[which.min(design_criteria)]
    y = ys[which.min(design_criteria)]
    dat_stage12 = rbind(
      dat_stage12,
      c(best_dose, 1, y, NA, NA)
    )

  }
  # return stage 2 design
  return(count(tail(dat_stage12, N), dose))
}

# generate data from exact design
# theta: model parameters
# x: dose levels
# n: sample size at each dose
sim_data_exact = function(theta, x, n) {

  # construct data

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
