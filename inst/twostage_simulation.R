# simulation code using simChef
# copied from desktop

# simulation study
# load my library for finding optimal designs
directory <- "R"
# List all files with the .R extension in the directory
r_files <- list.files(directory, pattern = "\\.R$", full.names = TRUE)
# Source each .R file
for (file in r_files) {
  source(file)
}


library(ggplot2)
library(dplyr)
library(tidyr)
library(drc)
library(ToxicR)
library(simChef)
library(future)
library(metaheuristicOpt)
library(rBayesianOptimization)

dat = drc::deguelin

mod1 = single_dichotomous_fit(
  D = dat$dose,
  Y = dat$r,
  N = dat$n,
  model_type = 'log-logistic',
  fit_type = 'mle',
  BMR = 0.1,
  alpha = 0.025
)
summary(mod1)
mod1$parameters

# use simChef
# need to translate into their framework
# dgp:
# true model should live here
dgp_fun = function(N1 = 100, N = 200) {
  x_init = drc::deguelin$dose
  w_init = rep(1/6, 6)
  max_dose = max(drc::deguelin$dose)
  theta = c(-0.7363599, -12.0933855,   4.1368046)
  G = 10
  return(list(theta = theta, N1 = N1, N = N, x_init = x_init, w_init = w_init, max_dose = max_dose, G = G))
}

# method:
# each method should simulate experiment and generate CI
uniform_twostage_fun = function(theta, N1, N, x_init, w_init, ...) {

  N1 = N1
  N2 = N - N1
  source("R/functions.R")

  # simulate data from stage 1
  stage1_dat = sim_data(N1, theta, x_init, w_init)
  stage2_dat = sim_data(N2, theta, x_init, w_init)

  # combine stage 1 and stage 2 data
  dat = rbind(stage1_dat, stage2_dat)

  # # fit model and get CI
  # CI_length = compute_BMD_CI_length(dat)
  # return(list(CI_length = CI_length, ...))

  return(list(dat = dat))
}

D_2stage_fun = function(theta, N1, N, x_init, w_init, max_dose, ...) {

  N1 = N1
  N2 = N - N1

  # List all files with the .R extension in the directory
  r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  # Source each .R file
  for (file in r_files) {
    source(file)
  }

  # simulate data from stage 1
  stage1_dat = sim_data(N1, theta, x_init, w_init)

  # fit model to stage 1 data
  mod = ToxicR::single_dichotomous_fit(
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

  x = out$design$x
  w = out$design$w

  # simulate data from stage 2
  stage2_dat = sim_data(N2, theta, x, w)

  # combine stage 1 and stage 2 data
  dat = rbind(stage1_dat, stage2_dat)

  return(list(dat = dat))

}

c_2stage_fun = function(theta, N1, N, x_init, w_init, max_dose, ...) {

  N1 = N1
  N2 = N - N1

  # List all files with the .R extension in the directory
  r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  # Source each .R file
  for (file in r_files) {
    source(file)
  }

  # simulate data from stage 1
  stage1_dat = sim_data(N1, theta, x_init, w_init)

  # fit model to stage 1 data
  mod = ToxicR::single_dichotomous_fit(
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

  x = out$design$x
  w = out$design$w

  # simulate data from stage 2
  stage2_dat = sim_data(N2, theta, x, w)

  # combine stage 1 and stage 2 data
  dat = rbind(stage1_dat, stage2_dat)

  return(list(dat = dat))

}

wang_2stage_fun = function(theta, N1, N, x_init, w_init, max_dose, G, ...) {

  N1 = N1
  N2 = N - N1
  source("R/functions.R")

  # simulate data from stage 1
  stage1_dat = sim_data(N1, theta, x_init, w_init)

  # fit model to stage 1 data
  mod = ToxicR::single_dichotomous_fit(
    D = stage1_dat$dose,
    Y = stage1_dat$events,
    N = stage1_dat$n,
    model_type = 'log-logistic',
    fit_type = 'mle',
    BMR = 0.1,
    alpha = 0.025
  )

  # dose grid
  dose_grid = seq(0.001, max_dose, length.out = G)

  # greedy algorithm loops
  # add a design point
  dat_stage12 = stage1_dat
  for (k in 1:N2) {
    # consider each grid point
    #cat('Choosing point', k, 'of', N2, '\n')
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
  # simulate from design
  design = dplyr::count(tail(dat_stage12, N2), dose)
  dat_stage2 = sim_data_exact(theta, design$dose, design$n)
  dat = rbind(stage1_dat, dat_stage2)

  return(list(dat = dat))

}

bayes_twostage_fun = function(theta, N1, N, x_init, w_init, max_dose, ...) {

  source("R/functions.R")

  N1 = N1
  N2 = N - N1

  # simulate data from stage 1
  stage1_dat = sim_data(N1, theta, x_init, w_init)

  # fit model to stage 1 data
  mod = ToxicR::single_dichotomous_fit(
    D = stage1_dat$dose,
    Y = stage1_dat$events,
    N = stage1_dat$n,
    model_type = 'log-logistic',
    fit_type = 'mle',
    BMR = 0.1,
    alpha = 0.025
  )
  theta = mod$parameters

  # define objective function
  force(theta)
  force(N2)
  obj_fun = function(x1, x2, x3, w1, w2, w3) {
    # extract
    x = c(x1, x2, x3)
    w = c(w1, w2, w3)

    # normalize weights
    w = w/sum(w)

    # simulate data
    dat = sim_data(N2, theta, x, w)

    # compute CI
    CI_length = compute_BMD_CI_length(dat)

    if (is.na(CI_length))
      CI_length = max_dose

    return(list(Score = -CI_length, Pred = NULL))
  }

  search_bound = list(
    x1 = c(0.01, max_dose),
    x2 = c(0.01, max_dose),
    x3 = c(0.01, max_dose),
    w1 = c(0, 1),
    w2 = c(0, 1),
    w3 = c(0, 1)
  )

  search_grid = data.frame(
    x1 = runif(5, 0.01, max_dose),
    x2 = runif(5, 0.01, max_dose),
    x3 = runif(5, 0.01, max_dose),
    w1 = rep(1/3, 5),
    w2 = rep(1/3, 5),
    w3 = rep(1/3, 5)
  )

  # getting some errors occasionally
  # after a 7 hour simulation run :/
  # use try and repeat initial design if there is an error
  bayes = try({
    rBayesianOptimization::BayesianOptimization(
      FUN = obj_fun,
      bounds = search_bound,
      init_grid_dt = search_grid,
      init_points = 0,
      n_iter = 10,
      acq = 'ucb',
      verbose = F
    )
  })

  if (inherits(bayes, 'try-error')) {
    message('Bayes opt package error: repeating first stage')
    # simulate data from stage 2
    stage2_dat = sim_data(N2, theta, x_init, w_init)

    dat = rbind(stage1_dat, stage2_dat)

    return(list(dat = dat))
  }
  else {
    design = list(
      x = bayes$Best_Par[1:3],
      w = bayes$Best_Par[4:6]/sum(bayes$Best_Par[4:6]),
      min_CI = -bayes$Best_Value
    )

    # simulate data from stage 2
    stage2_dat = sim_data(N2, theta, design$x, design$w)

    dat = rbind(stage1_dat, stage2_dat)

    return(list(dat = dat))
  }



}

# evaluation functions
compute_CI_length_fun = function(fit_results) {
  fit_results %>%
    dplyr::mutate(
      `CI length` = sapply(dat, compute_BMD_CI_length)
    )
}

# visualizers
visualizer_fun = function(eval_results) {
  eval_results[[1]] %>%
    group_by(N, N1, .method_name) %>%
    summarise(`Median CI length` = median(`CI length`, na.rm = T)) %>%
    ggplot(aes(x = N1, y = `Median CI length`, color = .method_name)) +
    geom_point() + geom_line() +
    facet_wrap(~N)
}

# visualizer_fun_r = function(eval_results) {
#   eval_results[[1]] %>%
#     group_by(N, r, .method_name) %>%
#     summarise(`Median CI length` = median(`CI length`, na.rm = T)) %>%
#     ggplot(aes(x = N, y = `Median CI length`, color = .method_name)) +
#     geom_point() + geom_line() +
#     facet_wrap(~r) +
#     labs(x='Stage 1 sample size')
# }

# convert functions
dgp = create_dgp(.dgp = dgp_fun, .name = 'Deguelin')
uniform_twostage = create_method(.method_fun = uniform_twostage_fun, .name = 'Repeat stage 1')
D_twostage = create_method(.method_fun = D_2stage_fun, .name = 'D-optimal Two Stage')
c_twostage = create_method(.method_fun = c_2stage_fun, .name = 'c-optimal Two Stage')
wang_twostage = create_method(.method_fun = wang_2stage_fun, .name = 'Wang Two Stage')
bayes_twostage = create_method(.method_fun = bayes_twostage_fun, .name = 'Bayesian optimization')
evaluator = create_evaluator(.eval_fun = compute_CI_length_fun, .name = 'CI length')
visualizer = create_visualizer(.viz_fun = visualizer_fun, .name = 'CI length plot')
#visualizer_r = create_visualizer(.viz_fun = visualizer_fun_r, .name = 'CI length plot by r')

# create simulation experiment
experiment = create_experiment(name = 'Two-stage N1') %>%
  add_dgp(dgp) %>%
  add_method(uniform_twostage) %>%
  add_method(D_twostage) %>%
  add_method(c_twostage) %>%
  add_method(wang_twostage) %>%
  add_method(bayes_twostage) %>%
  add_evaluator(evaluator) %>%
  add_visualizer(visualizer) %>%
  #add_visualizer(visualizer_r) %>%
  add_vary_across(
    .dgp = "Deguelin",
    N = 200,
    N1 = seq(10, 190, by = 10)
  )

experiment

# documentation
init_docs(experiment)

# run
plan(multisession, workers = 12)
results = run_experiment(experiment, n_reps = 100, save = T)

# render
render_docs(experiment)
