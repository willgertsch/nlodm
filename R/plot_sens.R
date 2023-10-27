# plot sensitivity function for a given design
# problem is same list from toxODmeta
# x, w are design point and weight vectors
# M: list of pre-computed information matrices
# grad_fun: gradient function
plot_sens = function(x, w, problem, M.list, grad_fun, prior_weights = c(1)) {

  # x values
  step = problem$bound/1000
  xvals = seq(0, problem$bound, step)

  # select derivative function for sensitivity function
  if (problem$obj == "D") {
    dPsi = dPsi.D
    param = NULL
  }
  else if (problem$obj == "A") {
    dPsi = dPsi.A
    param = NULL
  }
  else if (problem$obj == "bmd") {

    dPsi = dPsi.CD
    param = problem$param
  }
  else if (problem$obj == 'c') {
    dPsi = dPsi.c
    param = problem$param
  }
  else {
    # expand this to handle solving design problems with no verification
    stop("No derivative specified for this objective.")
    # use y=2 to denote missing derivative function
    #yvals = rep(2, length(xvals))
  }

  # compute sensitivity function
  # average over prior theta values
  yvals = rep(0, length(xvals))
  p = nrow(problem$theta)
  for (i in 1:p) {
    M_i = M.list[[i]]
    theta_i = problem$theta[i, ]
    # check first if matrix is invertible and then invert
    if (!checkMinv(M_i)) {
      # using y=1 to denote matrix singularity
      yvals = rep(1, length(xvals))
    }
    else {
      #Minv = solve(M_i)
      yvals = yvals +
        sapply(xvals, sens, grad_fun, dPsi, M_i, theta_i, param) * prior_weights[i]
    }
  }




  # plot
  # display message if missing matrix deriv or singular matrix
  if (sum(yvals - 1, na.rm = T) == 0) {
    p = ggplot2::ggplot(mapping = ggplot2::aes(y = yvals, x = xvals)) +
      ggplot2::geom_line(color = "blue") +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Equivalence Theorem Check") +
      ggplot2::xlab("x") +
      ggplot2::ylab("ch(x)") +
      ggplot2::annotate("text", x = mean(xvals), y = 0.5,
                        label = "Singular information matrix, try increasing max design points.", size = 5)
  }
  else if (sum(yvals - 2, na.rm = T) == 0) {
    p = ggplot2::ggplot(mapping = ggplot2::aes(y = yvals, x = xvals)) +
      ggplot2::geom_line(color = "blue") +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Equivalence Theorem Check") +
      ggplot2::xlab("x") +
      ggplot2::ylab("ch(x)") +
      ggplot2::annotate("text", x = mean(xvals), y = 0.5,
                        label = "No dPsi defined", size = 5)
  }
  else {
    p = ggplot2::ggplot(mapping = ggplot2::aes(y = yvals, x = xvals)) +
      ggplot2::geom_line(color = "blue") +
      ggplot2::geom_hline(yintercept = 0) +
      #ggplot2::geom_point(aes(x = design_points, y = pts_ch), col = "red", size = 3) +
      ggplot2::geom_vline(xintercept = x, color = "red", linetype = "dashed") +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Optimality check", subtitle = 'Is the blue line equal to 0 at the selected doses (red lines) and less than 0 everywhere else?') +
      ggplot2::xlab("dose") +
      ggplot2::ylab("directional derivative")
  }


  return(p)
}
