# utility functions for pareto fronts

# extract results from nsga2R returned object
# returns a data frame with the pareto front and decision variables
extract_front = function(nsga2R_out, exact=F) {

  if (exact) {
    num_pts = nsga2R_out$parameterDim
  }
  else {
    num_pts = nsga2R_out$parameterDim/2
  }

  num_obj = nsga2R_out$objectiveDim

  d = cbind(as.data.frame(nsga2R_out$objectives),
            as.data.frame(nsga2R_out$parameters))


  if (exact) {
    colnames(d) = c(
      paste0(rep("obj", num_obj), seq(1:num_obj)),
      paste0(rep("d", num_pts), seq(1:num_pts))
    )
  }
  else {
    colnames(d) = c(
      paste0(rep("obj", num_obj), seq(1:num_obj)),
      paste0(rep("d", num_pts), seq(1:num_pts)),
      paste0(rep("w", num_pts), seq(1:num_pts))
    )
  }


  d = d %>% dplyr::filter(nsga2R_out$paretoFrontRank == 1) %>%
    dplyr::distinct()



  return(d)

}

# plot pareto front for two objectives
# pareto_data: output from extract_nsga2R
# obj_names: vector of strings to label x and y axis
plot_pareto2d = function(pareto_data, obj_names) {

  ggplot2::ggplot(pareto_data,
                  ggplot2::aes(x = obj1, y = obj2)) +
    ggplot2::geom_point(color = "blue", size = 2) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Pareto front", x = obj_names[1], y = obj_names[2]) +
    ggplot2::geom_smooth(se = F, linetype = 2, color = 'grey')
}

# scaling function
# assuming a minimization problem
scale_obj = function(obj) {

  # deal with infinite values
  obj[which(obj == Inf)] = NA

  (max(obj, na.rm = T)-obj)/(max(obj, na.rm = T)-min(obj, na.rm = T))
}
