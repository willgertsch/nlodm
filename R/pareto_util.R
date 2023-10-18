# utility functions for pareto fronts

# extract results from nsga2R returned object
# returns a data frame with the pareto front and decision variables
extract_nsga2R = function(nsga2R_out) {

  num_pts = nsga2R_out$parameterDim/2
  num_obj = nsga2R_out$objectiveDim

  d = nsga2R_out$objectives %>%
    as.data.frame() %>%
    bind_cols(as.data.frame(nsga2R_out$parameters)) %>%
    dplyr::filter(nsga2R_out$paretoFrontRank == 1) %>%
    dplyr::distinct()

  colnames(d) = c(
    paste0(rep("obj", num_obj), seq(1:num_obj)),
    paste0(rep("d", num_pts), seq(1:num_pts)),
    paste0(rep("w", num_pts), seq(1:num_pts))
  )

  return(d)

}
