#' Package-specific location for holding the cluster reference
#'
#' @return name of variable holding a cluster reference
#'
#' @export
default_cluster_name = function() ".defaultCluster"

#' Automatically retrieve *some* cluster that is running
#'
#' @param cl object maybe holding a cluster
#' @param variable name of variable holding cluster 
#' @param env where the object holding the cluster (reference) will live
#' @return a running cluster from parallel package
#'
#' @export
get_cluster = function(cl = NULL, variable = default_cluster_name(), env = .GlobalEnv, ...) {
  if (is.null(cl) || !isTRUE('cluster' %in% class(cl))) {
    cl = rlang::env_get(env = env, nm = variable, default = NULL)
  }
  have_cluster = isTRUE('cluster' %in% class(cl))
  if (!have_cluster) {
    extra_args = list(...)
    if (!isTRUE('names' %in% names(extra_args))) {
      cl = list(parallel::makePSOCKcluster(names = parallel::detectCores()))
    } else {
      cl = list(rlang::exec(.fn = parallel::makePSOCKcluster, !!!extra_args))
    }
    names(cl) = variable
    rlang::env_bind(env, !!!cl)
  }
  cl_val = rlang::env_get(env = env, nm = variable, default = NULL)
  return(cl_val)
}



