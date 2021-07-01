
#' Run a simulation for a single set of parameters
#'
#' @param parameters see `parameters` function
#' @param initialization recipe for initializing sim population
#' @param step recipe for runnning the simulation
#' @param writer for recording simulation summaries
#' @return writer, post-application
#'
#' @export
simulation = function(
  parameters,
  initialization,
  step,
  writer = simulator::writer$new()
) {
  writer$setup(parameters$output, parameters$root)
  initialization$execute(parameters$initialization, parameters$root, parameters$running)
  writer$record(parameters$running, parameters$root)
  while(parameters$running$.continue) {
    step$execute(parameters$running, parameters$root)
    writer$record(parameters$running, parameters$root)
  }
  writer$finalize()
  return(writer)
}


#' Run a full set of simulations on the given cluster.
#'
#' @return same parameters as used
#'
#' @export
run_simulation = function(
  parameters, 
  initialization, 
  step, 
  writer, 
  exports, 
  .env = .GlobalEnv, 
  setup = recipe()
) {
  writers = purrr::map(1:length(parameters), ~ simulator::make_writer(writer$setup, writer$summaries))
  parallel::clusterExport(cl = get_cluster(), varlist = exports, envir = .env)
  parallel::clusterEvalQ(cl = get_cluster(), 
    expr = setup$execute(rlang::caller_env(), rlang::caller_env())) 
  parallel::clusterMap(cl = get_cluster(), fun = simulation, 
    parameters = parameters, writer = writers, 
    MoreArgs = list(initialization = initialization, step = step))
  return(parameters)
}















