
#' Combine environments in a hierarchy for passing simulation data
#'
#' @param output recipe used for defining output location parameters
#' @param shared recipe used for defining shared parameters
#' @param initialization recipe used for defining init parameters
#' @param running recipe used for defining run-time parameters
#' @param inputs list of variables passed in to all steps
#' @param .parent dev-side hook, parent of everything.
#' @return list of environments with specified nesting
#'
#' @export
parameters = function(
  output = recipe(),
  shared = recipe(),
  initialization = recipe(),
  running = recipe(),
  inputs = list(), 
  .parent = rlang::env()
) {
  root_ = rlang::new_environment(parent = .parent)
  purrr::imap(inputs, ~ rlang::env_bind(root_, !!.y := .x))
  output_ = rlang::new_environment(parent = root_)
  shared_ = rlang::new_environment(parent = output_)
  initialization_ = rlang::new_environment(parent = shared_)
  running_ = rlang::new_environment(parent = shared_)
  thetas = list(
    root = root_,
    output = output_, 
    shared = shared_, 
    initialization = initialization_, 
    running = running_)
  output$execute(output_, root_)
  shared$execute(shared_, root_)
  initialization$execute(initialization_, root_)
  running$execute(running_, root_)
  return(thetas)
}

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















