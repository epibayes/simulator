
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
#' @param parameters, need to describe all info required for sim
#' @param env environment to pull additional variables from for each sim node.
#' @return same parameters as used
#'
#' @export
run_simulation = function(parameters, env = .GlobalEnv, ...) {
  parallel::clusterExport(cl = get_cluster(), varlist = ls(env), envir = env)
  parallel::clusterMap(cl = get_cluster(), fun = simulation, 
                       parameters = parameters, MoreArgs = list(...))
  return(parameters)
}















