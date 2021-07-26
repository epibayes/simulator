

#' Create the initial state for a simulation.
#'
#' This is also an informal definition of the format of data stored
#' for the simulation.  We avoid ad-hoc modifications of the columns in the
#' code.  In the future that should make it possible to make this more of an
#' insulated object.
#'
#' Each row holds the state for one individual, although having it hold the
#' state for one group is reasonable by turning the 0/1 convention into a count
#' convention.  Marking the functions that would need to change elsewhere with 
#' FIXME as a go.
#'
#' @param time base time to start pop from
#' @param group group name for this pop
#' @param ... data available to parameterize recipe
#' @param steps recipe to construct population columns
#' @param .env enviornment use for population
#' @param .counter counter used as source for id's
#' @return tibble::tibble holding the simulation state for N individuals
#'
#' @export
make_population = function(
  time, group, ...,
  steps = recipe(),
  .env,
  .counter,
) {
  if (missing(.env)) {
    .env = rlang::env()
  }
  if (missing(.counter)) {
    .counter = simulator::get_counter()
  }
  e_local = rlang::new_environment(parent = .env)
  context = list(...)
  context_vars = names(context)
  rlang::env_bind(e_local, !!!context)
  rlang::env_bind(e_local, time = time, group = group)
  steps$execute(e_local, e_local)
  rm(list = c(context_vars, 'time', 'group'), envir = e_local)
  data = as.list(e_local)
  o = population$new(time = time, group = group, 
    data = data, .env = .env, .counter = .counter)
  return(o)
}
