
#' Turn inputs into a set of job descriptors
#'
#' @param parameters parameters defining the simulation
#' @param initialization recipe for initializing simulation
#' @param step recipe for each simulation step
#' @param writer list with elements anmed 'factory', 'setup', 'summaries'
#' @return a list ...
#'
#' @export
generate_jobs = function(
  parameters = list(),
  initialization = recipe(),
  step = recipe(),
  writer = list(
    factory = make_writer,
    setup = recipe(), summaries = recipe())
) {
  jobs = purrr::map(parameters, ~ list(
    parameters = .x, 
    initialization = initialization, 
    step = step, 
    writer = writer$factory(writer$setup, writer$summaries)))
  return(jobs)
}

