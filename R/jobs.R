
#' Turn inputs into a set of job descriptors
#'
#' @param env an environment where constant parameters are defined
#' @param modifiers a list of tibbles where per-scenario parameters are defined
#' @param n_replicates number of replicates to create per set
#' @param output_root location to output results to.  There is a specific
#'   directory structure implied under this location (it will be created).
#' @param tests a factory function for creating testing functions for each set
#'   by default uses functions from the package
#' @param pickers a factory function for createing picker functions for each
#'   set, by default uses factory function from the package
#' @return a list of list with a descriptor of sets, sets with replicates, and
#'   a list of jobs to run
#'
#' @export
generate_jobs = function(env, modifiers, n_replicates, output_root = 'output',
  tests = test_factory, pickers = picker_factory
) {
  sets = modifiers %>% 
    purrr::map(tibble::tibble_row) %>%
    purrr::lift_dl(dplyr::bind_rows)() %>% 
    dplyr::mutate(set_id = 1:n()) %>%
    dplyr::select(set_id, coverage, sensitivity, interval_days, interval_retest)
    
  replicates = expand.grid(
    replicate_id = 1:n_replicates,
    set_id = sets$set_id)
  
  replicates = replicates %>% dplyr::left_join(sets, by = 'set_id') %>%
    dplyr::mutate(
      job_id = pad(1:n()),
      replicate_id = pad(replicate_id),
      set_id = pad(set_id),
      output_dir = file.path(output_root, set_id, replicate_id)
    )

  jobs_list = purrr::pmap(replicates, function(..., env) {
    o = purrr::list_merge(..., as.list(env))
    o[['picker']] = rlang::exec(pickers, !!!o)
    o[['test']] = rlang::exec(tests, !!!o)
    return(o)
  }, env = env)

  parameters = list(
    sets = sets, 
    replicates = replicates,
    jobs = jobs_list)

  return(parameters)
}

