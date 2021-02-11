

#' Get state-at-time files for a directory
#'
#' They are grouped by replicate directory 
#'
#' @param path root path to check below
#' @return tibble with directory and a list of files in each directory
#'
#' @export
state_files = function(path) {
  files = dir(path = path, pattern = 'state-at-time',
              full.names = TRUE, recursive = TRUE)
  files_df = tibble::tibble(
      directory = dirname(files),
      files = files) %>%
    dplyr::group_by(directory) %>%
    dplyr::summarize(files = list(files))
  return(files_df)
}

#' Summarize state
#'
#' @param state, as describe by `make_susceptible_state` function
#' @return count of S, E, I, and R state by time, location, and quarantine
#'   state
#'
#' @export
count_states = function(state) {
  require(dplyr)
  state_counts = state %>% 
    dplyr::group_by(time, location, quarantine_state) %>%
    dplyr::summarize(
      S = sum(S), E = sum(E), I = sum(I), R = sum(R)) %>%
    dplyr::select(time, location, quarantine_state, S, E, I, R)
  return(state_counts)
}

#' Summarize state file
#'
#' Load file and defer to fixed summary function(s)
#'
#' @param file file to summarize
#' @return specified summary
#'
#' @export
summarize_state_file = function(file) {
  require(magrittr)
  state = readRDS(file) %>% count_states()
  return(state)
}

#' Summarize states for whole simulation directory
#'
#' Each simulation is in one directory
#'
#' @param path process the entire tree rooted at the path
#' @param env additional data loaded on each node
#' @return list of state summary files generated
#' 
#' @export
summarize_simulation_state = function(path, env = .GlobalEnv) {
  files = state_files(path)
  parallel::clusterEvalQ(cl = get_cluster(), expr = library(simulator))
  for (i in 1:nrow(files)) {
    state_summaries = parallel::clusterMap(cl = get_cluster(), summarize_state_file, file = files$files[[i]])
    state_summaries = purrr::lift_dl(dplyr::bind_rows)(state_summaries)
    saveRDS(state_summaries, file = file.path(files$directory[[i]], 'state-summary.rds'))
  }
  return(files)
}
    
#' Get summaries
#'
#' Returns the summarized states for all sets/replicates
#'
#' @param output_dir every summary under this root will be indexed
#' @return a data frame listing all summaries with their set and replicate
#'   index
#'
#' @export
get_state_summaries = function(output_dir) {
  files = dir(path = output_dir, pattern = 'state-summary.rds',
              full.names = TRUE, recursive = TRUE)
  data = tibble::tibble(
      state_summary = purrr::map(files, ~ readRDS(.) %>%
        purrr::lift_dl(dplyr::bind_rows)()),
      directory = dirname(files),
      path = files)
  data = data %>% 
    tidyr::unnest(cols = c('state_summary')) %>%
    dplyr::mutate(
      set = stringr::str_split(directory, '/') %>% purrr::map_chr( ~ .[length(.)-1]),
      replicate = stringr::str_split(directory, '/') %>% purrr::map_chr( ~ .[length(.)])
    )
  return(data)
}


#' Smooth points at at fine scale
#'
#' @param x values to smooth
#' @param t their locations
#' @return smoothed x values
#'
#' @export
smooth = function(x, t) {
  require(mgcv)
  data = tibble::tibble(x = x, t = t)
  o = mgcv::gam(log(x + 1) ~ s(t, k = 30), data = data) %>%
    predict()
  o = exp(o) - 1
  return(o)
}
  
#' Smooth states over time within each location
#'
#' @param data state counts
#' @return smoothed states by set and location
#'
#' @export
summary_smooth = function(data) {
  data = data %>%
    dplyr::group_by(set, location) %>%
    dplyr::mutate(
      lb_S = quantile(S, 0.05) %>% smooth(time), 
      est_S = quantile(S, 0.50) %>% smooth(time), 
      ub_S = quantile(S, 0.95) %>% smooth(time), 
      lb_E = quantile(E, 0.05) %>% smooth(time), 
      est_E = quantile(E, 0.50) %>% smooth(time), 
      ub_E = quantile(E, 0.95) %>% smooth(time), 
      lb_I = quantile(I, 0.05) %>% smooth(time), 
      est_I = quantile(I, 0.50) %>% smooth(time), 
      ub_I = quantile(I, 0.95) %>% smooth(time), 
      lb_R = quantile(R, 0.05) %>% smooth(time), 
      est_R = quantile(R, 0.50) %>% smooth(time), 
      ub_R = quantile(R, 0.95) %>% smooth(time)
    ) %>% dplyr::ungroup()
  return(data)
}
