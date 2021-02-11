



#' Get timings for all jobs recursively
#'
#' @param root dir to search from 
#' @return tibble with all timing data
#'
#' @export
get_timings = function(output_dir) {
  files = dir(path = output_dir, pattern = 'timing.rds',
              full.names = TRUE, recursive = TRUE)
  data = tibble::tibble(
      timing = purrr::map(files, ~ readRDS(.) %>%
        purrr::lift_dl(dplyr::bind_rows)()),
      directory = dirname(files),
      path = files) %>%
    dplyr::mutate(
      user_time = purrr::map(timing, ~ .x$user.self),
      elapsed_time = purrr::map(timing, ~ .x$elapsed)
    ) %>% dplyr::select(user_time, elapsed_time, directory, path)
  return(data)
}

#' Summarize timings
#'
#' Summarizes across all replicates
#'
#' @param timing data loaded with `get_timings()`
#' @return tibble of timings with mean/sd/etc..
#'
#' @export
summarize_timings = function(timings) {
  o = timings %>% tidyr::unnest(cols = c('user_time', 'elapsed_time')) %>%
    dplyr::group_by(directory)
  o = list(
    user = dplyr::summarize(o, 
      mean_user_time = mean(user_time),
      sd_user_time = sd(user_time),
      max_user_time = max(user_time),
      total_user_time = sum(user_time)
    ), 
    elapsed = dplyr::summarize(o, 
      mean_elapsed_time = mean(elapsed_time),
      sd_elapsed_time = sd(elapsed_time),
      max_elapsed_time = max(elapsed_time),
      total_elapsed_time = sum(elapsed_time)
    )
  )
  return(o)
}

