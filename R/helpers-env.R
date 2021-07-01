
#' Copy objects from one env to another
#'
#' Ignores missing objects.
#'
#' @param from environment to copy from
#' @param to environment to copy to
#' @param nms names of objects to copy
#' @return names of objects copied
#' @export
env_copy = function(from, to, nms) {
  nms_available = nms[nms %in% rlang::env_names(from)]
  for (nm in nms_available) {
    rlang::env_bind(to, !!nm := rlang::env_get(from, nm))
  }
  return(nms_available)
}

#' Move objects from one env to another
#'
#' Ignores missing objects.
#'
#' @param from environment to copy from
#' @param to environment to copy to
#' @param nms names of objects to copy
#' @return names of objects copied
#' @export
env_move = function(from, to, nms) {
  nms_copied = env_copy(from, to,  nms)
  for (nm in nms_copied) {
    rlang::env_unbind(from, nm)
  }
  return(nms_copied)
}

#' Turn a list of enivronments into a tibble row
#'
#' Resulting tibble includes all variables from all environments in a flat
#' structure
#'
#' @param p list of environments
#' @return tibble row
env_list_as_tibble_row = function(p) {
  names = purrr::map(p,  ls) %>%
    purrr::flatten_chr() %>% 
    sort() %>%
    unique()
  dm = rlang::new_data_mask(top = p$root, bottom = p$running)
  values = names %>% rlang::set_names() %>%
    purrr::map( ~ rlang::env_get(dm, .x, default = NULL, inherit = TRUE)) %>%
    purrr::lift_dl(tibble::tibble_row)()
  return(values)
}

#' Turn a list of environment lists into a tibble
#'
#' @param l list of lists of environments
#' @return a tibble
env_list_list_as_tibble = function(l) purrr::map(l, env_list_as_tibble_row) %>%
  purrr::lift_dl(dplyr::bind_rows)()
