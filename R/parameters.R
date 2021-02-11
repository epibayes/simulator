

#' Parameter range construction
#'
#' Parameter range will be composed of n points including those 
#' in the vector x.  Sub-lists are processed via recursion using purrr::map
#'
#' @param x points to include in the range, or a list of such vectors
#' @param n number of total points filled in by interpolating between x
#' @return vector of parameters of length n
#'
#' @export
parameter_range = function(x, n) {
  if (class(x) == 'list') {
    x = purrr::map(x, parameter_range, n = n)
    return(x)
  }
  x = sort(x) %>% unique()
  if (length(x) == 0) {
    stop("Must provide at least one default parameter value.")
  }
  if (length(x) == 1) {
    return(rep(x, n))
  }
  if (length(x) >= n) {
    return(x)
  }
  i = 1
  pts = x
  while(length(pts) < n) {
    if (i > length(pts)) {
      i = 1
    }
    new_pts = mean(pts[i:(i + 1)])
    pts = c(pts, new_pts) 
    i = i + 1
  }
  pts = sort(pts) %>% unique()
  return(pts)
}

#' FIXME: Bro, write the tests don't just leave this here
#purrr::map(1:10, ~ parameter_range(c(0, 5, 10), .x))
#purrr::map(1:10, ~ parameter_range(c(0, 5, 10), .x)) %>% purrr::map(length)

#' Classes used to indicate categories
#'
#' @name categorical_classes
#'
#' @export
categorical_classes = c('numeric', 'integer', 'character')

#' Expand parameter grid 
#'
#' Inputs are a list of (optionally nested lists of) vectors and these are
#' expanded within-list using expand.grid and transformed to tibbles for
#' consistency.  Nesting remains to avoid creating the full (possible large) 
#' tibble that represents the full parameter grid.
#'
#' @param x a list, optionally with nested lists, all named elements, 
#'        and leaf elements must be vectors of 'numeric', 'integer', or
#'        'character' class.  They are expanded to all combinations 
#'        in blocks (nested lists create blocking).
#' @return list of tibbles, with nesting in lists
#'
#' @export
expand_parameter_grid = function(x) {
  x_ready = x %>% 
    purrr::keep(~ class(.x) %in% categorical_classes) %>%
    purrr::lift_dl(expand.grid)(stringsAsFactors = FALSE) %>%
    purrr::lift_dl(tibble::tibble)()
  x_other_names = x %>% 
    purrr::discard(~ class(.x) %in% categorical_classes) %>%
    names()
  if (length(x_other_names) == 0) {
    return(x_ready)
  } else {
    x_ready = list(core = x_ready)
  }
  x_other = x %>% 
    purrr::discard(~ class(.x) %in% categorical_classes) %>%
    purrr::map(expand_parameter_grid) %>%
    rlang::set_names(x_other_names)
  for (i in seq_along(x_other)) {
    cn = x_other_names[[i]]
    x_ready[[cn]] = x_other[[i]]
  }
  return(x_ready)
}

#' Expand a flat list of tibbles
#'
#' The implied expansion is for each subsequent tibble to be inserted 
#' as a list column into the first tibble.  This is the equivalent to
#' expand.grid on a (flat) list of tibbles).
#'
#' @param x a flat list of tibbles
#'
#' @return a single tibble with (likely) nested tibbles
#'
#' @export
expand_tibbles = function(x) {
  if (length(x) <= 1) {
    if ( rlang::is_list(x) && 
        !rlang::is_tibble(x) && 
         rlang::is_tibble(x[[1]])) {
      x = x[[1]]
    }
    return(x)
  }
  chk = purrr::map_lgl(x, tibble::is_tibble) %>%
    all() %>% isTRUE()
  if (!chk) rlang::abort(
    message = "`expand_tibbles` should only be called on a list of tibbles.", 
    class = 'wrong-args', arg = x)
  if (length(x) == 2) {
    tail_name = names(x[2])
    tail_val = purrr::map(1:nrow(x[[1]]), ~ x[[2]])
    x = x[[1]] %>% dplyr::mutate(!!tail_name := tail_val)
    return(x)
  }
  x[2] = expand_tibbles(x[1:2])
  x = expand_tibbles(x[-1])
  return(x)
}

#' Flatten a list of (possibly nested list of) tibbles to a single tibble
#'
#' Nesting is preserved as nested list columns of tibbles
#'
#' @param a list of (possibly nested lists of ) tibbles
#' @return a single tibble with nested columns
#'
#' @export
expand_tibbles_nested = function(x) {
  if (tibble::is_tibble(x)) {
    return(x)
  }
  chk = purrr::map_lgl(x, ~ tibble::is_tibble(.x) || rlang::is_list(.x)) %>%
    all() %>% isTRUE()
  if (!chk) rlang::abort(
    message = paste("`expand_tibbles_nested` should only be called on a list of nested",
      "lists of tibbles."),
    class = 'wrong-args', arg = x)
  descend = purrr::map_lgl(x, ~ tibble::is_tibble(.x) %>% isFALSE())
  if (any(descend)) {
    x = x %>% purrr::map_if(descend, expand_tibbles_nested) %>%
      expand_tibbles_nested()
  } else {
    x = expand_tibbles(x)
  }
  return(x)
}

#' Unnest all columns within a nested tibble recursively
#'
#' @param x a tibble
#' @return a tibble with no nested columns left
#'
#' @export
flatten_tibble = function(x) {
  if (!tibble::is_tibble(x)) rlang::abort(
    message = "'flatten_tibble' should only be called on a tibble",
    class = 'wrong-args', arg = x)
  nested_col_idx = purrr::detect_index(x, ~ class(.x) == 'list')
  if (nested_col_idx > 0) {
    x = tidyr::unnest(x, tidyselect::all_of(nested_col_idx)) %>%
      flatten_tibble()
  }
  return(x)
}


#x = inputs[13:19] %>% expand_parameter_grid()
#o = expand_tibbles_nested(x)

