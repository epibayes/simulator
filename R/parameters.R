

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
  if (length(x) == 0) {
    stop("Must provide at least one default parameter value.")
  }
  if (class(x) == 'list') {
    x = purrr::map(x, parameter_range, n = n)
    return(x)
  }
  x = sort(x) %>% unique()
  if (length(x) == 1) {
    return(rep(x, n))
  }
  if (length(x) >= n) {
    return(x)
  }
  
  pts = parameter_range_impl(x,n)

  pts = sort(pts) %>% unique()
  return(pts)
}

#' Insert numbers between vector values until the vector contains
#' n values
#'
#' find an equal number of equidistant points between each 
#' pair of numbers in x to get a total n number of numbers.
#' any extra points will be made where the interval 
#' (distance between two #s in x) is largest.
#'
#' @param x a vector of numbers
#' @param n number of total points
#' @return a vector of length n
#' 
parameter_range_impl = function(x, n) {
  x = sort(x)
  d = diff(x)
  n_intervals = length(d)

  intervals = purrr::map2(x[1:n_intervals], d, 
    ~ Interval$new(upper = .x + .y, lower = .x, length = .y)) 

  #' determine where the new numbers will occur
  n_more_pts = n - length(intervals) - 1
  index = length(intervals)
  while (n_more_pts > 0){
    intervals[[index]]$increment_counter()
    if (index == 1){
      index = length(intervals)
    } else {
      index = index - 1
    }
    n_more_pts = n_more_pts - 1
  }

  #' determine and insert values of new numbers
  for (ivl in intervals){
    increment = ivl$length / (ivl$counter + 1)
    for (j in 1:ivl$counter) {
      x = x %>% c(ivl$lower + j * increment)
    }
  }
  x = sort(x)
  return(x)
}

#' Classes used to indicate categories in the context of parameters
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
expand_parameter_grid = function(x, stub = '', sep ='__') {
  if(is.null(names(x))){
    rlang::abort("all elements of x must be named")
  }
  x_tibble_names = x %>%
    purrr::keep(~ any(c('data.frame', 'tbl_df') %in% class(.x))) %>%
    names()
  x_tibbles = x %>%
    purrr::keep(~ any(c('data.frame', 'tbl_df') %in% class(.x)))
  x_expanded_names = x %>% 
    purrr::discard(~ any(c('data.frame', 'tbl_df') %in% class(.x))) %>%
    purrr::keep(~ class(.x) %in% categorical_classes) %>%
    names()
  x_expanded = x %>% 
    purrr::discard(~ any(c('data.frame', 'tbl_df') %in% class(.x))) %>%
    purrr::keep(~ class(.x) %in% categorical_classes) %>%
    purrr::lift_dl(expand.grid)(stringsAsFactors = FALSE) %>%
    purrr::lift_dl(tibble::tibble)()
  if (length(x_expanded) > 0 && nrow(x_expanded > 0)) {
    if (stub != '') {
      names(x_expanded) = paste(stub, names(x_expanded), sep = sep)
    }
    x_expanded = list(x_expanded)
    if (stub != '') {
      names(x_expanded) = stub
    } else {
      names(x_expanded) = 'core'
    }
  }
  x_other = x %>% 
    purrr::discard(~ any(class(.x) %in% categorical_classes)) %>%
    purrr::discard(~ any(c('data.frame', 'tbl_df') %in% class(.x)))
  x_other_names = names(x_other)
  if (stub != '') {
    x_other_stubs = paste(stub, x_other_names, sep = sep)
  } else {
    x_other_stubs = x_other_names
  }
  x_other = x_other %>%
    purrr::map2(x_other_stubs, expand_parameter_grid, sep = sep) %>%
    rlang::set_names(x_other_names)
  if (length(x_tibbles) > 0) {
    x_ready = x_tibbles
  }
  if (length(x_expanded[[1]]) > 0 && nrow(x_expanded[[1]]) > 0) {
    if (length(x_tibbles) == 0) {
      x_ready = x_expanded
    } else {
      x_ready = c(x_ready, x_expanded)
    }
  }
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
        !tibble::is_tibble(x) && 
         tibble::is_tibble(x[[1]])) {
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
  x[[2]] = expand_tibbles(x[1:2])
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

#' Flatten nested list of tibble to non-nested list of tibbles
#'
#' @param x list of nested (sometimes lists of) tibbles flatten
#' @param l propagate return value in recursion
#' @return list of tibbles
#'
#' @export
flatten_tibble_list = function(x, l = list()) {
  for (i in seq_along(x)) {
    ith_class = class(x[[i]])
    if (any(c('tbl_df', 'data.frame') %in% ith_class)) {
      l = c(l, x[i])
    } else if ('list' %in% ith_class) {
      l = flatten_tibble_list(x[[i]], l)
    } else {
      stop("Unexpected class in list of tibbles.")
    }
  }
  return(l)
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

#' Update parameter data with stage-specific recipes
#'
#' Input data is present in the root environment and further
#' stages form a hierarchy for the derived stage-specific data
#' defined in recipes
#'
#' @param .data list of variables passed in to all steps
#' @param output recipe used for defining output location parameters
#' @param shared recipe used for defining shared parameters
#' @param initialization recipe used for defining init parameters
#' @param running recipe used for defining run-time parameters
#' @param .parent dev-side hook, parent of everything.
#' @return list of environments with specified nesting
#'
#' @export
parameters = function(
  data = list(), 
  output = recipe(),
  shared = recipe(),
  initialization = recipe(),
  running = recipe(),
  .parent = rlang::env(),
  .return_type = 'list'
) {
  root_ = rlang::new_environment(parent = .parent)
  purrr::imap(data, ~ rlang::env_bind(root_, !!.y := .x))
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
  if (.return_type == 'tibble') {
    return(env_list_as_tibble_row(thetas))
  }
  return(thetas)
}



#' Expand lists of parameters, combine and insert metadata 
#'
#' Each of the dot-args is exapnded individuall, they are combined
#' using bind_rows and so much have matching parameters, and then
#' simulation id's, replicate id's, job tags, and job batch labels
#' are added
#'
#' @param ... each dot-arg is a list of (optionally nested) lists
#'        of (ultimately) numeric/integer/character arguments or 
#'        tibbles.  
#' @param recipes used to create derived parameters from the primary 
#'        expanded parameters
#' @param n_batches number of batches to break the work into (only 
#'        generates a grouping factor with labels).
#' @return tibble of parameter values
#' @export
expand_parameters = function(..., recipes, n_batches = 50) {
  args = list(...)
  for (i in seq_along(args)) {
    args[[i]] = args[[i]] %>%
      expand_parameter_grid() %>% 
      flatten_tibble_list() %>% 
      expand_tibbles_nested() %>% 
      flatten_tibble() %>%
      unique()
  }
  parameter_grid = purrr::lift_dl(dplyr::bind_rows)(args) %>%
    dplyr::mutate(
      simulation_id = simulator::pad(1:dplyr::n(), nchar(dplyr::n())),
      width_replicate_id = max(nchar(n_replicates)),
      replicate = purrr::map(n_replicates, 
        ~ tibble::tibble(id = simulator::pad(1:.x, unique(width_replicate_id))))
    ) %>% 
    dplyr::select(-width_replicate_id) %>%
    tidyr::unnest(replicate, names_sep = '_') %>%
    dplyr::mutate(
      output_path = list("simulations", simulation_name, simulation_id, replicate_id) %>%
        purrr::pmap(workflow::build_dir) %>%
        purrr::flatten_chr(),
      job_tag = paste(simulation_name, '-simulation-id', simulation_id,
        '-replicate-id', replicate_id, sep = '-'),
      job_batch = paste0("B", sample.int(n = n_batches, size = dplyr::n(), replace = TRUE))
  )
  parameter_grid = parameter_grid %>%
    purrr::pmap(list) %>%
    purrr::map(simulator::parameters, output = recipes$output, shared = recipes$shared,
      initialization = recipes$initialization, running = recipes$running) %>%
    env_list_list_as_tibble()
  return(parameter_grid)
}

