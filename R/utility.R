
#' Pad strings in a character vector to the same length with leading zeros
#'
#' @param s character vector to pad
#' @param w width to pad to
#' @return padded character vector
#'
#' @export
pad = function(s, w = max(nchar(s))) stringr::str_pad(s, w, "left", "0")


#' Of n items, only those at idx `picks` are TRUE
#'
#' @param picks these will be TRUE, the rest FALSE
#' @param n total number of items
#' @return a vector of logical values
#'
#' @export
tag_picks = function(picks, n) {
  o = rep(FALSE, n)
  o[picks] = TRUE
  return(o)
}

#' Generate a mix of states
#'
#' @param n number of units in the vector
#' @param mix named simplex of proportions of each state
#' @param labels additional (or primary) labels, optional
#' @param .state dev hook, return a character vector if FALSE, otherwise
#'        a 'state' class
#' @return state vector sampled from mix, format depends on .state
#'
#' @export
mix_generator = function(n, mix, labels, .state = TRUE) {
  if (missing(mix) && !missing(labels)) {
    mix = table(labels)
  }
  if (!missing(mix) && !missing(labels)) {
    stopifnot(names(mix) %in%  labels)
  }
  if (missing(labels)) {
    labels = names(mix)
  }
  x = sample(x = names(mix), size = n, replace = TRUE, prob = mix)
  if (.state) {
    x = state$new(x = x, labels = labels)
  }
  return(x)
}

#' Generate a mix of two states, based on probability of the first state
#'
#' @param n number of units to generate
#' @param p probability of first label
#' @param labels two labels for the two stats
#' @return n-vector of either of two labels
#'
#' @export
two_state_generator = function(n, p, labels, .state = TRUE) {
  if (missing(n) && !missing(p)) {
    n = length(p)
  }
  if (missing(p) && !missing(labels)) {
    tab = table(labels)
    p = tab[labels[1]] / sum(tab)
  }
  if (missing(labels)) {
    labels = c(TRUE, FALSE)
  }
  idx = 2 - rbinom(n = n, size = 1, prob = p)
  x = labels[idx]
  if (.state) {
    x = state$new(x = x, labels = labels)
  }
  return(x)
}


#' Standard naming for simulation files
#'
#' The files are named after the variable saved, with aux id data
#'
#' @param variable variable to save (used for name)
#' @param simulation id of simulation, character vector of length 1
#' @param replicate id of replicate, character vector of length 1
#' @param time id of timepoint, character vector of length 1
#' @param extension extension of file type (.rds by default)
#' @param format glue package format string
#' @param padding how many digits to pad the numeric id's to
#' @return file name
#'
#' @export
save_file = function(
  variable, 
  simulation,
  replicate,
  time,
  extension = "rds",
  format = "{variable}--simulation-{simulation}--replicate-{replicate}--time-{time}.{extension}",
  padding = 4
) {
  variable = rlang::enquo(variable) %>%
    rlang::quo_text() %>%
    stringr::str_replace_all('[_:]', '-')
  name = glue::glue(format, variable = variable, simulation = pad(simulation, padding), 
    replicate = pad(replicate, padding), time = pad(time, padding), extension = extension)
  return(name)
}

#' Standard saving for simulation vars
#'
#' The files are named after the variable saved, with aux id data, they
#' are saved in .rds format at the given path.
#'
#' @param variable variable to save (used for name)
#' @param simulation id of simulation, character vector of length 1
#' @param replicate id of replicate, character vector of length 1
#' @param time id of timepoint, character vector of length 1
#' @param extension extension of file type (.rds by default)
#' @param format glue package format string
#' @param padding how many digits to pad the numeric id's to
#' @return file name
#'
#' @export
save_rds = function(variable, root, simulation, replicate, time) {
  var = rlang::enquo(variable)
  file_name = save_file(!!var, simulation, replicate, time)
  file_path = fs::path(root, file_name)
  saveRDS(variable, file = file_path)
  return(file_path)
}


