

#' Generic test factory
#'
#' @param sensitivity, as per usual
#' @param specificity, as per usual
#' @return a test function with given specificity/sensitivity
#'
#' @export
test_factory = function(sensitivity, specificity, ...) {
  sensitivity; specificity;
  test_f = function(state, picks) {
    n_non_zero = sum(picks > 0)
    state = state %>%
       dplyr::mutate(
        test_outcome = dplyr::case_when(
          picks > 0 ~ rbinom(n = n(), size = I, prob = sensitivity) +
            rbinom(n = n(), size = S + E + R, prob = (1 - specificity)),
          picks == 0 ~ NA_integer_),
        last_test = dplyr::case_when(
          picks > 0 ~ as.integer(time),
          picks == 0 ~ last_test)
      )
    return(state)
  }
  return(test_f)
}

#' The perfect test
#' 
#' @param state as defined by `make_susceptible` function
#' @param picks rows to include in testing, 0 = skip, > 0 = test
#' @return state with modified `test_outcome` and `last_test` states
#'
#' @export
perfect_test = test_factory(1, 1)

#' Something like a PCR test
#' 
#' @param state as defined by `make_susceptible` function
#' @param picks rows to include in testing, 0 = skip, > 0 = test
#' @return state with modified `test_outcome` and `last_test` states
#'
#' @export
default_test = test_factory(sensitivity = .95, specificity = .999)

#' Factory for functions that pick individuals for testing
#'
#' @param coverage what proportion of rows should be tested 
#' @param how many days between rounds of testing
#' @param delay time required between tests
#' @return function that takes a state df and returns TRUE/FALSE vector for
#'   testing inclusion
#'
#' @export
picker_factory = function(coverage, interval_days, interval_retest, ...) {
  coverage; interval_days; interval_retest;
  picker = function(state) {
    n_test = (nrow(state) * coverage) %>% ceiling()
    available = state %>% dplyr::transmute(
        is_time = time %% interval_days == 0,
        no_recent_test = is.na(last_test) | last_test > interval_retest,
        available = is_time & !quarantine_state & no_recent_test) %>% # FIXME: assumes 1 per row
      dplyr::pull(available)
    pick_idx = sample(x = which(available > 0), size = min(n_test, sum(available)))
    pick = rep(FALSE, nrow(state))
    pick[pick_idx] = TRUE
    return(pick)
  }
  return(picker)
}

#' Picks all rows every time
#'
#' @param state as defined by `make_susceptible` function
#' @return vector of TRUE/FALSE picking rows of the state for testing
#'
#' @export
all_picker = picker_factory(1, 1, 0)


#' Picks all rows every 30 days
#'
#' @param state as defined by `make_susceptible` function
#' @return vector of TRUE/FALSE picking rows of the state for testing
#'
#' @export
monthly_picker = picker_factory(1, 30, 0)
