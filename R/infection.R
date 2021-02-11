
#' Risk calculation of SEIR model
#'
#' @param state as defined by `make_susceptible` function
#' @param parameters must include 'beta', 'epsilon', and 'gamma'
#'   for infection rate, onset rate, and recovery/death rate
#' @return tibble with p_infection, p_onset, p_recover
#'
#' @export
risk = function(state, parameters) {
  groups = state %>% 
    dplyr::select(location) %>%
    unique
  risk = state %>% 
    dplyr::filter(!quarantine_state) %>%
    dplyr::group_by(location) %>%
    dplyr::select(S, E, I, R) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::mutate(
      N = S + E + I + R,
      p_infection = (1 - exp(-parameters$beta * I / N))
    ) %>%
    dplyr::select(-S, -E, -I, -R, -N) %>% 
    dplyr::right_join(groups) %>%
    dplyr::mutate(
      p_infection = dplyr::if_else(is.na(p_infection), 0, p_infection),
      p_onset = (1 - exp(-parameters$epsilon)),
      p_recover = (1 - exp(-parameters$gamma))
    )
  return(risk)
}

#' Compartment transfers of SEIR model
#'
#' @param state as defined by `make_susceptible` function
#' @param risk risk table from `risk` function
#'
#' @return state, with modified SEIR memership, no other column changes
infection = function(state, risk) {
  nms = names(state)
  state = state %>% 
    dplyr::left_join(
      y = risk, by = 'location') %>%
    dplyr::mutate(
      infection = rbinom(n(), 1, p_infection * S * !quarantine_state),
      onset = rbinom(n(), 1, p_onset * E),
      recovery = rbinom(n(), 1, p_recover * I),
      S = S - infection,
      E = E + infection - onset,
      I = I + onset - recovery,
      R = R + recovery) %>%
    dplyr::select(-p_infection, -p_onset, -p_recover,
                  -infection, -onset, -recovery)
  
  ## Check for structure
  stopifnot(all(names(state) %in% nms))
  stopifnot(all(nms %in% names(state)))
  return(state)  
}
