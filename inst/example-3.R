
#devtools::document(); devtools::install(); unloadNamespace('simulator'); devtools::test()
library(simulator)
library(magrittr)
library(ggplot2)

#' Column-wise rules for infection status
calculate_status = function(time, infection_time, latent_duration, infectious_duration) {
  N = length(infection_time)
  infected = !is.na(infection_time)
  infection_age = time - infection_time
  stopifnot(all(is.na(infection_age) | infection_age >= 0))
  status = rep(NA_character_, N) %>%
    purrr::map_if(!infected, ~ 'S') %>%
    purrr::map_if(infected & (infection_age <= latent_duration), ~'E') %>%
    purrr::map_if(infected & (infection_age  > latent_duration) & 
      (infection_age <= latent_duration + infectious_duration), ~'I') %>%
    purrr::map_if(infected & (infection_age > latent_duration + infectious_duration), ~ 'R') %>%
    purrr::flatten_chr()
  stopifnot(all(!is.na(status)))
  status = state$new(x = status, labels = c('S', 'E', 'I', 'R'))
  return(status)
}

#' Column-wise rules for isolation status
calculate_isolation = function(time, start_time, duration, status, test_pr) {
  N = length(start_time)
  isolated = !is.na(start_time)
  isolation_age = time - start_time
  stopifnot(all(is.na(isolation_age) | isolation_age >= 0))
  iso_status = rep(NA, N) %>% 
    purrr::map_if(!isolated, ~ FALSE) %>%
    purrr::map_if(isolated & isolation_age <= duration, ~ TRUE) %>%
    purrr::map_if(isolated & isolation_age > duration & status %in% c('E', 'I'), 
      ~ rbinom(n = 1, size = 1, prob = test_pr) == 1) %>%
    purrr::map_if(isolated & isolation_age > duration & status %in% c('S', 'R'), ~ FALSE) %>%
    purrr::flatten_lgl()
  stopifnot(all(!is.na(iso_status)))
  return(iso_status)
}

#' Constructing each new cohort
daily_cohort_recipe = recipe(
  # Population initialization
  latent_period_duration = rexp(n = n_intake_today, rate = sigma),
  disease_period_duration = rexp(n = n_intake_today, rate = gamma),
  infection_time = rep(NA_real_, n_intake_today) %>% 
    purrr::map_at(
        sample.int(n_intake_today, n_intake_infections), 
      ~ time - sample.int(mean_disease_duration, size = 1)) %>%
    purrr::map_dbl(as.numeric),
  infection_status = calculate_status(time, infection_time, latent_period_duration, disease_period_duration),
  infection_silent = simulator::mix_generator(n = n_intake_today, 
    mix = c(silent = silent_rate, symptomatic = 1 - silent_rate)),
  isolation_start_time = rep(NA_real_, n_intake_today),
  isolation = rep(FALSE, n_intake_today),

  # Intake testing
  symptomatic_assessment_result = simulator::two_state_generator(
    labels = c('positive', 'negative'),
    p = infection_status$state == 'I' & infection_silent$state == 'symptomatic'),
  rapid_test_result = simulator::two_state_generator(
    labels = c('positive', 'negative'),
    p = (infection_status$state == 'I') * test_sensitivity['rapid']),
  pcr_test_result = simulator::two_state_generator(
    labels = c('positive', 'negative'),
    p = (infection_status$state == 'I') * test_sensitivity['pcr']),
  cxr_test_result = simulator::two_state_generator(
    labels = c('positive', 'negative'),
    p = (infection_status$state %in% c('I', 'R')) * (
      (infection_silent$state == 'silent') * test_sensitivity['cxr_asymp'] +
      (infection_silent$state == 'symptomatic') * test_sensitivity['cxr_symp'])),
  cxr_tb_test_result = simulator::two_state_generator(
    labels = c('positive', 'negative'), p = rep(tb_rate, infection_status$n_units)),
  test_positive = 
    symptomatic_assessment_result$state == 'positive' |
    rapid_test_result$state == 'positive' |
    pcr_test_result$state == 'positive' |
    cxr_test_result$state == 'positive' |
    cxr_tb_test_result$state == 'positive'
)

#' Once per simulation
sim_init = recipe(
  cohorts = list(),
  n_intake_today = numeric(),
  n_intake_infections = numeric(),
  n_staff_infections = numeric(),
  time = time,
  .continue = TRUE,
  set.seed(seed)
)

#' Each time step of the simulation
sim_step = recipe(
  time = time + 1,
  cohorts = cohorts %>% 
    purrr::discard( ~ isTRUE(all(.x$get('infection_status')$state %in% c('S', 'R')))) %>%
    purrr::map( ~ {.x$time = time; .x}) %>%
    purrr::map( ~ .x$mutate(test_positive = rep(FALSE, length(test_positive)))),
  n_intake_today = rpois(n = 1, lambda = n_intake_rate),
  n_intake_infections = rpois(n = 1, lambda = intake_seed_rate),
  n_staff_infections = rpois(n = 1, lambda = staff_seed_rate), 

  today_cohort = simulator::make_population(
    time = time, group = paste0('cohort--', simulator::pad(time, 3)),
    n_intake_today = n_intake_today, n_intake_infections = n_intake_infections,
    n_staff_infections = n_staff_infections, mean_disease_duration = mean_disease_duration,
    sigma = sigma, gamma = gamma, silent_rate = silent_rate, tb_rate = tb_rate,
    test_sensitivity = test_sensitivity,
    steps = daily_cohort_recipe),

  purrr::map(cohorts, ~ .x$mutate(
      # State update
      infection_status = calculate_status(time, infection_time, 
        latent_period_duration, disease_period_duration),

      # Regular testing
      symptomatic_assessment_result = simulator::two_state_generator(
        labels = c('positive', 'negative'),
        p = infection_status$state == 'I' & infection_silent$state == 'symptomatic'),
      test_positive = symptomatic_assessment_result$state == 'positive'
  )),

  purrr::map_if(cohorts, ~ .x$age == 14, ~ .x$mutate(
      pcr_test_result = simulator::two_state_generator(
        labels = c('positive', 'negative'),
        p = (infection_status$state == 'I') * test_sensitivity['pcr']),
      test_positive = test_positive | pcr_test_result$state == 'positive'
  )),

  cohorts = c(cohorts, list(today_cohort)),
  purrr::map(cohorts, ~ .x$mutate(
      isolation_start_time = isolation_start_time %>% 
        purrr::map_if(test_positive, ~ time) %>% 
        purrr::flatten_dbl(),
      isolation = calculate_isolation(time, isolation_start_time, isolation_retest, 
        infection_status$state, test_sensitivity['pcr']),
      escaped_case = infection_status$state == 'I' & !isolation,
      infection_lambda = (infection_status$state == 'S') * beta *
        (sum(escaped_case) / (length(isolation) - sum(isolation))),
      p_infection = 1 - exp(-infection_lambda),
      new_case = rbinom(n = length(infection_time), size = 1, prob = p_infection) == 1,
      infection_time = infection_time %>% 
        purrr::map_if(new_case, ~ time) %>%
        purrr::flatten_dbl(),
  )),
  
  .continue = ((length(cohorts) > 0) || time <= n_steps) & time <= max_n_steps
)

#' Object responsible for saving simulation data
summarizer = writer$new(
  setup = recipe(
    .output_path = output_path,
    output_path %>% fs::dir_create(recurse = TRUE),
    log_path = log_path,
    log_path %>% fs::dir_create(recurse = TRUE),
    log_file = fs::path(log_path, "logger.txt"),
    cat("log file: ", log_file, "\n"),
    logger::log_appender(logger::appender_file(log_file))
  ),
  summaries = recipe(
    .time = time,
    .simulation = simulation_id,
    .replicate = replicate_id,
    logger::log_info("step: {time}, n_cohorts: {n_cohorts}", 
      time = time, n_cohorts = length(cohorts))
    timepoint_data = tibble::tibble(
      time = time,
      simulation_id = simulation_id,
      replicate_id = replicate_id,
      n_intake_today = n_intake_today,
      n_intake_infections = n_intake_infections,
      n_staff_infections = n_staff_infections
    ),
    cohort_summary = purrr::map(cohorts, ~ c(list(group = .x$group, age = .x$age), .x$summarize(
        p_infection = max(p_infection), 
        n_exposed = sum(infection_status$state == 'E'),
        n_infectious = sum(infection_status$state == 'I'),
        n_exposed_escaped = sum(infection_status$state == 'E' & !isolation),
        n_infectious_escaped = sum(infection_status$state == 'I' & !isolation),
        n_recovered = sum(infection_status$state == 'R'),
        n_recovered_escaped = sum(infection_status$state == 'R' & !isolation),
        n_escaped = sum(escaped_case),
        n_infections = sum(new_case),
        n_total = length(isolation),
        n_isolated = sum(isolation),
        n_susceptible = sum(infection_status$state == 'S'),
        .return_type = 'list')) %>%
      purrr::lift_dl(tibble::tibble)()
    ) %>% purrr::lift_dl(dplyr::bind_rows)() %>%
      dplyr::mutate(time = time),
    cohorts = cohorts
  )
)

theta_grid = list(
  staff_prevalence = 0.05,
  intake_prevalence = c(0.01, 0.1),
  silent_rate = c(0.1, 0.4),
  r0 = c(1.0, 3.0),
  mean_latent_duration = c(2,5),
  mean_disease_duration = c(10, 14),
  test_sensitivity = list(rapid = c(0.30, 0.55), pcr = c(0.5, 0.7),
    cxr_asymp = c(0.1, 0.2), cxr_symp = c(0.5, 0.65))
) %>% purrr::map(simulator::parameter_range, n = 3) %>%
      purrr::map(unique) %>%
      simulator::expand_parameter_grid() %>%
      simulator::expand_tibbles_nested() %>%
      tidyr::unnest(test_sensitivity) %>%
      unique() %>%
      dplyr::mutate(
        simulation_name = "grid-test-1",
        simulation_id = simulator::pad(1:dplyr::n(), nchar(dplyr::n())), 
        replicate_id = simulator::pad(1, 3),
        n_intake_rate = 100,
        n_staff = 3000,
        n_steps = 365,
        max_n_steps = 425,
        seed = 33,
        tb_rate = 0.001,
        isolation_retest = 14,
        output_root = workflow::build_dir(simulation_name, simulation_id, replicate_id)
      ) %>%
      purrr::pmap(list) 

  # specific sim job description here.
  theta = theta_grid %>% purrr::map(~ simulator::parameters(
      output = simulator::recipe(
        output_path = fs::path(output_root),
        log_path = fs::path(output_root, "logger")
      ),
      shared = simulator::recipe(
        beta = r0 / (mean_disease_duration - mean_latent_duration),
        sigma = 1 / mean_latent_duration,
        gamma = 1 / (mean_disease_duration - mean_latent_duration),
        intake_seed_rate = intake_prevalence * n_intake_rate,
        staff_seed_rate = staff_prevalence * n_staff,
        calculate_status = calculate_status,
        calculate_isolation = calculate_isolation
      ),
      initialization = simulator::recipe(
        time = 0
      ),
      running = simulator::recipe(
        n_steps = n_steps
      ),
      inputs = .x 
    ))

#o = run_simulation(theta, env = .GlobalEnv, 
#  initialization = sim_init, step = sim_step, writer = summarizer)
sim = simulation(theta[[1]], sim_init, sim_step, summarizer)

