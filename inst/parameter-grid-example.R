
#devtools::document(); devtools::install(); unloadNamespace('simulator'); devtools::test()
library(simulator)
library(magrittr)


theta_grid = list(
  staff_prevalence = 0.05,
  intake_prevalence = c(0.01, 0.1),
  silent_rate = c(0.1, 0.4),
  r0 = c(1.0, 3.0),
  mean_latent_duration = c(2,5),
  mean_disease_duration = c(10, 14),
  test_sensitivity = list(rapid = c(0.30, 0.55), pcr = c(0.5, 0.7),
    cxr_asymp = c(0.1, 0.2), cxr_symp = c(0.5, 0.65))
) %>% purrr::map(simulator::parameter_range, n = 3) %>%   ## FIXME: needs tests 
      purrr::map(unique) %>%
      simulator::expand_parameter_grid() %>%              ## FIXME: needs tests
      simulator::expand_tibbles_nested() %>%              ## FIXME: needs tests
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
        output_root = file.path(simulation_name, simulation_id, replicate_id)
      ) %>%
      purrr::pmap(list) 

  # specific sim job description here.
  theta = theta_grid %>% purrr::map(~ simulator::parameters(    ## FIXME: needs tests
      output = simulator::recipe(
        output_path = fs::path(output_root),
        log_path = fs::path(output_root, "logger")
      ),
      shared = simulator::recipe(
        beta = r0 / (mean_disease_duration - mean_latent_duration),
        sigma = 1 / mean_latent_duration,
        gamma = 1 / (mean_disease_duration - mean_latent_duration),
        intake_seed_rate = intake_prevalence * n_intake_rate,
        staff_seed_rate = staff_prevalence * n_staff
      ),
      initialization = simulator::recipe(
        time = 0
      ),
      running = simulator::recipe(
        n_steps = n_steps
      ),
      inputs = .x 
    ))







