
#devtools::document(); devtools::install(); unloadNamespace('simulator'); devtools::test()
library(simulator)
library(magrittr)

# step 1: create a list
# result is a list of 7
# staff_prevalence is a num
# intake_prevalence is a set of 2 num
# silent_rate is a set of 2 num
# r0 is a set of 2 num
# mean_latent_duration is a set of 2 num
# mean_disease duration is a set of 2 num
# test sensitivity is a list of 4. each list item is a set of 2 num
theta_list = list(
  staff_prevalence = 0.05,
  intake_prevalence = c(0.01, 0.1),
  silent_rate = c(0.1, 0.4),
  r0 = c(1.0, 3.0),
  mean_latent_duration = c(2,5),
  mean_disease_duration = c(10, 14),
  test_sensitivity = list(rapid = c(0.30, 0.55), pcr = c(0.5, 0.7),
    cxr_asymp = c(0.1, 0.2), cxr_symp = c(0.5, 0.65))
) 

# step 2: map with purr
# each obj in theta_grid is expanded to the specified parameter range
# result is a list of 7
# staff_prevalence, a set of 3 num
# intake_prevalence, a set of 3 num
# silent_rate, a set of 3 num
# r0, a set of 3 num
# mean_latent_duration: a set of 3 num
# mean_disease_duration, a set of 3 num
# test_sensitivity, a list of 4. each list item is a set of 3 num
theta_purr = theta_list %>% 
  purrr::map(simulator::parameter_range, n = 3) 

# step 3: grab uniques
# result is a list of 7
# staff_prevalence, a num
# intake_prevalence, a set of 3 num
# silent rate, a set of 3 num
# r0, a set of 3 num
# mean_latent_duration, a set of 3 num
# mean_disease_duration, a set of 3 num
# test_sensitivity, a list of 4 sets of 3 num
theta_unique = theta_purr %>%   ## FIXME: needs tests 
      purrr::map(unique) 

# step 4: with uniques, expand parameter grid
# result is a list of 2: 
# core, a 243 * 6 tibble object; 
# test_sensitivity, a 81 * 4 tibble object
theta_expand_param = theta_unique %>%
      simulator::expand_parameter_grid() 

# step 5: with parameter grid, expand nested tibbles
# result is 243 obs of 7 variables
# staff_prevalence is a set of 243 num
# intake_prevalence is a set of 243 num
# silent_rate is a set of 243 num
# r0 is a set of 243 num
# mean_latent_duration is a set of 243 num
# mean_disease_duration is a set of 243 num
# test_sensitivity is a list of 243 (81 * 4) tibbles; 
## each of the 4 var have sets of 81 num
theta_expand_tibbles = theta_expand_param %>%              ## FIXME: needs tests
      simulator::expand_tibbles_nested() 

# step 6: unnest sensitivity
# result is 19683 obs. of 10 variables
# staff_prevalence is a set of 19683 num
# intake_prevalence is a set of 19683 num
# silent_rate is a set of 19683 num
# r0 is a set of 19683 num
# mean_latent_duration is a set of 19683 num
# mean_disease_duration is a set of 19683 num
# var1, var2, var3, and var4 are sets of 19683 num
theta_sensitivity = theta_expand_tibbles %>%              ## FIXME: needs tests
      tidyr::unnest(test_sensitivity) 

# step 7: map out into a new list using just the unique values
# result is a large list of 19683 elements
# a list of 21, with:
## staff_prevalence, a number
## intake_prevalence, a number
## silent_rate, a number
## r0, a number
## mean_latent_duration, a number
## mean_disease_duration, a number
## var1, var2, var3, var4, each a number
## simulation_name, "grid-test-1"
## simulation_id, a character string ("00001", "00002" etc up to 19683)
## replicate_id, a character string ("001")
## n_intake_rate, a number
## n_staff, a number
## n_steps, a number
## max_n_steps a number
## seed, a number
## tb_rate, a number
## isolation_retest, a number
## output_root, a character string ("grid-test-1/{simulation_id}/{replicate_id})
theta_grid = theta_sensitivity %>%
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

#final: theta is a large list of 19683 elements. each element is a list of 5 environments?
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







