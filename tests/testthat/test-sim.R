

testthat::test_that("Simulation parameters list parent-child relationships work.", {
  theta = parameters(
    output = recipe(
      a = dog, zerg = cat, nono = 55, file = "bananas"),
    shared = recipe(
      some_letters = sample(letters, nono, replace = TRUE),
      bug = 55),
    initialization = recipe(n_iterations = 100, n_steps = 50),
    running = recipe(n_iterations = 101, n_steps = 52),
    data = list(dog = "zebra", cat = 10))
  testthat::expect_equal(theta$initialization$n_iterations, 100)
  testthat::expect_equal(theta$running$n_iterations, 101)
  testthat::expect_equal(rlang::env_get(theta$running, 'a', inherit = TRUE), "zebra")
  testthat::expect_equal(length(theta$shared$some_letters), theta$output$nono)
})

testthat::test_that("Simulation parameters list parent-child relationships work for steps.", {
  theta = parameters(
    output = recipe(
      a = dog, zerg = cat, nono = 55, file = "bananas"),
    shared = recipe(
      some_letters = sample(letters, nono, replace = TRUE),
      bug = 55),
    initialization = recipe(n_iterations = 100, n_steps = 50),
    running = recipe(n_iterations = 101, n_steps = 52),
    data = list(dog = "zebra", cat = 10))
  steppies = recipe(bobo = nono + n_steps + n_iterations)
  testthat::expect_equal(steppies$execute(bottom = theta$running, top = theta$root)$bobo, 55 + 52 + 101)
  testthat::expect_equal(steppies$execute(theta$initialization, theta$root)$bobo, 55 + 50 + 100)
})

testthat::test_that("Simulation function runs a simple simulation.", {
  theta = parameters(
    output = recipe(
      .output_path = fs::file_temp(), log_file = fs::file_temp(),
      dir.create(.output_path, recursive = TRUE)
    ),
    shared = recipe(
      n_total = rpois(n = 1, lambda = n_total_rate),
      n_seed_infections = rpois(n = 1, lambda = n_seed_rate)
    ),
    initialization = recipe(
      n_infected = n_seed_infections,
      n_susceptible = n_total - n_infected,
      n_recovered = 0,
      n_dead = 0,
      time = 0
    ),
    running = recipe(
      n_steps = n_steps
    ),
    data = list(
      simulation_name = "simple-test",
      simulation_id = "A0001",
      replicate_id = "R00001",
      n_total_rate = 200,
      n_seed_rate = 5,
      beta = 1.5,
      delta = 5, 
      gamma = 1, 
      n_steps = 50,
      seed = 33
    )
  )

  testthat::expect_equal(rlang::env_get(theta$running, 'seed', inherit = TRUE), 33)

  initialization_steps = recipe(
    testthat::expect_true(rlang::is_scalar_double(n_infected) || 
                          rlang::is_scalar_integer(n_infected)),
    testthat::expect_true(rlang::is_scalar_double(n_susceptible) || 
                          rlang::is_scalar_integer(n_susceptible)),
    testthat::expect_identical(n_recovered, 0),
    testthat::expect_identical(n_dead, 0),
    testthat::expect_true(rlang::is_scalar_double(beta)),
    testthat::expect_true(rlang::is_scalar_double(delta)),
    testthat::expect_true(rlang::is_scalar_double(gamma)),
    n_infected = n_infected,
    n_susceptible = n_susceptible,
    n_recovered = n_recovered,
    n_dead = n_dead,
    n_infections = 0,
    n_deaths = 0,
    n_recoveries = 0,
    p_infection = 0,
    p_death = 0,
    p_recovery = 0,
    time = time,
    .continue = TRUE,
    set.seed(seed)
  )

  simulation_steps = recipe(
    p_infection = dplyr::case_when(
      n_susceptible > 0 ~ 1 - exp(-beta * n_infected / (n_infected + n_susceptible + n_recovered)),
      n_susceptible == 0 ~ 0),
    testthat::expect_true(p_infection >= 0),
    testthat::expect_true(p_infection <= 1),
    p_death = 1 - exp(-gamma),
    testthat::expect_true(p_death >= 0),
    testthat::expect_true(p_death <= 1),
    p_recovery = 1 - exp(-delta),
    testthat::expect_true(p_recovery >= 0),
    testthat::expect_true(p_recovery <= 1),
    n_infections = rbinom(n = 1, size = n_susceptible, prob = p_infection),
    n_deaths = rbinom(n = 1, size = n_infected, prob = p_death),
    n_recoveries = rbinom(n = 1, size = n_infected - n_deaths, prob = p_recovery),
    n_dead = n_dead + n_deaths,
    n_susceptible = n_susceptible - n_infections,
    n_infected = n_infected + n_infections - n_deaths - n_recoveries,
    n_recovered = n_recovered + n_recoveries,
    time = time + 1,
    .continue = time < n_steps && n_infected > 0
  )
  basic_writer = writer$new(
    setup = recipe(
      log_file = log_file,
      .output_path = .output_path,
      logger_dir = log_file %>% fs::path_dir(),
      logger::log_appender(logger::appender_file(log_file))
    ),
    summaries = recipe(
      .time = time,
      .simulation = simulation_id,
      .replicate = replicate_id,
      .output_path = .output_path,
      outcomes = tibble::tibble(
        time = time,
        n_susceptible = n_susceptible,
        n_infections = n_infections, n_infected = n_infected,
        n_deaths = n_deaths, n_dead = n_dead,
        n_recoveries = n_recoveries, n_recovered = n_recovered,
        p_infection = p_infection)
    )
  )

  sim = simulation(theta, initialization_steps, simulation_steps, basic_writer)

})



