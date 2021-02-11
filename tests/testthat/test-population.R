


testthat::test_that("A population can be created directly with no state.", {
  pop_1 = simulator:::population$new(time = 0, group = "default")
  testthat::expect_equal(class(pop_1), c("population", "R6"))
  testthat::expect_equal(pop_1$n_units, 0)
  testthat::expect_equal(pop_1$n_covariates, 0)
  testthat::expect_equal(pop_1$n_states, 0)
  testthat::expect_equal(pop_1$id, integer())
  testthat::expect_equal(pop_1$time, 0)
  testthat::expect_equal(pop_1$group, 'default')
  testthat::expect_equal(pop_1$states, character())
  testthat::expect_equal(pop_1$covariates, character())
})


testthat::test_that("A population can be created directly with covariates only.", {
  N = 20
  data = list(zz = rnorm(N), qq = rnorm(N), bobo = letters[1:N])
  pop_1 = simulator:::population$new(time = 0, group = "default", data = data, 
    .counter = simulator:::get_counter(name = 'another-counter'))
  testthat::expect_equal(class(pop_1), c("population", "R6"))
  testthat::expect_equal(pop_1$n_units, N)
  testthat::expect_equal(pop_1$n_covariates, 3)
  testthat::expect_equal(pop_1$n_states, 0)
  testthat::expect_equal(pop_1$id, 1:N)
  testthat::expect_equal(pop_1$time, 0)
  testthat::expect_equal(pop_1$group, 'default')
  testthat::expect_equal(pop_1$states, character())
  testthat::expect_equal(pop_1$get('zz'), data$zz)
  testthat::expect_equal(pop_1$get('qq'), data$qq)
  testthat::expect_equal(pop_1$get('bobo'), data$bobo)
})

testthat::test_that("A population can be created directly with covariates only.", {
  N = 6
  data = list(zs = state$new(letters[1:(1 + N - 1)], labels = letters[1:20]),
              qs = state$new(LETTERS[9:(9 + N - 1)], labels = LETTERS))
  pop_1 = simulator:::population$new(time = 3, group = "default", data = data, 
    .counter = simulator:::get_counter(name = 'yet-another-counter'))
  testthat::expect_equal(class(pop_1), c("population", "R6"))
  testthat::expect_equal(pop_1$n_units, N)
  testthat::expect_equal(pop_1$n_covariates, 0)
  testthat::expect_equal(pop_1$n_states, 2)
  testthat::expect_equal(pop_1$id, 1:N)
  testthat::expect_equal(pop_1$time, 3)
  testthat::expect_equal(pop_1$group, 'default')
  testthat::expect_true('zs' %in% pop_1$states)
  testthat::expect_true('qs' %in% pop_1$states)
  testthat::expect_equal(pop_1$get('zs'), data$zs)
  testthat::expect_equal(pop_1$get('qs'), data$qs)
})

testthat::test_that("A population can be created directly with states and covariates.", {
  N = 20
  data = list(zs = state$new(letters[1:(1 + N - 1)], labels = letters[1:N]),
              qs = state$new(LETTERS[7:(7 + N - 1)], labels = LETTERS),
              zz = rnorm(N), qq = rnorm(N), bobo = letters[1:N])
  pop_1 = simulator:::population$new(time = 3, group = "other-group", data = data, 
    .counter = simulator:::get_counter(name = 'still-yet-another-counter'))
  testthat::expect_equal(class(pop_1), c("population", "R6"))
  testthat::expect_equal(pop_1$n_units, N)
  testthat::expect_equal(pop_1$n_covariates, 3)
  testthat::expect_equal(pop_1$n_states, 2)
  testthat::expect_equal(pop_1$id, 1:N)
  testthat::expect_equal(pop_1$time, 3)
  testthat::expect_equal(pop_1$group, 'other-group')
  testthat::expect_true('zs' %in% pop_1$states)
  testthat::expect_true('qs' %in% pop_1$states)
  testthat::expect_true('qq' %in% pop_1$covariates)
  testthat::expect_true('zz' %in% pop_1$covariates)
  testthat::expect_true('bobo' %in% pop_1$covariates)
  testthat::expect_equal(pop_1$get('zs'), data$zs)
  testthat::expect_equal(pop_1$get('qs'), data$qs)
  testthat::expect_equal(pop_1$get('zz'), data$zz)
  testthat::expect_equal(pop_1$get('qq'), data$qq)
  testthat::expect_equal(pop_1$get('bobo'), data$bobo)
})

testthat::test_that("A population has working get/set.", {
  N = 20
  data = list(zs = state$new(letters[1:(1 + N - 1)], labels = letters[1:N]),
              qs = state$new(LETTERS[7:(7 + N - 1)], labels = LETTERS),
              zz = rnorm(N), qq = rnorm(N), bobo = letters[1:N])
  id_start = simulator:::get_counter()$current + 1
  pop_1 = simulator:::population$new(time = 3, group = "other-group", data = data)
  testthat::expect_equal(class(pop_1), c("population", "R6"))
  testthat::expect_equal(pop_1$get('qs'), data$qs)
  testthat::expect_equal(pop_1$get('zz'), data$zz)
  pop_1$set('zz', pop_1$get('qs'))
  testthat::expect_equal(pop_1$get('zz'), data$qs)
  testthat::expect_true('zz' %in% pop_1$states)
  testthat::expect_equal(pop_1$n_units, N)
  testthat::expect_equal(pop_1$n_covariates, 2)
  testthat::expect_equal(pop_1$n_states, 3)
  testthat::expect_equal(pop_1$id, id_start:simulator::get_counter()$current)
  testthat::expect_equal(pop_1$time, 3)
  testthat::expect_equal(pop_1$group, 'other-group')
})

testthat::test_that("A pair of populations can be merged.", {
  N = 20
  data_1 = list(zs = state$new(letters[1:(1 + N - 1)], labels = letters[1:N]),
              qs = state$new(LETTERS[7:(7 + N - 1)], labels = LETTERS),
              zz = rnorm(N), qq = rnorm(N), bobo = letters[1:N])
  M = 5
  data_2 = list(zs = state$new(letters[1:(1 + M - 1)], labels = letters[1:M]),
              qs = state$new(LETTERS[7:(7 + M - 1)], labels = LETTERS),
              zz = rnorm(M), qq = rnorm(M), bobo = letters[1:M])
  id_start = simulator:::get_counter()$current + 1
  pop_1 = simulator:::population$new(time = 3, group = "other-group", data = data_1)
  pop_2 = simulator:::population$new(time = 3, group = "some-group", data = data_2)
  id_stop = simulator:::get_counter()$current

  testthat::expect_equal(class(pop_1), c("population", "R6"))
  testthat::expect_equal(class(pop_2), c("population", "R6"))
  
  testthat::expect_equal(pop_1$n_units, N)
  testthat::expect_equal(pop_1$n_covariates, 3)
  testthat::expect_equal(pop_1$n_states, 2)
  ids_pop_1 = pop_1$id

  testthat::expect_equal(pop_2$n_units, M)
  testthat::expect_equal(pop_2$n_covariates, 3)
  testthat::expect_equal(pop_2$n_states, 2)
  ids_pop_2 = pop_2$id

  pop_3 = pop_1$absorb(pop_2)
  testthat::expect_equal(pop_3$n_units, N + M)
  testthat::expect_equal(pop_1$n_units, N + M)
  testthat::expect_equal(pop_2$n_units, M)
  testthat::expect_equal(pop_3$id, c(ids_pop_1, ids_pop_2))
  testthat::expect_equal(pop_3$get('zs')$state, c(data_1$zs$state, data_2$zs$state))
  testthat::expect_equal(pop_3$get('qs')$state, c(data_1$qs$state, data_2$qs$state))
  testthat::expect_equal(pop_3$get('bobo'), c(data_1$bobo, data_2$bobo))
  testthat::expect_equal(pop_3$get('zz'), c(data_1$zz, data_2$zz))
  testthat::expect_equal(pop_3$get('qq'), c(data_1$qq, data_2$qq))

})


testthat::test_that("A pair of populations can be merged and then drop rows/levels.", {
  N = 20
  data_1 = list(zs = state$new(letters[1:(1 + N - 1)], labels = letters[1:N]),
              qs = state$new(LETTERS[7:(7 + N - 1)], labels = LETTERS),
              zz = rnorm(N), qq = rnorm(N), bobo = letters[1:N])
  M = 5
  data_2 = list(zs = state$new(letters[1:(1 + M - 1)], labels = letters[1:M]),
              qs = state$new(LETTERS[7:(7 + M - 1)], labels = LETTERS),
              zz = rnorm(M), qq = rnorm(M), bobo = letters[1:M])
  id_start = simulator:::get_counter()$current + 1
  pop_1 = simulator:::population$new(time = 3, group = "other-group", data = data_1)
  ids_pop_1 = pop_1$id
  pop_2 = simulator:::population$new(time = 3, group = "some-group", data = data_2)
  ids_pop_2 = pop_2$id
  id_stop = simulator:::get_counter()$current

  dropped_rows = 7:10
  pop_3 = pop_1$absorb(pop_2)
  pop_3$drop(dropped_rows)
  pop_4 = simulator:::population$new(time = 3, group = 'a-group', data = list(
    zs = state$new(c(data_1$zs$state, data_2$zs$state)[-dropped_rows]),
    qs = state$new(c(data_1$qs$state, data_2$qs$state)[-dropped_rows]), 
    bobo = c(data_1$bobo, data_2$bobo)[-dropped_rows],
    zz = c(data_1$zz, data_2$zz)[-dropped_rows],
    qq = c(data_1$qq, data_2$qq)[-dropped_rows]
  ))
  testthat::expect_equal(pop_3$n_units, N + M - length(dropped_rows))
  testthat::expect_equal(pop_3$id, c(ids_pop_1, ids_pop_2)[-dropped_rows])
  testthat::expect_equal(pop_3$get('zs')$state, pop_4$get('zs')$state)
  testthat::expect_equal(pop_3$get('qs')$state, pop_4$get('qs')$state)
  testthat::expect_equal(pop_3$get('bobo'), pop_4$get('bobo'))
  testthat::expect_equal(pop_3$get('qq'), pop_4$get('qq'))
  testthat::expect_equal(pop_3$get('zz'), pop_4$get('zz'))
  idx_lgl_a = pop_3$matches(qs$state %in% c('J', 'K'))
  idx_a = pop_3$which(qs$state %in% c('J', 'K'))
  testthat::expect_equal(idx_a, which(idx_lgl_a))
  idx_lgl_b = pop_3$matches(zs$state %in% c('j', 'k'))
  idx_b = pop_3$which(zs$state %in% c('j', 'k'))
  testthat::expect_equal(idx_b, which(idx_lgl_b))
  pop_3$drop(levels = c('j', 'k', 'J', 'K'))
  dropped_rows = c(idx_a, idx_b) %>% unique
  testthat::expect_equal(pop_3$n_units, pop_4$n_units - length(dropped_rows))
  testthat::expect_equal(pop_3$get('zs')$state, pop_4$get('zs')$state[-dropped_rows])
  testthat::expect_equal(pop_3$get('qs')$state, pop_4$get('qs')$state[-dropped_rows])
  testthat::expect_equal(pop_3$get('bobo'), pop_4$get('bobo')[-dropped_rows])
  testthat::expect_equal(pop_3$get('qq'), pop_4$get('qq')[-dropped_rows])
  testthat::expect_equal(pop_3$get('zz'), pop_4$get('zz')[-dropped_rows])
})

testthat::test_that("A populations can bleb, then merge to re-form", {
  N = 20
  data_1 = list(zs = state$new(letters[1:(1 + N - 1)], labels = letters[1:N]),
              qs = state$new(LETTERS[7:(7 + N - 1)], labels = LETTERS),
              zz = rnorm(N), qq = rnorm(N), bobo = letters[1:N])
  id_start = simulator:::get_counter()$current + 1
  pop_1 = simulator:::population$new(time = 3, group = "other-group", data = data_1)
  ids_pop_1 = pop_1$id
  id_stop = simulator:::get_counter()$current
  pop_2 = pop_1$which(qs$state %in% c('G', 'H', 'J', 'K', 'O')) %>% pop_1$bleb()
  testthat::expect_true(all(pop_2$id %in% id_start:id_stop))
  testthat::expect_true(all(pop_1$id %in% id_start:id_stop))
  testthat::expect_false(any(pop_1$id %in% pop_2$id))
  tib_1 = pop_2$absorb(pop_1)$tibble %>% dplyr::arrange(id)
  tib_2 = tibble::tibble(
    zs = data_1$zs$state, qs = data_1$qs$state, zz = data_1$zz, qq = data_1$qq, bobo = data_1$bobo)
  testthat::expect_equal(sort(tib_1$zz), sort(data_1$zz))
  testthat::expect_equal(sort(tib_1$qs), sort(data_1$qs$state))
  testthat::expect_equal(sort(tib_1$qq), sort(data_1$qq))
  testthat::expect_equal(sort(tib_1$zs), sort(data_1$zs$state))
  testthat::expect_equal(sort(tib_1$bobo), sort(data_1$bobo))
})

testthat::test_that("Population match functionality works.", {
  N = 20
  data_1 = list(zs = state$new(letters[1:(1 + N - 1)], labels = letters[1:N]),
              qs = state$new(LETTERS[7:(7 + N - 1)], labels = LETTERS),
              zz = rnorm(N), qq = rnorm(N), bobo = letters[1:N])
  id_start = simulator:::get_counter()$current + 1
  pop_1 = simulator:::population$new(time = 3, group = "other-group", data = data_1)
  ids_pop_1 = pop_1$id
  id_stop = simulator:::get_counter()$current
  data_1_matches = data_1$qs$state %in% LETTERS[9:12] &
                   data_1$zz < 0
  pop_1_matches_a = pop_1$matches(qs$state %in% LETTERS[9:12], zz < 0)
  pop_1_matches_b = pop_1$matches(qs$state %in% LETTERS[9:12] & zz < 0)
  testthat::expect_equal(data_1_matches, pop_1_matches_a)
  testthat::expect_equal(data_1_matches, pop_1_matches_b)
})


testthat::test_that("Population mutate/summarize functionality works.", {
  N = 20
  data_1 = list(zs = state$new(letters[1:(1 + N - 1)], labels = letters[1:N]),
              qs = state$new(LETTERS[7:(7 + N - 1)], labels = LETTERS),
              zz = rnorm(N), qq = rnorm(N), bobo = letters[1:N])
  id_start = simulator:::get_counter()$current + 1
  pop_1 = simulator:::population$new(time = 3, group = "other-group", data = data_1)
  ids_pop_1 = pop_1$id
  id_stop = simulator:::get_counter()$current
  data_1_matches = data_1$qq < 0 & data_1$zz < 0
  pop_1_matches_a = pop_1$mutate(qqzz = qq < 0 & zz < 0)$matches(qqzz)
  pop_1_matches_b = pop_1$matches(qq < 0, zz < 0)
  pop_1_summ = pop_1$summarize(qqzz = qq < 0 & zz < 0, qss = qs$state, .return_type = 'list') %>%
    purrr::lift_dl(tibble::tibble)()
  testthat::expect_equal(data_1_matches, pop_1_matches_a)
  testthat::expect_equal(data_1_matches, pop_1_matches_b)
  testthat::expect_equal(data_1_matches, pop_1_summ$qqzz)
  testthat::expect_equal(data_1$qs$state, pop_1_summ$qss)
})



