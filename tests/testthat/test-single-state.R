testthat::test_that("The same number of entries are present in the state and index", {
  o = state$new(letters[1:3])
  testthat::expect_equal(sum(o$matrix), purrr::map_int(o$index, length) %>% sum())
})

testthat::test_that("A state can have more labels than currently present states.", {
  o = state$new(x = letters[1:3], labels = letters)
  testthat::expect_equal(o$labels, letters)
  testthat::expect_equal(o$n_units, 3)
})

testthat::test_that("The same number of entries are present in the state and index after drop/append", {
  o = state$new(letters[1:10])
  o$drop(levels = c('b', 'c'))
  o$append(letters[14:16])
  testthat::expect_equal(sum(o$matrix), purrr::map_int(o$index, length) %>% sum())
})

testthat::test_that("Bleb operation can be reversed with bind operation.", {
  s = c(letters, letters)
  o = state$new(s)
  o_bleb = o$bleb(levels = c('a','b','c','d'))
  o_old = o
  o_new = o$bind(o_bleb)
  testthat::expect_true(identical(o_old, o_new))
  testthat::expect_equal(o_old$labels, o_new$labels)
  testthat::expect_equal(o$labels, o_bleb$labels)
})

testthat::test_that("State can be reconstructed.", {
  s = c(letters, letters, LETTERS)
  o = state$new(s)
  testthat::expect_equal(s, o$state)
})


testthat::test_that("State transitions can be implemented.", {
  s = c(letters, letters, LETTERS)
  o = state$new(s)
  testthat::expect_equal(s, o$state)
})

