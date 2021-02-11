

testthat::test_that("A counter can be created and incremented.", {
  c1 = ctr$new(i = 0)
  testthat::expect_equal(c1$current, 0)
  testthat::expect_equal(c1$increment, 1)
  testthat::expect_equal(c1$current, 1)
  testthat::expect_equal(c1$increment, 2)
  testthat::expect_equal(c1$current, 2)
  c2 = ctr$new(i = 5)
  testthat::expect_equal(c2$current, 5)
  testthat::expect_equal(c2$increment, 6)
  testthat::expect_equal(c2$current, 6)
  testthat::expect_equal(c2$increment, 7)
  testthat::expect_equal(c2$current, 7)
})

testthat::test_that("Two counters can be created and share state.", {
  c1 = get_counter()
  testthat::expect_equal(c1$current, 0)
  testthat::expect_equal(c1$increment, 1)
  testthat::expect_equal(c1$current, 1)
  testthat::expect_equal(c1$increment, 2)
  testthat::expect_equal(c1$current, 2)
  c2 = get_counter()
  testthat::expect_equal(c2$current, 2)
  testthat::expect_equal(c2$increment, 3)
  testthat::expect_equal(c1$current, 3)
  testthat::expect_equal(c2$increment, 4)
  testthat::expect_equal(c1$current, 4)
  c3 = get_counter(name = 'zorg')
  testthat::expect_equal(c3$current, 0)
})

