

testthat::test_that("Various inputs can be padded.", {
  s1 = pad("3", 0); s2 = pad("3", -1)
  testthat::expect_equal(s1, s2)
  s3 = pad(letters, 22)
  testthat::expect_equal(22, purrr::map_int(s3, nchar) %>% unique())
  s4 = pad("333", 22)
  s5 = pad("1", 22)
  testthat::expect_equal(nchar(s4), nchar(s5))
})


testthat::test_that("A vector of indexes can be converted to a logical representation.", {
  N = 10
  K = rbinom(n = N, size = 200, prob = 0.5)
  for (i in seq_along(K)) {
    v1_idx = sample(x = 1:K[i], size = 20, replace = FALSE)
    v1_lgl = tag_picks(v1_idx, K[i])
    testthat::expect_equal(length(v1_idx), sum(v1_lgl))
  }
})

testthat::test_that("A mix of states can be generated.", {
  N = 10
  K = rbinom(n = N, size = 100, prob = 0.5)
  n = sample.int(n = length(letters), size = N, replace = TRUE)
  for (i in seq_along(K)) {
    p = sample.int(n = 100, size = n[i], replace = TRUE)
    names(p) = letters[1:n[i]]
    m1 = mix_generator(n = K[i], mix = p)
    testthat::expect_true(all(m1$state %in% letters[1:n[i]]))
  }
})
