


testthat::test_that("Steps can be used to apply expressions in an environment.", {
  s1 = steps$new(
    zorg = 33,
    bb = 47 + zorg, 
    qqq = paste(letters, zorg),
    krump = f(33)
  )
  e = rlang::env()
  e$f = function(x) paste(x^2, "ZOG")
  o = s1$execute(e)
  testthat::expect_equal(e$zorg, 33)
  testthat::expect_equal(e$bb, e$zorg + 47)
  testthat::expect_equal(e$qqq, paste(letters, 33))
  testthat::expect_equal(e$krump, paste(33^2, "ZOG"))
})



