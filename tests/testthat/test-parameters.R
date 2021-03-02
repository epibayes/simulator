library(dplyr)
library(tidyverse)


## test parameter_range function
testthat:: test_that("test parameter_range function",{
  #######
  #arbitrary vectors and numbers we will use to see if 
  #parameter_range works the way we want it to
  x1 = c(0)
  x2 = c(1,2,4,8,20)
  n1 = 3
  n2 = 5
  n3 = 10
  ########
  
  #if the length of the list passed to parameter_range is 1, the result
  #of parameter_range is just the contents of the list repeated n times
  testthat::expect_equal(parameter_range(x1,n1), c(0,0,0))
  testthat::expect_equal(parameter_range(x1,n2), c(0,0,0,0,0))

  #if n is smaller than x, just return x
  testthat::expect_equal(length(parameter_range(x2,n1)), length(x2))
  testthat::expect_equal(parameter_range(x2,n1), x2)
  
  #if n is larger than x, return a list of n values (including the values in x)
  testthat::expect_equal(length(parameter_range(x2,n3)), 10)

  #testing that all numbers fall between the smallest and largest numbers
  testthat::expect_true(all(between(parameter_range(x2,n3), 1, 20)))
  
})

testthat::test_that("test parameter grid expansion function", {
  ##########
  ##lists of lists used for testing purposes
  small_n_ls = list(c(1,2,3), c(4,5,6))
  small_ch_ls = list(c("1","2","3"), c("4","5","6"))
  small_ls_ls = list(list(1,2,3),list(3,4,5))
  large_n_ls = list(c(1,2,3,4), c(5,6,7,8), c(9,10,11,12), c(13,14,15,16))
  small_dbl_ls = list(c(1.1,2.2,3.3), c(4.4,5.5,6,6))
  ##########
  ##########
  ##grid for small_n_ls
  sm_lsts = tibble(
    Var1 = c(1,2,3),
    Var2 = c(4,5,6)
    
  )
  sm_tibb = sm_lsts %>% expand(Var1, Var2) %>%
    arrange(Var2)

  ##grid for small_dbl_ls
  sm_dbls = tibble(
    Var1 = c(1.1,2.2,3.3),
    Var2 = c(4.4,5.5,6.6)
  )
  sm_dbls = sm_dbls %>% expand(Var1,Var2) %>% 
    arrange(Var2)
  ##########
  
  
  #test that the results are tibbles
  testthat::expect_s3_class(expand_parameter_grid(small_n_ls), "tbl")
  testthat::expect_s3_class(expand_parameter_grid(small_ch_ls),"tbl")
  testthat::expect_s3_class(expand_parameter_grid(small_dbl_ls),"tbl")
  
  #test that tibbles of the correct size are created when given a list of number vectors
  testthat::expect_vector(expand_parameter_grid(small_n_ls),size=9)
  testthat::expect_vector(expand_parameter_grid(small_ch_ls),size=9)
  testthat::expect_vector(expand_parameter_grid(small_ls_ls), size=0)
  testthat::expect_vector(expand_parameter_grid(large_n_ls),size=256)
  
  #test that expand_parameter_grid result is the same as hard-coded tibble
  testthat::expect_setequal(expand_parameter_grid(small_n_ls)[[1]], sm_tibb[[1]])
  testthat::expect_setequal(expand_parameter_grid(small_n_ls[[2]]), sm_tibb[[2]])
  testthat::expect_setequal(expand_parameter_grid(small_dbl_ls)[[1]], sm_dbls[[1]])
  testthat::expect_setequal(expand_parameter_grid(small_dbl_ls)[[2]], sm_dbls[[2]])
  
})