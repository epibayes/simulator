# step 3: grab uniques
# result is a list of 7
# staff_prevalence, a num
# intake_prevalence, a set of 3 num
# silent rate, a set of 3 num
# r0, a set of 3 num
# mean_latent_duration, a set of 3 num
# mean_disease_duration, a set of 3 num
# test_sensitivity, a list of 4 sets of 3 num
testthat::test_that("Test that purr maps uniques correctly?",{
})

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
testthat::test_that("Test that tibbles are expanded correctly?",{
})

# step 6: unnest sensitivity
# result is 19683 obs. of 10 variables
# staff_prevalence is a set of 19683 num
# intake_prevalence is a set of 19683 num
# silent_rate is a set of 19683 num
# r0 is a set of 19683 num
# mean_latent_duration is a set of 19683 num
# mean_disease_duration is a set of 19683 num
# var1, var2, var3, and var4 are sets of 19683 num
testthat::test_that("Test that things are unnested?",{
})


#final: theta is a large list of 19683 elements. each element is a list of 5 environments?
testthat::test_that("Test that an element is made of 5 lists?",{
})

