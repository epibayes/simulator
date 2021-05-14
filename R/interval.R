library(R6)

Interval = R6Class("Interval",
                       public = list(
                         upper = 0,
                         lower = 0,
                         length = 0,
                         counter = 0,
                         initialize = function(upper, lower, length, counter = 0){
                           self$upper = upper
                           self$lower = lower
                           self$length = length
                           self$counter = counter
                         },
                         increment_counter = function(){
                           self$counter = self$counter + 1
                         }
                       ))
