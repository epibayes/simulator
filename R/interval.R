library(R6)

an_Interval <- R6Class("an_Interval",
                       public = list(
                         upper = 0,
                         lower = 0,
                         intvl = 0,
                         counter = 0,
                         initialize = function(upper, lower, intvl, counter){
                           self$upper <- upper
                           self$lower <- lower
                           self$intvl <- intvl
                           self$counter <- counter
                         },
                         increment_counter = function(){
                           self$counter <- self$counter + 1
                         }
                       ))
