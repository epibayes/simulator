

#' Counters that can be shared among objects.
#'
#' @export
ctr = R6::R6Class(classname = 'counter', 
  public = list(
    #' @description
    #' Create a counter
    #' @param i start counting at i
    initialize = function(i = 0L) {
      private$.counter = as.integer(i)
    },
    #' @description
    #' Increment the counter explicitly
    incr = function() {
      last = private$.counter
      private$.counter = private$.counter + 1L
      if (last == private$.counter) stop("counter failed.")
      return(private$.counter)
    }
  ),
  private = list(
    .counter = 0L
  ),
  active = list(
    #' @field increment return the next value from the counter 
    increment = function() self$incr(),
    #' @field current return the current counter value
    current = function() private$.counter
  )
)

#' Location where global counters are stored
#'
#' Each named counter is independent of all other counters with different names
#' @export
.simulator_counters = rlang::env()

#' Retrieve a global counter
#'
#' @param name name of the counter
#' @return a counter 
#' @export
get_counter = function(name = 'default_counter') {
  if (!rlang::env_has(simulator:::.simulator_counters, name)) {
    cntr = rlang::env_bind(simulator:::.simulator_counters, !!name := ctr$new())
  }
  cntr = rlang::env_get(simulator:::.simulator_counters, name) 
  return(cntr)
}

