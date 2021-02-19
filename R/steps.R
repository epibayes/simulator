
#' Records a set of steps to apply to a simulation later
#'
#' @export
steps = R6::R6Class(
  classname = "steps",
  public = list(
    #' @description 
    #' Record a list of steps 
    #' @param ... expressions to record (argument names are the created targets
    #' @return class storing the expressions
    initialize = function(...) {
      private$.steps = rlang::enquos(...)
    },
    #' @description
    #' Execute the stored steps in the pre-specified environment
    #' @param top one end of environment stack to execute in
    #' @param bottom other end of environment stack to execute in
    #' @param target environment to place recipe results in (if named).
    #' @return modified environment
    execute = function(
      bottom = rlang::env(), 
      top = bottom,
      target = bottom
    ) {
      good_parent = rlang::env_parent(top)
      dm = rlang::new_data_mask(bottom = bottom, top = top)
      target_names = names(private$.steps)
      for (i in seq_len(self$n_steps)) {
        var_name = target_names[i]
        result = rlang::eval_tidy(expr = private$.steps[[i]], data = dm)
        if (!is.null(var_name) && var_name != "") {
          rlang::env_bind(.env = bottom, !!var_name := result)
          if (!identical(bottom, target)) {
            rlang::env_bind(.env = target, !!var_name := result)
          }
        }
      }
      rlang::env_poke_parent(top, good_parent)
      return(bottom)
    }
  ),
  private = list(
    .steps = rlang::enquos()
  ),
  active = list(
    #' @field n_steps Number of steps in the recipe
    n_steps = function() length(private$.steps)
  )
)

#' Function for recording a set of program steps
#'
#' @param ... named expressions
#' @return a `steps` object
#'
#' @export
recipe = function(...) steps$new(...)


