
#' Write that records simulation state 
#'
#' @export
writer = R6::R6Class(
  classname = "writer",
  public = list(
    #' @description
    #' Create a writer...
    #' @return self
    initialize = function(setup = recipe(), summaries = recipe()) {
      private$.setup = setup
      private$.summaries = summaries
      private$.sink = rlang::new_environment(parent = private$.root)
    },
    #' @description
    #'
    #' Set up the writer for a specific task, the enviroment must provide
    #' all the specific parameters used
    setup = function(bottom, top) {
      private$.setup$execute(bottom, top, private$.root)
    },
    #' @description
    #'
    #' Record data from the environment after applying summaries
    record = function(bottom, top) {
      private$.summaries$execute(bottom, top, private$.sink)
      files = self$save()
      return(files)
    },
    #' @description
    #'
    #' Save recorded data...
    save = function() {
      files = character()
      var_names = ls(private$.sink)
      for (var_name in var_names) {
        q_var_name = rlang::parse_quo(x = var_name, env = private$.sink)
        q_extra = rlang::quos(root = .output_path, simulation = .simulation, 
                              replicate = .replicate, time = .time)
        purrr::map(q_extra, ~ rlang::quo_set_env(.x, private$.sink))
        f = save_rds(!!q_var_name, !!!q_extra)
        files = c(files, f)
      }
      return(files)
    },
    finalize = function(...) {}
  ),
  private = list(
    .setup = recipe(),
    .summaries = recipe(),
    .root = rlang::env(),
    .sink = rlang::env()
  ),
  active = list(
    env = function() private$.sink
  )
)




