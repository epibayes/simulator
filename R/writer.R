
#' Generate a writer
#'
#' @export
make_writer = function(setup = recipe(), summaries = recipe()) {
  w = writer$new(setup = setup, summaries = summaries)
  return(w)
}


#' Write that records simulation state 
#'
#' @export
writer = R6::R6Class(
  classname = "writer",
  public = list(
    #' @description
    #' Create a writer...
    #'
    #' @param setup recipe used to initialize the writer environment
    #' @param summaries recipe used to summarize simulation data at each step,
    #'             all named results will be saved. By default the recipe must
    #'             include special parameters .time, .simulation,
    #'             .replicate, and .output_path
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
    #'
    #' @param bottom bottom of simulation environment stack 
    #' @param top top of simulation environment stack 
    setup = function(bottom, top) {
      private$.setup$execute(bottom, top, private$.root)
    },
    #' @description
    #'
    #' Record data from the environment after applying summaries
    #'
    #' @param bottom bottom of simulation environment stack 
    #' @param top top of simulation environment stack 
    #' @return list of files that were used to record the data
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
        file_name = save_file(!!q_var_name, private$.sink$.simulation, 
                              private$.sink$.replicate, private$.sink$.time)
        file_path = fs::path(private$.sink$.output_path, file_name)
        val = rlang::env_get(private$.sink, nm = var_name)
        saveRDS(val, file = file_path)
        files = c(files, file_name)
      }
      return(files)
    },
    #' @description
    #'
    #' Method runs at the end of a simulation
    #'
    #' @param ... generic args, not implemented
    finalize = function(...) {}
  ),
  private = list(
    .setup = recipe(),
    .summaries = recipe(),
    .root = rlang::env(),
    .sink = rlang::env()
  ),
  active = list(
    #' @field env return the environment where data is copied prior to saving.
    env = function() private$.sink
  )
)



