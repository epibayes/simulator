

#' Container for simulation state, includes states and covariates, supports limited (but 
#' fast) operations required for simulation
#' 
#' @description
#' Supported operations: 
#'
#' @details
#' Methods that evaluate a dotlist of expressions (matches, mutate, etc...) are
#' evaluated with a data mask constructed from the data enviornment, and the
#' listed quosures are individually evaluated in the context of that data mask.
#' For mutate, this means that expressions that create new data items can be
#' evaluated sequentially.
#'
#' @export
population = R6::R6Class(
  classname = "population",
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #'
    #' @param time current time for population
    #' @param group population group identifier
    #' @param data list of states and covariates, states will be deep-copied to
    #'        avoid weird reference semantics in common usage.
    #' @param .env environment where the state will be stored (dev-facing
    #'        hook)
    #' @param .counter object that can generate unique ids (dev-facing hook)
    #' @return population object
    initialize = function(
      time, 
      group, 
      data = list(),
      .env = rlang::env(),
      .counter = simulator:::get_counter()
    ) {
      private$.counter = .counter
      private$.data = .env
      private$.time = time
      private$.birth_time = time
      private$.group = group
      rlang::env_bind(private$.data, !!!data)
      for (state in self$states) {
        s = self$get(state)$clone(deep = TRUE)
        rlang::env_bind(private$.data, !!state := s)
      }
      private$.n_units = private$.count_units(private$.data)
      private$.id = seq_len(self$n_units) %>% 
        purrr::map_int(~ private$.counter$incr())
    },
    #' @description
    #' Get the value of the named population variable for all individuals
    #' (rows)
    #' @param name data member name to retrieve
    #' @return specified data member
    get = function(name) {
      o = rlang::env_get(private$.data, name, inherit = FALSE)
      return(o)
    },
    #' @description
    #' Set the value of the named population variable for all individuals
    #' (rows)
    #' @param name data member name to set
    #' @param value value to set the member to
    #' @return self, modified
    set = function(name, value) {
      rlang::env_bind(private$.data, !!name := value)
      return(self)
    },
    #' @description
    #' Merge another population into this one, times must be matched and the
    #' group of the other population will be over-written.  Data must match
    #' between populations (other will be appended).
    #'
    #'@param x other populatino to absorb into this one
    #'@return self, with other oppended
    absorb = function(x) {
      if (self$time != x$time) {
        stop("Times of populations must match to absorb.")
      }
      if (x$n_units == 0) {
        return(self)
      } else if (self$n_units == 0) {
        self = x
        return(x)
      }
      private$.match_components(x)
      for (sn in self$states) {
        self$get(sn)$bind(x$get(sn))
      }
      for (cov in self$covariates) {
        self$set(cov, c(self$get(cov), x$get(cov)))
      }
      private$.id = c(self$id, x$id)
      private$.n_units = private$.count_units(private$.data)
      return(self)
    },
    #' @description
    #' Separate a set of rows specified by rows or levels
    #' @param rows vector of row indexes to drop
    #' @param levels vector of levels (of any state) to drop
    #' @return copy of self, with only specified rows
    bleb = function(rows, levels) {
      if (missing(rows) && !missing(levels)) {
        return(private$.bleb_levels(levels))
      } else if (!missing(rows)) {
        return(private$.bleb_rows(rows))
      } else if (missing(rows) && missing(levels)) {
        return(private$.bleb_rows(numeric()))
      }
      stop("One and only one of 'rows' and 'levels' must be specified'")
    },
    #' @description 
    #' Drop specified rows or levels
    #' @param rows vector of row indexes to drop
    #' @param levels vector of levels (of any state) to drop
    #' @return self, without specified rows
    drop = function(rows, levels) {
      if (self$n_units == 0) {
        return(self)
      }
      if (missing(rows) && !missing(levels)) {
        return(private$.drop_levels(levels))
      } else if (!missing(rows)) {
        return(private$.drop_rows(rows))
      } else if (missing(rows) && missing(levels)) {
        return(self)
      }
      stop("One and only one of 'rows' and 'levels' must be specified'")
    },
    #' @description
    #' Return boolean vector for rows matching the expression
    #' @param ... expressions
    #' @return self boolean vector with one value per row
    matches = function(...) {
      if (self$n_units == 0 || self$n_states == 0) {
        return(logical(length = 0))
      }
      private$.sync()
      env = rlang::child_env(private$.data)
      rlang::env_bind(env, time = rep(private$.time, self$n_units))
      rlang::env_bind(env, group = rep(private$.group, self$n_units))
      rlang::env_bind(env, id = private$.id)
      dm = rlang::new_data_mask(bottom = env, top = private$.data)
      test = rlang::enquos(...)
      matched = purrr::map(test, rlang::eval_tidy, data = dm) %>%
        purrr::pmap(~ isTRUE(all(...))) %>% 
        purrr::map_lgl( ~ .x)
      return(matched)
    },
    #' @description
    #' Add covariates or states from a series of evaluated expressions
    #' @param ... expressions
    #' @return self, for chaining
    mutate = function(...) {
      if (self$n_units == 0 || self$n_states == 0) {
        return(self)
      }
      private$.sync()
      dm = rlang::new_data_mask(bottom = private$.data, top = private$.data)
      exprs = rlang::enquos(...)
      for (target in names(exprs)) {
        o = try(rlang::eval_tidy(expr = exprs[[target]], data = dm))
        if ('try-error' %in% class(o)) {
          error_expr = exprs[[target]]
          error_env = private$.data
          rlang::abort("Expression could not be evaluated.", "mutate-expression-failure") 
        }
        rlang::env_bind(.env = private$.data, !!target := o)
      }
      return(self)
    },
    #' @description
    #' Evaluate an expression in the context of the population and return the
    #' result
    #' @param ... expressions
    #' @param .return_type how to return evaluated expressions (use a list if
    #'        the value is not 'env'
    #' @return list or enviornment with evaluated expressions
    summarize = function(..., .return_type = 'env') {
      if (self$n_units == 0 || self$n_states == 0) {
        if (isTRUE(.return_type == 'env')) {
          return(rlang::child_env(private$.data))
        } else if (.return_type == 'tibble') {
          return(tibble::tibble())
        } else {
          return(list())
        }
      }
      private$.sync()
      env = rlang::child_env(private$.data)
      dm = rlang::new_data_mask(bottom = env, top = private$.data)
      exprs = rlang::enquos(...)
      for (target in names(exprs)) {
        o = rlang::eval_tidy(expr = exprs[[target]], data = dm)
        rlang::env_bind(.env = env, !!target := o)
      }
      if (.return_type == 'env') {
        return(env)
      } else if (.return_type == 'tibble') {
        return(purrr::lift_dl(tibble::tibble)(as.list(env)))
      } else {
        return(as.list(env))
      }
    },
    #' @description
    #' Return indexes of rows matching the expression
    #' @param ... expressions
    #' @return vector of indexes
    which = function(...) {
      private$.sync()
      idx = self$matches(...) %>% which()
      return(idx)
    }
  ),
  private = list(
    .time = 0,
    .birth_time = 0,
    .n_units = 0,
    .n_states = 0,
    .counter = simulator:::get_counter(),
    .id = integer(),
    .group = "default",
    .data = rlang::env(),
    .modifiers = list(),
    .match_components = function(other) { 
      if(!all(self$states %in% other$states)) {
        stop("Some local states are not contained in other object.")
      }
      if(!all(other$states %in% other$states)) {
        stop("Some other states are not contained in local object.")
      }
      if(!all(self$covariates %in% other$covariates)) {
        stop("Some local covariates are not contained in other object.")
      }
      if(!all(other$covariates %in% other$covariates)) {
        stop("Some other covariates are not contained in local object.")
      }
    },
    .bleb_levels = function(levels) {
      idx = integer()
      for (state in names(levels)) {
        idx = c(idx, self$get(levels[[state]])$which(levels[[state]]))
      }
      return(private$.bleb_rows(idx))
    },
    .bleb_rows = function(rows) {
      other = self$clone(deep = TRUE)
      local_rows = seq_len(private$.n_units) %>% purrr::discard( ~ .x %in% rows)
      other$drop(local_rows)
      self$drop(rows)
      return(other)
    },
    .drop_levels = function(levels) {
      idx = integer()
      for (state in self$states) {
        idx = c(idx, self$get(state)$which(levels))
      }
      return(private$.drop_rows(idx))
    },
    .drop_rows = function(rows) {
      if (length(rows) == 0) {
        return(self)
      }
      for (state in self$states) {
        self$get(state)$drop(rows)
      }
      for (cov in self$covariates) {
        rlang::env_bind(private$.data, !!cov := self$get(cov)[-rows])
      }
      private$.id = private$.id[-rows]
      private$.n_units = private$.count_units(private$.data)
      return(self)
    },
    .sync = function() {
      rlang::env_bind(private$.data, time = private$.time)
      rlang::env_bind(private$.data, group = private$.group)
      rlang::env_bind(private$.data, id = private$.id)
    },
    deep_clone = function(name, value) {
      if (name == '.data') {
        value = rlang::env_clone(private$.data)
        for (state in ls(value)) {
          s = rlang::env_get(value, state)
          if ('state' %in% class(s)) {
            rlang::env_bind(private$.data, !!state := s$clone(deep = TRUE))
          }
        }
      }
      return(value)
    },
    .get_state_names = function(data) {
      name_vec = ls(data)
      match = purrr::map(name_vec, ~ class(rlang::env_get(data, .x))) %>%
        purrr::map_lgl( ~  any('state' %in% .x))
      state_name_vec = name_vec[match]
      state_name_vec = state_name_vec[state_name_vec != 'id']
      state_name_vec = state_name_vec[state_name_vec != 'group']
      state_name_vec = state_name_vec[state_name_vec != 'time']
      return(state_name_vec)
    },
    .get_covariate_names = function(data) {
      name_vec = ls(data)[!(ls(data) %in% c('time', 'group'))]
      match = purrr::map(name_vec, ~ class(rlang::env_get(data, .x))) %>%
        purrr::map_lgl( ~ !any('state' %in% .x))
      cov_name_vec = name_vec[match]
      cov_name_vec = cov_name_vec[cov_name_vec != 'id']
      cov_name_vec = cov_name_vec[cov_name_vec != 'group']
      cov_name_vec = cov_name_vec[cov_name_vec != 'time']
      return(cov_name_vec)
    },
    .count_units = function(data) {
      n_units = integer()
      for (cov in self$covariates) {
        n_units = c(n_units, length(rlang::env_get(data, cov)))
      }
      for (state in self$states) {
        n_units = c(n_units, rlang::env_get(data, state)$n_units)
      }
      n_units = unique(n_units)
      if (length(n_units) == 0) {
        n_units = 0
      }
      stopifnot(length(n_units) == 1)
      private$.n_units = n_units
      return(n_units)
    }
  ),
  active = list(
    #' @field id ids for all members/units
    id = function() private$.id,
    #' @field time current time step, or set current time step
    time = function(x) {
      if (!missing(x)) {
        private$.time = x
      }
      return(private$.time)
    },
    #' @field age population age
    age = function() self$time - private$.birth_time,
    #' @field group population group
    group = function() private$.group,
    #' @field states names of population states
    states = function() private$.get_state_names(private$.data),
    #' @field covariates names of population covariates 
    covariates = function() private$.get_covariate_names(private$.data),
    #' @field n_states number of states in the population
    n_states = function() length(self$states),
    #' @field n_covariates number of covariates in the population
    n_covariates = function() length(self$covariates),
    #' @field n_units number of units (rows) in the population
    n_units = function() private$.count_units(private$.data),
    #' @field list return state as a list
    list = function() {
      private$.sync()
      states = self$states
      covs = self$covs
      states = self$states %>% 
        purrr::map(~ self$get(.x)$state) %>% 
        rlang::set_names(self$states)
      covs = self$covariates %>%
        purrr::map(~ self$get(.x)) %>%
        rlang::set_names(self$covariates)
      lis = list(
        core = list(id = self$id, time = self$time, group = self$group),
        states = states, covs = covs)
      return(lis)
    },
    #' @field tibble returns state as a tibble
    tibble = function() {
      private$.sync()
      states = self$states
      covs = self$covs
      states = self$states %>% 
        purrr::map(~ self$get(.x)$state) %>% 
        rlang::set_names(self$states) %>%
        purrr::lift_dl(tibble::tibble)()
      covs = self$covariates %>%
        purrr::map(~ self$get(.x)) %>%
        rlang::set_names(self$covariates) %>%
        purrr::lift_dl(tibble::tibble)()
      tib = tibble::tibble(
        id = self$id, time = self$time, group = self$group
      ) %>% dplyr::bind_cols(states, covs)
      return(tib)
    }
  )
)


