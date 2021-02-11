

#' R6 Class representing a single state variable (or factor) that
#' implements a limited but reasonably fast set of operations
#'
#' @details The supported operations include
#'
#' ## Initialize
#'
#' Class is created from a state vector and (optionally) a set of 
#' labels that can extend the states present in the vector.
#'
#' ## Append
#'
#' The states are extended with a new state vector and (optionally)
#' a set of labels that can extend the states present in the vector.
#'
#' ## Bind
#'
#' The states are extended with another state class (or vector)
#'
#' ## Bleb
#'
#' The class splits off a set of rows (or levels) and returns those
#' while dropping them from the current object
#'
#' ## Drop 
#'
#' The rows (or levels) specified are dropped from the class
#'
#' ## Counts
#'
#' The count of each state known to the class present currently is returned
#'
#' ## Which
#'
#' The indexes in the state vector of a set of levels are returned
#'
#' @export
state = R6::R6Class(
  classname = "state",
  public = list(
    #' @description
    #' Create a new state container
    #' @param x state vector to create the container from
    #' @param labels set of state levels to include in addition to those
    #'        contained in the state vector.
    #' @return A new state object
    initialize = function(x = NULL, labels = NULL) {
      if (!is.null(x)) {
        self$append(x, labels)
      }
    },
    #' @description
    #' Append a state vector to the current object and expand known labels
    #' @param x state vector to append to the current representation
    #' @param labels additional state levels, potentially not present in
    #'        current state vectors.
    #' @return nothing useful
    append = function(x, labels = character()) {
      private$.vec = c(private$.vec, x)
      if (length(labels) == 0 && !is.null(levels(x))) {
        labels = levels(x)
      }
      private$.index = private$.update_idx(private$.index, private$.n_units, labels, x)
      private$.labels = names(private$.index)
      private$.n_units = private$.n_units + length(x)
      private$.n_states = length(private$.labels)
      private$.matrix = private$.index_to_matrix(private$.index, self$dims)
    },
    #' @description
    #' Append another state representation to this one
    #' @param state other state to append
    #' @return combined representation
    bind = function(state) {
      if ('state' %in% class(state)) {
        self$append(state$state, labels = state$labels)
      } else {
        self$append(state)
      }
      return(self)
    },
    #' @description
    #' Remove a set of rows (or state levels) from the current representation
    #' and return it. Modifies the object internally.
    #' @param rows rows to split out
    #' @param levels levels to split out
    #' @return specified levels in their own object
    bleb = function(rows, levels) {
      if ( missing(rows) & !missing(levels)) {
        return(private$.bleb_levels(levels))
      } else if (!missing(rows)) {
        return(private$.bleb_rows(rows))
      } 
      stop("One and only one of 'rows' and 'levels' must be specified'")
    },
    #' @description
    #' Count the number of times (optionally) specified levels of the state
    #' appear in the state vector
    #' @param levels state levels to include in the count
    #' @return named vector of state counts
    counts = function(levels) {
      if (missing(levels)) {
        levels = private$.labels
      }
      levels = levels[levels %in% private$.labels]
      counts = purrr::map_int(private$.index[levels], length)
      return(counts)
    },
    #' @description
    #' Remove a set of rows (or state levels) from the current representation
    #' @param rows rows to split out
    #' @param levels levels to split out
    #' @return self (modified)
    drop = function(rows, levels) {
      if ( missing(rows) & !missing(levels)) {
        return(private$.drop_levels(levels))
      } else if (!missing(rows)) {
        return(private$.drop_rows(rows))
      } 
      stop("One and only one of 'rows' and 'levels' must be specified'")
    },
    #' @description
    #' Calculate the index (into the state vector) of specified state levels
    #' @param levels state levels whose position is retrieved
    #' @return vector of indexes into the state vector
    which = function(levels, .strict = FALSE) {
      if (.strict) {
        private$.check_levels(levels)
      }
      idx = private$.index[levels] %>% 
        purrr::map(as.integer) %>% 
        purrr::discard(~ any(is.na(.x))) %>%
        purrr::flatten_int() %>%
        unique()
      return(idx)
    }
  ),
  private = list(
    #' (internal) .vec state vector (as a character vector)
    .vec = character(),
    #' (internal) .matrix column-wise sparse matrix representing indicator variable coding
    #' of the state vector
    .matrix = Matrix::sparseMatrix(i = 1, j = 1)[-1, -1],
    #' (internal) .n_units number of entries in the state vectors
    .n_units = 0,
    #' (internal) .n_states number of total state levels in the state vector (whether
    #' currently included in the vector or not).
    .n_states = 0,
    #' (internal) .labels labels of the state levels (whether currently included in the
    #' vector or not
    .labels = character(),
    #' (internal) .index named list stating which entries for each level appear in the
    #' state vector.
    .index = list(),
    #' (internal) implementation of blebbing rows
    .bleb_rows = function(rows) {
      private$.check_rows(rows)
      local_rows = 1:self$n_units %>% purrr::discard(~ .x %in% rows)
      other = self$clone()
      other = other$drop(local_rows)
      private$.drop_rows(rows)
      return(other)
    },
    #' (internal) implementation of blebbing levels (defers to blebbing rows).
    .bleb_levels = function(levels, .strict = FALSE) {
      if (.strict) {
        private$.check_levels(levels)
      }
      idx = self$which(levels) 
      return(private$.bleb_rows(idx))
    },
    #' (internal) check that state vector indexes are valid
    .check_rows = function(rows) {
      rows_in_set = purrr::map_lgl(rows, ~ isTRUE(.x >= 1 && .x <= private$.n_units)) %>% all()
      stopifnot(rows_in_set)
    },
    #' (internal) check that levels are valid
    .check_levels = function(levels) {
      levels_in_set = purrr::map_lgl(levels, ~ isTRUE(.x %in% names(private$.index))) %>% all()
      stopifnot(levels_in_set)
    },
    #' (internal) implementation of dropping rows
    .drop_rows = function(rows) {
      if (length(rows) == 0) {
        return(self)
      }
      private$.check_rows(rows)
      private$.matrix = private$.matrix[-rows, , drop = FALSE]
      private$.n_units = nrow(private$.matrix)
      private$.index = private$.matrix_to_index(private$.matrix)
      return(self)
    },
    #' (internal) implementation of dropping levels
    .drop_levels = function(levels, .strict = FALSE) {
      if (.strict) {
        private$.check_levels(levels)
      }
      idx = self$which(levels)
      return(private$.drop_rows(idx))
    },
    #' (internal) transform the matrix form of the state vector to an index and
    #' return it
    .matrix_to_index = function(m) {
      idx = purrr::map2(
          .x = m@p[1:(length(m@p) - 1)],
          .y = m@p[2:(length(m@p)    )] - 1, 
          ~ (.x:.y) + 1) %>% 
        purrr::map_if(~ isTRUE(length(.x) == 2 && .x[1] > .x[2]), ~ numeric()) %>% 
        purrr::map( ~ m@i[.x] + 1) %>% 
        rlang::set_names(colnames(m))
      return(idx)
    },
    .update_labels = function(labels = character(), new_labels = character(), state = character()) {
      if (is.null(labels)) {
        labels = character()
      }
      if (is.null(new_labels)) {
        new_labels = character()
      }
      if (is.null(state)) {
        state = character()
      }
      new_labels = state %>% c(new_labels) %>% unique() %>%
        purrr::discard( ~ .x %in% labels) %>% sort()
      labels = c(labels, new_labels)
      return(labels)
    },
    #' (internal) state to index
    .update_idx = function(idx, n_units, new_labels, new_state) {
      labels = private$.update_labels(names(idx), new_labels, new_state)
      idx = purrr::map(labels, ~ c(idx[[.x]], n_units + which(new_state == .x))) %>%
        purrr::map_if(~ is.null(.x), ~ integer()) %>%
        rlang::set_names(labels)
      return(idx)
    },
    #' (internal) transform the index to row indexes
    .index_to_r_idx = function(idx) {
      row_idx = idx %>% 
        purrr::map(as.integer) %>%
        purrr::flatten_int()
      return(row_idx)
    },
    #' (internal) transform the index to col indexes
    .index_to_c_idx = function(idx) {
      col_idx = purrr::map2(
          .x = seq_along(idx),
          .y = purrr::map_int(idx, length),
          .f = ~ as.integer(.x) %>% rep(.y)) %>%
        purrr::flatten_int()
      return(col_idx)
    },
    #' (internal) transform the index from of the state vecto to the matrix
    #' form and return it
    .index_to_matrix = function(idx, dims) {
      row_idx = private$.index_to_r_idx(idx)
      col_idx = private$.index_to_c_idx(idx)
      if (missing(dims)) {
        warning("Inferring n_rows and n_cols, rarely works.")
        dims = c(max(row_idx), max(col_idx))
      }
      m = Matrix::sparseMatrix(
        i = row_idx, j = col_idx, x = rep(1, length(row_idx)),
        dims = dims, dimnames = list(1:dims[1], names(idx)))
      return(m)
    },
    #' (internal) transform the index representation of the state vector to a 
    #' state vector and return it
    .index_to_state = function(idx) {
      o = vector(mode = 'character', length = private$.n_units)
      for (label in self$labels) {
        o[self$index[[label]]] = label
      }
      return(o)
    },
    #' (internal) probabilistic transition
    .transition = function(m) self$.matrix %*% m 
  ),
  active = list(
    #' @field index return the index (into state vector) of known levels
    index = function() private$.index,
    #' @field labels return the known state level labels 
    labels = function() private$.labels,
    #' @field matrix return the matrix representation of the state vector
    matrix = function() private$.matrix,
    #' @field state return the (implicit) state vector
    state = function() private$.index_to_state(private$.index),
    #' @field n_units return the (implicit) number of units
    n_units = function() private$.n_units,
    #' @field dims return the (implicit) matrix representation dimensions
    dims = function() c(private$.n_units, private$.n_states)
  )
)




