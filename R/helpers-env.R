
#' Copy objects from one env to another
#'
#' Ignores missing objects.
#'
#' @param from environment to copy from
#' @param to environment to copy to
#' @param nms names of objects to copy
#' @return names of objects copied
#' @export
env_copy = function(from, to, nms) {
  nms_available = nms[nms %in% rlang::env_names(from)]
  for (nm in nms_available) {
    rlang::env_bind(to, !!nm := rlang::env_get(from, nm))
  }
  return(nms_available)
}

#' Move objects from one env to another
#'
#' Ignores missing objects.
#'
#' @param from environment to copy from
#' @param to environment to copy to
#' @param nms names of objects to copy
#' @return names of objects copied
#' @export
env_move = function(from, to, nms) {
  nms_copied = env_copy(from, to,  nms)
  for (nm in nms_copied) {
    rlang::env_unbind(from, nm)
  }
  return(nms_copied)
}

