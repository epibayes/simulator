
#' Smooth points at at fine scale
#'
#' @param x values to smooth
#' @param t their locations
#' @return smoothed x values
#'
#' @export
smooth = function(x, t) {
  require(mgcv)
  data = tibble::tibble(x = x, t = t)
  o = mgcv::gam(log(x + 1) ~ s(t, k = 30), data = data) %>%
    predict()
  o = exp(o) - 1
  return(o)
}
  
