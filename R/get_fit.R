get_fit <- function(x) {
  UseMethod("get_fit")
}

get_fit.incidence_fit <- function(x) {
  x
}

get_fit.incidence_fit_list <- function(x) {
  locations <- attr(x, "locations")
  res <- lapply(locations, function(i) x[[i]])
  names(res) <- vapply(locations, paste, character(1), collapse = "_")
  res
}
