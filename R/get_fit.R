#' Accessors for `incidence_fit` objects
#' 
#' @param x an `incidence_fit` or `incidence_fit_list`
#'   object.
#' @return a list of `incidence_fit` objects. 
#' @export
#' @examples
#' 
#' if (require(outbreaks)) {
#'  dat <- ebola_sim$linelist$date_of_onset
#'
#'  ## EXAMPLE WITH A SINGLE MODEL
#'
#'  ## compute weekly incidence
#'  sex <- ebola_sim$linelist$gender
#'  i.sex <- incidence(dat, interval = 7, group = sex)
#'  
#'  ## Compute the optimal split for each group separately
#'  fits  <- fit_optim_split(i.sex, separate_split = TRUE)
#'
#'  ## `fits` contains an `incidence_fit_list` object
#'  fits$fit
#'  
#'  ## Grab the list of `incidence_fit` objects
#'  get_fit(fits$fit)
#'  
#' }
get_fit <- function(x) {
  UseMethod("get_fit")
}
#' @rdname get_fit
#' @export
get_fit.incidence_fit <- function(x) {
  x
}

#' @rdname get_fit
#' @export
get_fit.incidence_fit_list <- function(x) {
  locations <- attr(x, "locations")
  res <- lapply(locations, function(i) x[[i]])
  names(res) <- vapply(locations, paste, character(1), collapse = "_")
  res
}
