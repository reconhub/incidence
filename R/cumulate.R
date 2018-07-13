#' Compute cumulative 'incidence'
#'
#' `cumulate` is an S3 generic to compute cumulative numbers, with methods
#' for different types of objects:
#'
#' \itemize{
#'
#' \item default method is a wrapper for `cumsum`
#'
#' \item `incidence` objects: computes cumulative incidence over time
#'
#' \item `projections` objects: same, for `projections` objects,
#' implemented in the similarly named package; see `?cumulate.projections`
#' for more information, after loading the package
#'
#' }
#'
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @seealso The [incidence()] function to generate the 'incidence'
#' objects.
#'
#' @param x An incidence object.
#'
#' @export
#'
#' @examples
#' dat <- as.integer(c(0,1,2,2,3,5,7))
#' group <- factor(c(1, 2, 3, 3, 3, 3, 1))
#' i <- incidence(dat, groups = group)
#' i
#' plot(i)
#'
#' i_cum <- cumulate(i)
#' i_cum
#' plot(i_cum)
#'

#' @rdname cumulate
cumulate <- function(x) {
  UseMethod("cumulate", x)
}




#' @rdname cumulate
#' @export
cumulate.default <- function(x) {
  cumsum(x)
}




#' @rdname cumulate
#' @export
cumulate.incidence <- function(x) {
  if (isTRUE(x$cumulative)) {
    stop("x is already a cumulative incidence")
  }
  out <- x
  out$counts <- apply(x$counts, 2, cumsum)
  out$cumulative <- TRUE
  out
}
