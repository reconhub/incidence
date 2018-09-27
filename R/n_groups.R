#' count number of groups
#' @param x an [incidence()] object.
#' @param ... unused
#' @return an integer indicating the number of groups present in the incidence
#'   object.
#' @export
#' @examples
#' i <- incidence(dates = sample(10, 100, replace = TRUE), 
#'                interval = 1L,
#'                groups = sample(letters[1:2], 100, replace = TRUE))
#' n_groups(i)
n_groups <- function(x, ...) {
	UseMethod("n_groups")
}

#' @rdname n_groups
#' @export
#' @aliases n_groups.default
n_groups.default <- function(x, ...) {
    stop(sprintf("Not implemented for class %s",
                 paste(class(x), collapse = ", ")))
}

#' @rdname n_groups
#' @export
#' @keywords accessors
n_groups.incidence <- function(x, ...){
	ncol(get_counts(x))
}
