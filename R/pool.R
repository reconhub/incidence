##' Pool 'incidence' across groups
##'
##' This function pools incidence across all groups of an \code{indicence} object.
##'
##' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
##'
##' @rdname subset
##'
##' @seealso the \code{\link{incidence}} function to generate the 'incidence' objects.
##'
##' @inheritParams incidence
##'
##' @export
##'

pool <- function(x){
    if (!inherits(x, "incidence")) {
        stop(sprintf("x should be an 'incidence' object (its class is: %s)",
                     class(x)))
    }

    x$counts <- matrix(apply(x$counts, 1 , sum), ncol = 1)
    x
}
