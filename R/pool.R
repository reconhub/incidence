##' Pool 'incidence' across groups
##'
##' This function pools incidence across all groups of an `incidence`
##' object. The resulting [incidence()] object will contains counts
##' summed over all groups present in the input.
##'
##' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
##'
##' @seealso The [incidence()] function to generate the 'incidence'
##' objects.
##'
##' @inheritParams incidence
##'
##' @export
##'
##' @examples
##' dat <- as.integer(c(0,1,2,2,3,5,7))
##' group <- factor(c(1, 2, 3, 3, 3, 3, 1))
##' i <- incidence(dat, groups = group)
##' i
##' i$counts
##'
##' ## pool all groups
##' pool(i)
##' pool(i)$counts
##'
##' ## pool only groups 1 and 3
##' pool(i[,c(1,3)])
##' pool(i[,c(1,3)])$counts
##'

pool <- function(x){
    if (!inherits(x, "incidence")) {
        stop(sprintf(
            "x should be an 'incidence' object (its class is: %s)",
            class(x)))
    }
    if (ncol(x$counts) == 1) return(x)
    x$counts <- matrix(apply(x$counts, 1 , sum), ncol = 1)
    x
}
