##' Color palettes used in incidence
##'
##' These functions are color palettes used in incidence.
##'
##' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
##'
##' @param n a number of colors
##'
##' @rdname palettes
##' @aliases palettes pal1 pal1light pal1dark
##'
##' @export
##' @importFrom grDevices colorRampPalette
##'
##' @examples
##'
##' plot(1:4, cex=8, pch=20, col = pal1(4), main="palette: pal1")
##' plot(1:100, cex=8, pch=20, col = pal1(100), main="palette: pal1")
##'
pal1 <- function(n){
    if(!is.numeric(n)) stop("n is not a number")
    colors <- c("#d4876a", "#d4aa6a", "#4a6a8a","#499371")
    return(colorRampPalette(colors)(n))
}





##' @export
##' @rdname palettes

pal1light <- function(n){
    if(!is.numeric(n)) stop("n is not a number")
    colors <- c("#ffc1aa", "#ffddaa", "#738ca6","#76b096")
    return(colorRampPalette(colors)(n))
}






##' @export
##' @rdname palettes

pal1dark <- function(n){
    if(!is.numeric(n)) stop("n is not a number")
    colors <- c("#aa5739", "#aa7d39", "#2b4c6f","#277552")
    return(colorRampPalette(colors)(n))
}
