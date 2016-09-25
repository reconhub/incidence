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
    colors <- c("#aa3939", "#4a6a8a", "#d4aa6a","#499371")
    if (n < 4) return(colors[1:n])
    return(colorRampPalette(colors)(n))
}





##' @export
##' @rdname palettes

pal1light <- function(n){
    if(!is.numeric(n)) stop("n is not a number")
    colors <- c("#d46a6a", "#738ca6", "#ffddaa","#76b096")
    if (n < 4) return(colors[1:n])
    return(colorRampPalette(colors)(n))
}






##' @export
##' @rdname palettes

pal1dark <- function(n){
    if(!is.numeric(n)) stop("n is not a number")
    colors <- c("#801515", "#2b4c6f", "#aa7d39","#277552")
    if (n < 4) return(colors[1:n])
    return(colorRampPalette(colors)(n))
}
