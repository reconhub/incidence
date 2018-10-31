#' extract and set group names
#' @param x an [incidence()] object.
#' @param value character vector used to rename groups
#' @return an integer indicating the number of groups present in the incidence
#'   object.
#' @details This accessor will return a  
#' @export
#' @examples
#' i <- incidence(dates = sample(10, 100, replace = TRUE), 
#'                interval = 1L,
#'                groups = sample(letters[1:3], 100, replace = TRUE))
#' i
#' group_names(i)
#'
#' # change the names of the groups
#' group_names(i) <- c("Group 1", "Group 2", "Group 3")
#' i
#'
#' # example if there are mistakes in the original data, e.g. 
#' # something is misspelled
#' set.seed(50)
#' grps <- sample(c("child", "adult", "adlut"), 100, replace = TRUE, prob = c(0.45, 0.45, 0.05))
#' i <- incidence(dates = sample(10, 100, replace = TRUE), 
#'                interval = 1L,
#'                groups = grps)
#' colSums(get_counts(i))
#' 
#' # If you change the name of the mis-spelled group, it will be merged with the
#' # correctly-spelled group
#' gname <- group_names(i)
#' gname[gname == "adlut"] <- "adult"
#' # without side-effects
#' print(ii <- group_names(i, gname))
#' colSums(get_counts(i))  # original still has three groups
#' colSums(get_counts(ii))
#' # with side-effects
#' group_names(i) <- gname
#' colSums(get_counts(i))
group_names <- function(x, value) {
	UseMethod("group_names", x)
}

#' @export
#' @rdname group_names
"group_names<-" <- function(x, value) {
	UseMethod("group_names<-", x)
}

#' @rdname group_names
#' @export
#' @aliases group_names.default
group_names.default <- function(x, value) {
    stop(sprintf("Not implemented for class %s",
                 paste(class(x), collapse = ", ")))
}
#' @rdname group_names
#' @export
#' @aliases `group_names<-`.default
"group_names<-.default" <- function(x, value) {
    stop(sprintf("Not implemented for class %s",
                 paste(class(x), collapse = ", ")))
}

#' @rdname group_names
#' @export
#' @keywords accessors
group_names.incidence <- function(x, value = NULL){
	if (is.null(value)) {
		colnames(x$counts)
	} else {
		`group_names<-`(x, value)
	}
}

#' @rdname group_names
#' @export
"group_names<-.incidence" <- function(x, value) {
	if (length(value) != ncol(x)) {
		stop("value must be the same length as the number of groups.")
	}
	if (anyNA(value <- as.character(value))) {
		stop("value must be able to be coerced to a character vector")
	}
	uval <- unique(value)
	if (identical(uval, value)) {
		colnames(x$counts) <- value
	} else {
		the_counts <- x$counts
		out <- matrix(integer(nrow(the_counts)*length(uval)),
			      nrow = nrow(the_counts),
			      ncol = length(uval)
			     )
		colnames(out) <- uval
		for (i in uval) {
			out[, i] <- rowSums(the_counts[, value == i, drop = FALSE])
		}
		x$counts <- out
	}
	x
}
