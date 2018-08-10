#' Get counts from an incidence object
#' 
#' @param x an `incidence` object.
#' @param groups if there are groups, use this to specify a group or groups to 
#'   subset. Defaults to `NULL` indicating that all groups are returned.
#' 
#' @return a matrix of counts where each row represents a date bin
#' @export
#' @examples
#' if (require(outbreaks)) { withAutoprint({
#'   dat  <- ebola_sim$linelist$date_of_onset
#'   gend <- ebola_sim$linelist$gender
#'   i    <- incidence(dat, interval = "week", groups = gend)
#'   
#'   ## Use with an object and no arguments gives the counts matrix
#'   head(get_counts(i))
#'
#'   ## Specifying a position or group name will return a matrix subset to that
#'   ## group
#'   head(get_counts(i, 1L))
#'   head(get_counts(i, "f"))
#'
#'   ## Specifying multiple groups allows you to rearrange columns
#'   head(get_counts(i, c("m", "f")))
#'
#'   ## If you want a vector, you can use drop
#'   drop(get_counts(i, "f"))
#' })}
get_counts <- function(x, groups = NULL) {
  UseMethod("get_counts")
}

#' @rdname get_counts
#' @export
get_counts.incidence <- function(x, groups = NULL){
  if (is.null(groups) || ncol(x$counts) == 1) {
    return(x$counts)
  }
  if (is.character(groups)) {
    correct_groups <- groups[groups %in% colnames(x$counts)]
  }
  if (is.numeric(groups)) {
    correct_groups <- groups[groups %in% seq(ncol(x$counts))]
  }
  if (!identical(correct_groups, groups)) {
    grps <- paste(setdiff(groups, correct_groups), collapse = ", ")
    msg  <- sprintf("The following groups were not recognised: %s", grps)    
    message(msg)
  } 
  if (length(correct_groups) == 0) {
    grps <- paste(colnames(x$counts), collapse = ", ")
    stop(sprintf("No groups matched those present in the data: %s", grps))
  }
  return(x$counts[, correct_groups, drop = FALSE])
}
