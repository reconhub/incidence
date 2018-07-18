#' Check the user-fed arguments and give warnings if they are wrong.
#'
#' @param dots a list of user-supplied arguments
#' @param args names of arguments appropriate for the function.
#'
#' @return dots, modified if necessary
#' @noRd
#' @keywords internal
#'
#' @examples
#' dots <- c(hAy = 1, "lsdflk" = 1, "stuperman" = TRUE, iso_week = TRUE)
#' args <- c("superman", "hey", "ho", "lets", "go", "standard")
#' check_dots(dots, args)
check_dots <- function(dots, args) {
  if (length(dots) == 0) {
    return(dots)
  }
  dnames     <- names(dots)
  scores     <- utils::adist(paste0("^", dnames), args, fixed = FALSE) < 2
  recognized <- rowSums(scores) > 0
  msg <- ""
  if (sum(scores) > 0) {
    words <- apply(scores[recognized, , drop = FALSE], 1,
                   function(i) paste(args[i], collapse = ", "))
    errs  <- paste(format(dnames[recognized]), format(words), sep = " : ")
    errs  <- paste(errs, collapse = "\n\t")
    msg   <- sprintf("\n\nPotentially misspelled options:\n\t%s", errs)
  }
  if (sum(!recognized) > 0) {
    dre <- paste(dnames[!recognized & dnames != "iso_week"], collapse = ", ")
    msg <- paste0(msg, "\n\nUnrecognised options:\n\t", dre)
  }
  if ("iso_week" %in% dnames) {
    warning(paste("The parameter `iso_week` has been deprecated as of incidence",
                  "version 1.3. Please use `standard` instead."),
            call. = FALSE
    )
    names(dots)[dnames == "iso_week"] <- "standard"
  }
  MSG <- "Misspelled or unrecognized options will be ignored."
  warning(paste(MSG, msg), call. = FALSE)
  dots
}
