check_timespan <- function(x) {
  max_days <- as.difftime(getOption("incidence.max.days"), units = "days")
  my_range <- range(x, na.rm = TRUE)
  if (diff(my_range) > max_days) {
    msg <- paste("The data has a date range of greater than %d days [%s to %s].",
                 "Please check your data to ensure this is accurate.\n",
                 "To remove this warning, set the `incidence.max.days` option",
                 "to Inf:\n\n\toptions(incidence.max.days = Inf)"
                 )
    msg <- sprintf(msg, 
                   max_days, 
                   as.character(my_range[1]), 
                   as.character(my_range[2]))
    warning(msg, immediate. = TRUE, call. = FALSE)
  }
  invisible()
}
