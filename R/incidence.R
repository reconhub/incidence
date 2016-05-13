# This is the generic function that decides which method to call, depending on the
# class of the input object. The possible methods are:
# incidence.numeric
# incidence.integer
# incidence.PoisXlt
# incidence.PoisXct
# incidence.Date
# incidence.list
# incidence.matrix

incidence <- function(SymptomOnset, Interval = 1, ...) {
  UseMethod("incidence")
  }
