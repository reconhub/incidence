.onLoad <- function(...) {
  op <- options()
  op.incidence <- list(incidence.max.days = 18262)
  toset <- !names(op.incidence) %in% op
  if (any(toset)) options(op.incidence[toset])
}
