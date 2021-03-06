% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_counts.R
\name{get_counts}
\alias{get_counts}
\alias{get_counts.incidence}
\title{Get counts from an incidence object}
\usage{
get_counts(x, groups = NULL)

\method{get_counts}{incidence}(x, groups = NULL)
}
\arguments{
\item{x}{an \code{incidence} object.}

\item{groups}{if there are groups, use this to specify a group or groups to
subset. Defaults to \code{NULL} indicating that all groups are returned.}
}
\value{
a matrix of counts where each row represents a date bin
}
\description{
Get counts from an incidence object
}
\examples{
if (require(outbreaks)) { withAutoprint({
  dat  <- ebola_sim$linelist$date_of_onset
  gend <- ebola_sim$linelist$gender
  i    <- incidence(dat, interval = "week", groups = gend)
  
  ## Use with an object and no arguments gives the counts matrix
  head(get_counts(i))

  ## Specifying a position or group name will return a matrix subset to that
  ## group
  head(get_counts(i, 1L))
  head(get_counts(i, "f"))

  ## Specifying multiple groups allows you to rearrange columns
  head(get_counts(i, c("m", "f")))

  ## If you want a vector, you can use drop
  drop(get_counts(i, "f"))
})}
}
