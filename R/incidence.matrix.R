#' Method for input of class matrix.
#'
#' This is the method that is used when the input into incidence() is of the
#' class matrix.
#'
#' This method prints a warning if the matrix has more than one column. It then
#' takes the first column and re-runs incidence, which will choose a method
#' according to the class of the first column of the input.
#'
#' @param SymptomOnset An nxm matrix, where n is the number of cases and all but
#'   the first column are irrelevant for the computation of incidents
#' @param Interval An integer (number of days in the interval, default = 1)
#' @return A named list with 2 members (Days [first day per interval] and
#'   NoOfCases [count of incidents during the respective interval])
#' @examples
#' incidenceComputation(matrix(c(1, 5, 8, 3, 7, 2, 4, 6, 9, 2), nrow=10))
#' incidenceComputation(matrix(c(1, 5, 8, 3, 7, 2, 4, 6, 9, 2), nrow=10), 2)
incidence.matrix <- function(SymptomOnset, Interval = 1, ...) {

  # Delete after testing and before creating package
  print("Matrix")

  # Print a warning if there are more than one column
  if (ncol(SymptomOnset) > 1) {
    warning("The matrix SymptomOnset has more than one column. Only the first column has been used")
    }

  # Choose only the first column of the matrix
  SymptomOnset_NonMatrix <- SymptomOnset[, 1]

  # Run incidence again
  incidence(SymptomOnset_NonMatrix, Interval)
}
