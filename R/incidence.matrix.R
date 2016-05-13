# This is the method that is used when the input into incidence() is of the
# class matrix. This method prints a warning if the matrix has more than one
# column. It then takes the first column and re-runs incidence, which will choose a
# method according to the class of the first column of the input.

# Input:
#     SymptomOnset  - a nxm matrix, where n is the number of cases and the columns
#                     after the first are irrelevant for this computation
#     Interval      - an integer (number of days in the interval)
# Output:
#                   - there is no output, because incidence is re-run with the
#                     first column of the matrix

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
