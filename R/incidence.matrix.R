incidence.matrix <-
function(SymptomOnset, Interval = 1, ...) {

  # Delete after testing and before creating package
  print("Matrix")

  # Print a warning if there are more than one column
  if (ncol(SymptomOnset) > 1) {
    warning("The matrix SymptomOnset has more than one column. Only the first column has been used")  }

  # Choose only the first member of the list
  SymptomOnset_NonMatrix <- SymptomOnset[, 1]

  # Run incidence again
  incidence(SymptomOnset_NonMatrix, Interval)
  }
