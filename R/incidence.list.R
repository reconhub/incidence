incidence.list <-
function(SymptomOnset, Interval=1, ...) {
  
  # Delete after testing and before creating package
  print("List")
  
  # Print a warning if there are more than one member of the list
  if (length(SymptomOnset)>1) {
    warning("The list SymptomOnset has more than one member. Only the first member of the list has been used")  }

  # Choose only the first member of the list
  SymptomOnset_NonList <- SymptomOnset[[1]]
  
  # Run incidence again
  incidence(SymptomOnset_NonList, Interval) }
