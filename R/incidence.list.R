# This is the method that is used when the input into incidence() is of the
# class list. This method prints a warning if the list has more than one
# members. It then takes the first member and re-runs incidence, which will choose
# a method according to the class of the first member of the input.

# Input:
#     SymptomOnset  - a list with m members, each of length n, where n is the
#                     number of cases and the members after the first are
#                     irrelevant for this computation
#     Interval      - an integer (number of days in the interval)
# Output:
#                   - there is no output, because incidence is re-run with the
#                     first member of the matrix

incidence.list <-
function(SymptomOnset, Interval = 1, ...) {

  # Delete after testing and before creating package
  print("List")

  # Print a warning if there are more than one member of the list
  if (length(SymptomOnset) > 1) {
    warning("The list SymptomOnset has more than one member. Only the first member of the list has been used")  }

  # Choose only the first member of the list
  SymptomOnset_NonList <- SymptomOnset[[1]]

  # Run incidence again
  incidence(SymptomOnset_NonList, Interval)
  }
