incidence.numeric <-
function(SymptomOnset, Interval = 1, ...) {

  # Delete after testing and before creating package
  print("Numeric")

  # Move so that the first incidence is on day 1 (assumed in
  # incidenceComputation)
  SymptomOnsetNumeric <- SymptomOnset - min(SymptomOnset) + 1

  # Use incidenceComputation to compute the incidents
#  # Delete after testing
#  source("incidenceComputation.R")
  Incidents <- incidenceComputation(SymptomOnsetNumeric, Interval)

  # Shift data back to the starting value of the input
  Incidents$Days <- Incidents$Days + min(SymptomOnset) - 1
  Incidents   }
