## This function takes date of symptom onset, per case, as input which is in
## the format of number of days after a reference date. Each row represents a
## case, and the reference day could be the day of the oubreak, if known. 0
## would suggest the case developed symptoms on the reference date, 1 would
## mean the day after the reference date, etc.
## The number of incidences per interval (in days) are then computed, for
## which the default length of the interval is 1 day.
## The output is a named list with a member for the interval (Days - same
## format as the input of SymptomOnset) and a member for the number of
## incidences in that interval. The interval is denoted by the first day in
## the interval, i.e. if the list of the interval is 0,3,6,... then the
## interval is of length 3 and days 0, 1 and 2 are in the first interval, days
## 3, 4 and 5 are in the seconde, etc.

# Input:
#   SymptomOnset  - an nx1 numerical vector, where n is the number of cases
#   Interval      - an integer (number of days in the interval)
# Output:
#   Incidents     - a named list with 2 members (Days [first day per interval]
#                   and NoOfCases [count of incidents during the respective
#                   interval])

incidenceComputation <- function(SymptomOnset, Interval) {

  LastDay       <- max(SymptomOnset)
  DaysTemp      <- numeric()
  NoOfCasesTemp <- numeric()
  CasesTemp     <- table(SymptomOnset)
  # Delete after testing
  print(CasesTemp)

  ## Compute the number of incidents per day
  for (i in seq(0, LastDay))   {
    DaysTemp[i + 1] <- i
    if (length(CasesTemp[names(CasesTemp) == i]) == 0) {
      NoOfCasesTemp[i + 1] <- 0
    } else {
      NoOfCasesTemp[i + 1] <- CasesTemp[names(CasesTemp) == i]
    }
    }

  ## If Interval is > 1 group Interval days and cases together
  Days      <- numeric()
  NoOfCases <- numeric()
  if (Interval > 1) {
    for (j in seq(0, LastDay, Interval)) {
      Days[(j + Interval) / Interval]       <- j
      Cases <- 0
      for (ii in seq(1, Interval)) {
        if (length(CasesTemp[names(CasesTemp) == Days[(j + Interval) / Interval] + ii - 1]) > 0) {
        Cases <- Cases + as.vector(CasesTemp[names(CasesTemp) == Days[(j + Interval) / Interval] + ii - 1])
        }
        }
        NoOfCases[(j + Interval) / Interval]  <- Cases
        }
    } else  {
        Days      <- DaysTemp
        NoOfCases <- NoOfCasesTemp
    }

  # Add 0 for the number of incidents on the last day without any incidents
  Days[floor(LastDay / Interval) + 2] <- Interval * floor(LastDay / Interval) + Interval
  NoOfCases[floor(LastDay / Interval) + 2]  <- 0

  ## Save as named list
  Incidents <- list(Days = Days, NoOfCases = NoOfCases)
  }
