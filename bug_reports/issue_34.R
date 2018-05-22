
## Reproduce the error
library(incidence)
library(outbreaks)

onset <- ebola_sim$linelist$date_of_onset
i <- incidence(onset, interval = 7)
plot(i)

## Clue: this is not happening if we don't use the iso_week definition
i <- incidence(onset, 7, iso_week = FALSE)
plot(i)


## Explanation: the code generating iso weeks hacked through the ggplot2
## structure. Instead we specify our own labels and pass them through
## ggplot2::scale_x_date().

breaks_ini <- pretty(i$dates)
iso_weeks <- ISOweek::date2ISOweek(breaks_ini)
iso_weeks_day1 <- sub("-[1-7]+$", "-1", iso_weeks)
breaks_day1 <- ISOweek::ISOweek2date(iso_weeks_day1)
labels <- sub("-[1-7]+$", "", iso_weeks)


plot(i) + scale_x_date(breaks = breaks_day1, labels = labels)



