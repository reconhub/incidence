library(incidence)
library(outbreak)

onset <- ebola_sim$linelist$date_of_onset
i <- incidence(dat, interval = 7)

