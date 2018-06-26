

library(incidence)
library(outbreaks)

dat <- ebola_sim_clean$linelist


cat(paste(head(dat$date_of_onset), collapse = "\n"))

