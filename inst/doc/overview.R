## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  fig.width=10, 
  fig.height=6, 
  fig.path="figs-overview/"
)

## ----install, eval=FALSE-------------------------------------------------
#  devtools::install_github("reconhub/incidence")

## ---- data---------------------------------------------------------------
library(outbreaks)
library(ggplot2)
library(incidence)

dat <- ebola.sim$linelist$date.of.onset
class(dat)
head(dat)

## ---- incid1-------------------------------------------------------------
i <- incidence(dat)
i
plot(i)

## ---- interv-------------------------------------------------------------
## weekly
i.7 <- incidence(dat, interval=7)
plot(i.7)

## bi-weekly
i.14 <- incidence(dat, interval=14)
plot(i.14, border = "white")

## period of 30 days
i.30 <- incidence(dat, interval=30)
plot(i.30, border = "white")


## ---- gender-------------------------------------------------------------
i.7.sex <- incidence(dat, interval=7, groups = ebola.sim$linelist$gender)
i.7.sex
plot(i.7.sex, stack = TRUE, border = "grey")

## ---- hosp---------------------------------------------------------------
i.7.hosp <- with(ebola.sim.clean$linelist, 
	 incidence(date.of.onset, interval=7, groups = hospital))
i.7.hosp
head(i.7.hosp$counts)
plot(i.7.hosp, stack=TRUE) + 
    theme(legend.position= "top") + 
    labs(fill="")

## ---- middle-------------------------------------------------------------
i[100:250]
plot(i[100:250])

## ---- stripes------------------------------------------------------------
i.7[c(TRUE,FALSE)]
plot(i.7[c(TRUE,FALSE)])

## ---- tail---------------------------------------------------------------
i.tail <- subset(i, from=as.Date("2015-01-01"))
i.tail
plot(i.tail, border="white")

## ---- i7outcome----------------------------------------------------------
i.7.outcome <- incidence(dat, 7, groups=ebola.sim$linelist$outcome)
i.7.outcome
plot(i.7.outcome, stack = TRUE, border = "grey")

## ---- groupsub-----------------------------------------------------------
i.7.outcome[,1:2]
plot(i.7.outcome[,1:2], stack = TRUE, border = "grey")

## ---- pool---------------------------------------------------------------
i.pooled <- pool(i.7.outcome)
i.pooled
identical(i.7$counts, i.pooled$counts)

## ---- fit1---------------------------------------------------------------
plot(i.7[1:20])
early.fit <- fit(i.7[1:20])
early.fit

## ------------------------------------------------------------------------
plot(early.fit)

## ------------------------------------------------------------------------
plot(i.7[1:20], fit = early.fit)

## ---- fit.both-----------------------------------------------------------
fit.both <- fit(i.7, split=as.Date("2014-10-15"))
fit.both
plot(i.7, fit=fit.both)

## ---- optim--------------------------------------------------------------
best.fit <- fit_optim_split(i.7)
best.fit
plot(i.7, fit=best.fit$fit)

## ---- optim2-------------------------------------------------------------
best.fit2 <- fit_optim_split(i.7.sex)$fit
best.fit2
plot(i.7.sex, fit=best.fit2)

