## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  fig.width=10, 
  fig.height=6, 
  fig.path="figs-class/"
)

## ---- data---------------------------------------------------------------
library(incidence)
set.seed(1)
dat <- sample(1:50, 200, replace = TRUE, prob = 1 + exp(1:50 * 0.1))
sex <- sample(c("female", "male"), 200, replace = TRUE)

## ---- i------------------------------------------------------------------
i <- incidence(dat, interval = 2)
i
plot(i)

## ---- sex----------------------------------------------------------------
i.sex <- incidence(dat, interval = 2, group = sex)
i.sex
plot(i.sex)

## ---- names--------------------------------------------------------------
class(i)
is.list(i)
names(i)

## ---- access-------------------------------------------------------------
## use name
head(i$dates)

## use numeric indexing
head(i[[2]])

## ---- dates1-------------------------------------------------------------
class(i$dates)
class(dat)

i$dates

## ---- counts1------------------------------------------------------------
class(i$counts)
storage.mode(i$counts)

i$counts
i.sex$counts

## ---- as.data.frame------------------------------------------------------
## basic conversion
as.data.frame(i)
as.data.frame(i.sex)

## long format for ggplot2
as.data.frame(i.sex, long = TRUE)

## ---- timespan-----------------------------------------------------------
i$timespan
range(i$dates)
diff(range(i$dates)) + 1

## ---- interval-----------------------------------------------------------
i$interval
diff(i$dates)

## ---- n------------------------------------------------------------------
i$n

## ---- n2-----------------------------------------------------------------
apply(i.sex$counts, 2, sum)

