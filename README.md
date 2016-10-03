
[![Travis-CI Build Status](https://travis-ci.org/reconhub/incidence.svg?branch=master)](https://travis-ci.org/reconhub/incidence)

[![Coverage Status](https://img.shields.io/codecov/c/github/reconhub/incidence/master.svg)](https://codecov.io/github/reconhub/incidence?branch=master)

# incidence: computation, handling and visualisation of incidence

This package implements functions and classes to compute, handle, visualise and model incidences
from dates data.



# Installing the package

To install the devel version of the package, type:

```r
devtools::install_github("reconhub/incidence")
```


Note that this requires the package *devtools* installed.



# What does it do?

In the following we provide a quick overview of the package's functionalities.

## Main functions

- **`incidence`**: compute incidence from dates in various formats; any fixed time interval can be used; the returned object is an instance of the (S3) class *incidence*.
- **`subset`**: subset an *incidence* object by specifying a time window.
- **`[`**: lower-level subsetan of *incidence* objects, permiting to specify which dates and groups to retain; uses a syntax similar to matrices, i.e. `x[i, j]`, where `x` is the *incidence* object, `i` a subset of dates, and `j` a subset of groups.
- **`fit`**: fit one or two exponential models (i.e. linear regression on log-incidence) to an *incidence* object; two models are calibrated only if a date is provided to split the time series in two (argument `split`); this is typically useful to model the two phases of exponential growth, and decrease of an outbreak; each model returned is an instance of the (S3) class *incidence.fit*, each of which contains various useful information (e.g. growth rate *r*, doubling/halving time, predictions and confidence intervals).
- **`fit.optim.split`**: finds the optimal date to split the time series in two, typically around the peak of the epidemic.
- **`plot`**: this method (see `?plot.incidence` for details) plots *incidence* objects, and can also add predictions of the model(s) contained in an  *incidence.fit* object (or a list of such objects).


## Examples

### Simulated Ebola outbreak

This example uses the simulated Ebola Virus Disease (EVD) outbreak from the package
[*outbreaks*](http://github.com/reconhub/outbreaks). We will compute incidence for various time
steps, calibrate two exponential models around the peak of the epidemic, and analyse the results.

First, we load the data:

```r
library(outbreaks)
library(ggplot2)
library(incidence)

dat <- ebola.sim$linelist$date.of.onset
class(dat)
```

```
## [1] "Date"
```

```r
head(dat)
```

```
## [1] "2014-04-07" "2014-04-15" "2014-04-21" "2014-04-27" "2014-04-26"
## [6] "2014-04-25"
```


#### Computing and plotting incidence
We compute the daily incidence:

```r
i <- incidence(dat)
i
```

```
## <incidence object>
## [5888 cases from days 2014-04-07 to 2015-04-30]
## 
## $counts: matrix with 389 rows and 1 columns
## $n: 5888 cases in total
## $dates: 389 dates marking the left-side of bins
## $interval: 1 day
## $timespan: 389 days
```

```r
plot(i)
```

![plot of chunk incid1](figs/incid1-1.png)

Note that `i` case be subsetted easily; for instance, to keep the tail of the epidemics:

```r
i.tail <- subset(i, from=as.Date("2015-01-01"))
i.tail
```

```
## <incidence object>
## [1205 cases from days 2015-01-01 to 2015-04-30]
## 
## $counts: matrix with 120 rows and 1 columns
## $n: 1205 cases in total
## $dates: 120 dates marking the left-side of bins
## $interval: 1 day
## $timespan: 120 days
```

```r
plot(i.tail, border="white")
```

![plot of chunk tail](figs/tail-1.png)

Or, to focus on the peak of the distribution:

```r
plot(i[100:250]) 
```

![plot of chunk middle](figs/middle-1.png)

The daily incidence is quite noisy, but we can easily compute other incidence using larger time intervals:

```r
## weekly
i.7 <- incidence(dat, interval=7)
plot(i.7)
```

![plot of chunk interv](figs/interv-1.png)

```r
## bi-weekly
i.14 <- incidence(dat, interval=14)
plot(i.14, border = "white")
```

![plot of chunk interv](figs/interv-2.png)

```r
## period of 30 days
i.30 <- incidence(dat, interval=30)
plot(i.30, border = "white")
```

![plot of chunk interv](figs/interv-3.png)

`incidence` can also compute incidence by specified groups using the `groups` argument. For instance:

```r
i.7.sex <- incidence(dat, interval=7, groups = ebola.sim$linelist$gender)
i.7.sex
```

```
## <incidence object>
## [5888 cases from days 2014-04-07 to 2015-04-27]
## [2 groups: f, m]
## 
## $counts: matrix with 56 rows and 2 columns
## $n: 5888 cases in total
## $dates: 56 dates marking the left-side of bins
## $interval: 7 days
## $timespan: 389 days
```

```r
plot(i.7.sex, stack = TRUE)
```

![plot of chunk gender](figs/gender-1.png)



#### Modelling incidence

Incidence data, excluding zeros, can be modelled using log-linear regression of the form:
log(*y*) = *r* x *t* + *b*

where *y* is the incidence, *r* is the growth rate, *t* is the number of days since a specific point in time (typically the start of the outbreak), and *b* is the intercept.

Such model can be fitted to any incidence object using `fit`; for instance:

```r
fit(i.7)
```

```
## <incidence.fit object>
## 
## $lm: regression of log-incidence over time
## 
## $info: list containing the following items:
##   $r.day: 0.00072 (daily growth rate)
##   $r.day.conf: [0.00033 ; 0.00111] (confidence interval)
##   $doubling: 961.1 (doubling time in days)
##   $doubling.conf: [621.8 ; 2115.7] (confidence interval)
##   $pred: 4 predictions of incidence
```

```r
plot(i.7, fit=fit(i.7))
```

![plot of chunk fit1](figs/fit1-1.png)

Of course, in this case, the model does not make any sense, as it assumes a single trend.
As a more reasonable alternative, we could for instance try to fit a model to the first 20 weeks of the epidemic:

```r
fit(i.7[1:20])
```

```
## <incidence.fit object>
## 
## $lm: regression of log-incidence over time
## 
## $info: list containing the following items:
##   $r.day: 0.00454 (daily growth rate)
##   $r.day.conf: [0.00371 ; 0.00536] (confidence interval)
##   $doubling: 152.8 (doubling time in days)
##   $doubling.conf: [129.2 ; 186.9] (confidence interval)
##   $pred: 4 predictions of incidence
```

```r
plot(i.7[1:20], border="white", fit=fit(i.7[1:20])) + 
     labs(title = "Model fitted on the first 20 weeks")
```

![plot of chunk fit.early](figs/fit.early-1.png)


In this case, we would ideally like to fit two models, before and after the peak.
This is possible using the following approach:

```r
fit.both <- fit(i.7, split=as.Date("2014-10-15"))
fit.both
```

```
## $before
## <incidence.fit object>
## 
## $lm: regression of log-incidence over time
## 
## $info: list containing the following items:
##   $r.day: 0.00392 (daily growth rate)
##   $r.day.conf: [0.00344 ; 0.00439] (confidence interval)
##   $doubling: 177.0 (doubling time in days)
##   $doubling.conf: [157.7 ; 201.5] (confidence interval)
##   $pred: 4 predictions of incidence
## 
## $after
## <incidence.fit object>
## 
## $lm: regression of log-incidence over time
## 
## $info: list containing the following items:
##   $r.day: -0.00145 (daily growth rate)
##   $r.day.conf: [-0.00161 ; -0.00129] (confidence interval)
##   $halving: 478.3 (halving time in days)
##   $halving.conf: [538.4 ; 430.2] (confidence interval)
##   $pred: 4 predictions of incidence
```

```r
plot(i.7, fit=fit.both)
```

![plot of chunk fit.both](figs/fit.both-1.png)

This is much better, but the splitting date is not completely optimum. To look for the best possible splitting date (i.e. the one maximizing the average fit of both models), we use:

```r
best.fit <- fit.optim.split(i.7)
best.fit
```

```
## $df
##         dates   mean.R2
## 1  2014-08-04 0.7650406
## 2  2014-08-11 0.8203351
## 3  2014-08-18 0.8598316
## 4  2014-08-25 0.8882682
## 5  2014-09-01 0.9120857
## 6  2014-09-08 0.9246023
## 7  2014-09-15 0.9338797
## 8  2014-09-22 0.9339813
## 9  2014-09-29 0.9333246
## 10 2014-10-06 0.9291131
## 11 2014-10-13 0.9232523
## 12 2014-10-20 0.9160439
## 13 2014-10-27 0.9071665
## 
## $split
## [1] "2014-09-22"
## 
## $fit
## $fit$before
## <incidence.fit object>
## 
## $lm: regression of log-incidence over time
## 
## $info: list containing the following items:
##   $r.day: 0.00426 (daily growth rate)
##   $r.day.conf: [0.00373 ; 0.00479] (confidence interval)
##   $doubling: 162.7 (doubling time in days)
##   $doubling.conf: [144.6 ; 186.0] (confidence interval)
##   $pred: 4 predictions of incidence
## 
## $fit$after
## <incidence.fit object>
## 
## $lm: regression of log-incidence over time
## 
## $info: list containing the following items:
##   $r.day: -0.00145 (daily growth rate)
##   $r.day.conf: [-0.00158 ; -0.00133] (confidence interval)
##   $halving: 477.5 (halving time in days)
##   $halving.conf: [521.8 ; 440.1] (confidence interval)
##   $pred: 4 predictions of incidence
## 
## 
## $plot
```

![plot of chunk optim](figs/optim-1.png)

```r
plot(i.7, fit=best.fit$fit)
```

![plot of chunk optim](figs/optim-2.png)

These models are very good approximation of these data, showing a doubling time of 23 days during the first phase, and a halving time of 68 days during the second.


Note that `fit` will also take groups into account:

```r
best.fit2 <- fit.optim.split(i.7.sex)$fit
```

```
## Warning in fit(x, split = split): 3 dates with an incidence of 0 were
## removed before fitting

## Warning in fit(x, split = split): 3 dates with an incidence of 0 were
## removed before fitting

## Warning in fit(x, split = split): 3 dates with an incidence of 0 were
## removed before fitting

## Warning in fit(x, split = split): 3 dates with an incidence of 0 were
## removed before fitting

## Warning in fit(x, split = split): 3 dates with an incidence of 0 were
## removed before fitting

## Warning in fit(x, split = split): 3 dates with an incidence of 0 were
## removed before fitting

## Warning in fit(x, split = split): 3 dates with an incidence of 0 were
## removed before fitting

## Warning in fit(x, split = split): 3 dates with an incidence of 0 were
## removed before fitting

## Warning in fit(x, split = split): 3 dates with an incidence of 0 were
## removed before fitting

## Warning in fit(x, split = split): 3 dates with an incidence of 0 were
## removed before fitting

## Warning in fit(x, split = split): 3 dates with an incidence of 0 were
## removed before fitting

## Warning in fit(x, split = split): 3 dates with an incidence of 0 were
## removed before fitting

## Warning in fit(x, split = split): 3 dates with an incidence of 0 were
## removed before fitting

## Warning in fit(x, split = split): 3 dates with an incidence of 0 were
## removed before fitting
```

```r
best.fit2
```

```
## $before
## <incidence.fit object>
## 
## $lm: regression of log-incidence over time
## 
## $info: list containing the following items:
##   $r.day: 0.00340 (daily growth rate)
##    $r.day: 0.00377 (daily growth rate)
##   $r.day.conf: [0.00300 ; 0.00279] (confidence interval)
##   $doubling: 203.7 (doubling time in days)
##    $doubling: 183.7 (doubling time in days)
##   $doubling.conf: [145.9 ; 182.0] (confidence interval)
##   $pred: 5 predictions of incidence
## 
## $after
## <incidence.fit object>
## 
## $lm: regression of log-incidence over time
## 
## $info: list containing the following items:
##   $r.day: -0.00143 (daily growth rate)
##    $r.day: -0.00151 (daily growth rate)
##   $r.day.conf: [-0.00159 ; -0.00188] (confidence interval)
##   $halving: 484.1 (halving time in days)
##    $halving: 459.5 (halving time in days)
##   $halving.conf: [609.8 ; 542.5] (confidence interval)
##   $pred: 5 predictions of incidence
```

```r
plot(i.7.sex, fit=best.fit2)
```

![plot of chunk optim2](figs/optim2-1.png)


### Contributors (by alphabetic order):
- Thibaut Jombart (@thibautjombart)
- Rich Fitzjohn (@richfitz)

Maintainer (temporary): Thibaut Jombart (thibautjombart@gmail.com)
