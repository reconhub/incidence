
![incidence logo](http://raw.githubusercontent.com/reconhub/incidence/master/logo/logo.png)

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

The main functions of the package include:

- **`incidence`**: compute incidence from dates in various formats; any fixed time interval can be used; the returned object is an instance of the (S3) class *incidence*.
- **`plot`**: this method (see `?plot.incidence` for details) plots *incidence* objects, and can also add predictions of the model(s) contained in an  *incidence.fit* object (or a list of such objects).
- **`fit`**: fit one or two exponential models (i.e. linear regression on log-incidence) to an *incidence* object; two models are calibrated only if a date is provided to split the time series in two (argument `split`); this is typically useful to model the two phases of exponential growth, and decrease of an outbreak; each model returned is an instance of the (S3) class *incidence.fit*, each of which contains various useful information (e.g. growth rate *r*, doubling/halving time, predictions and confidence intervals).
- **`fit.optim.split`**: finds the optimal date to split the time series in two, typically around the peak of the epidemic.
- **`[`**: lower-level subsetan of *incidence* objects, permiting to specify which dates and groups to retain; uses a syntax similar to matrices, i.e. `x[i, j]`, where `x` is the *incidence* object, `i` a subset of dates, and `j` a subset of groups.
- **`subset`**: subset an *incidence* object by specifying a time window.
- **`pool`**: pool incidence from different groups into one global incidence time series.




# Worked example: simulated Ebola outbreak

## Loading the data

This example uses the simulated Ebola Virus Disease (EVD) outbreak from the package
[*outbreaks*](http://github.com/reconhub/outbreaks). We will compute incidence for various time
steps, calibrate two exponential models around the peak of the epidemic, and analyse the results.

First, we load the data:

```r
library(outbreaks)
library(ggplot2)
```

```
## Find out what's changed in ggplot2 at
## http://github.com/hadley/ggplot2/releases.
```

```r
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


## Computing and plotting incidence
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
plot(i.7.sex, stack = TRUE, border = "grey")
```

![plot of chunk gender](figs/gender-1.png)

```r
i.7.hosp <- with(ebola.sim.clean$linelist, 
	 incidence(date.of.onset, interval=7, groups = hospital))
i.7.hosp
```

```
## <incidence object>
## [5829 cases from days 2014-04-07 to 2015-04-27]
## [6 groups: Connaught Hospital, Military Hospital, NA, other, Princess Christian Maternity Hospital (PCMH), Rokupa Hospital]
## 
## $counts: matrix with 56 rows and 6 columns
## $n: 5829 cases in total
## $dates: 56 dates marking the left-side of bins
## $interval: 7 days
## $timespan: 389 days
```

```r
head(i.7.host$counts)
```

```
## Error in head(i.7.host$counts): object 'i.7.host' not found
```

```r
plot(i.7.host, stack=TRUE) + 
    theme(legend.position= "top") + 
    labs(fill="")
```

```
## Error in plot(i.7.host, stack = TRUE): object 'i.7.host' not found
```



## Handling `incidence` objects
`incidence` objects can be manipulated easily. The `[` operator implements subetting of dates (first argument) and groups (second argument). 
For instance, to keep only the peak of the distribution:

```r
i[100:250]
```

```
## <incidence object>
## [4103 cases from days 2014-07-15 to 2014-12-12]
## 
## $counts: matrix with 151 rows and 1 columns
## $n: 4103 cases in total
## $dates: 151 dates marking the left-side of bins
## $interval: 1 day
## $timespan: 151 days
```

```r
plot(i[100:250])
```

![plot of chunk middle](figs/middle-1.png)
Or to keep every other week:

```r
i.7[c(TRUE,FALSE)]
```

```
## <incidence object>
## [2891 cases from days 2014-04-07 to 2015-04-20]
## 
## $counts: matrix with 28 rows and 1 columns
## $n: 2891 cases in total
## $dates: 28 dates marking the left-side of bins
## $interval: 7 days
## $timespan: 379 days
```

```r
plot(i.7[c(TRUE,FALSE)])
```

![plot of chunk stripes](figs/stripes-1.png)

Some temporal subsetting can be even simpler using `subset`, which permits to retain data within a specified time window:

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

Subsetting groups can also matter. For instance, let's try and visualise the incidence based on onset of symptoms by outcome:

```r
i.7.outcome <- incidence(dat, 7, groups=ebola.sim$linelist$outcome)
i.7.outcome
```

```
## <incidence object>
## [5888 cases from days 2014-04-07 to 2015-04-27]
## [3 groups: Death, NA, Recover]
## 
## $counts: matrix with 56 rows and 3 columns
## $n: 5888 cases in total
## $dates: 56 dates marking the left-side of bins
## $interval: 7 days
## $timespan: 389 days
```

```r
plot(i.7.outcome, stack = TRUE, border = "grey")
```

![plot of chunk i7outcome](figs/i7outcome-1.png)
By default, `incidence` treats missing data (NA) as a separate group (see argument `na.as.group`). We could disable this to retain only known outcomes, but alternatively we can simply subset the object to exclude the last (3rd) group:

```r
i.7.outcome[,1:2]
```

```
## <incidence object>
## [3905 cases from days 2014-04-07 to 2015-04-27]
## [2 groups: Death, NA]
## 
## $counts: matrix with 56 rows and 2 columns
## $n: 3905 cases in total
## $dates: 56 dates marking the left-side of bins
## $interval: 7 days
## $timespan: 386 days
```

```r
plot(i.7.outcome[,1:2], stack = TRUE, border = "grey")
```

![plot of chunk groupsub](figs/groupsub-1.png)

Groups can also be collapsed into a single time series using `pool`:

```r
i.pooled <- pool(i.7.outcome)
i.pooled
```

```
## <incidence object>
## [5888 cases from days 2014-04-07 to 2015-04-27]
## 
## $counts: matrix with 56 rows and 1 columns
## $n: 5888 cases in total
## $dates: 56 dates marking the left-side of bins
## $interval: 7 days
## $timespan: 389 days
```

```r
identical(i.7$counts, i.pooled$counts)
```

```
## [1] TRUE
```



## Modelling incidence

Incidence data, excluding zeros, can be modelled using log-linear regression of the form:
log(*y*) = *r* x *t* + *b*

where *y* is the incidence, *r* is the growth rate, *t* is the number of days since a specific point in time (typically the start of the outbreak), and *b* is the intercept.

Such model can be fitted to any incidence object using `fit`.
Of course, a single log-linear model is not sufficient for modelling our time series, as there is clearly an growing and a decreasing phase.
As a start, we can calibrate a model on the first 20 weeks of the epidemic:



```r
plot(i.7[1:20])
```

![plot of chunk fit1](figs/fit1-1.png)

```r
early.fit <- fit(i.7[1:20])
early.fit
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

The resulting objects can be plotted, in which case the prediction and its confidence interval is displayed:


```r
plot(early.fit)
```

![plot of chunk unnamed-chunk-2](figs/unnamed-chunk-2-1.png)

However, a better way to display these predictions is adding them to the incidence plot using the argument `fit`:

```r
plot(i.7[1:20], fit = early.fit)
```

![plot of chunk unnamed-chunk-3](figs/unnamed-chunk-3-1.png)


In this case, we would ideally like to fit two models, before and after the peak of the epidemic.
This is possible using the following approach, if you know what date to use to split the data in two phases:

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

This is much better, but the splitting date is not completely optimal. To look for the best possible splitting date (i.e. the one maximizing the average fit of both models), we use:

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

These models are very good approximation of these data, showing a doubling time of 162.7 days during the first phase, and a halving time of 477.5 days during the second.


Note that `fit` will also take groups into account if incidence has been computed for several groups:

```r
best.fit2 <- fit.optim.split(i.7.sex)$fit
```

```
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

Maintainer: Thibaut Jombart (thibautjombart@gmail.com)
