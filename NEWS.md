
incidence 1.7.1
============================

### BUG FIXES

* Fix for a [bug in](https://github.com/reconhub/incidence/issues/119) `plot.incidence` 
introduced with new release of *ggplot2* ([bug report](https://github.com/tidyverse/ggplot2/issues/3873))
As a temporary fix, dates are now centered within the interval instead of to the left of the interval. 



incidence 1.7.0 
============================

### NEW FEATURES

 * Any interval `seq.Date()` can handle (e.g. "5 weeks") can be handled by
   `incidence()` (see https://github.com/reconhub/incidence/issues/67)
 * Weekly intervals can start on any day of the week by allowing things like 
   "epiweek", "isoweek", "wednesday week", "2 Saturday weeks", etc.
   (see https://github.com/reconhub/incidence/issues/55#issuecomment-405297526)
 * the item `$weeks` is now added to the incidence object, which contains an
   "aweek" class
 * plotting will now force the first tick to be the starting point of the
   incidence curve

### NEW FUNCTIONS

 * `make_breaks()` will automatically calculate breaks from an incidence object
   for plotting. 
 * `scale_x_incidence()` will produce a ggplot2 "ScaleContinuous" object to add
   to a ggplot.

### DEPRECATED

 * `plot.incidence()` argument `labels_iso` is deprecated in favor of 
   `labels_week`
 * Incidence objects will still have `$isoweeks` if the weeks are ISO 8601
   standard, but users should rely intead on `$weeks` instead. The `$isoweeks`
   element will be removed in a future version of incidence.
 * `as.incidence()` argument `isoweeks` has been deprecated in favour of 
   `standard`

### DEPENDENCIES

 - ISOweek import changed to [aweek](https://www.repidemicsconsortium.org/aweek)

### Documentation

 - Vignettes have been updated with examples.

incidence 1.6.0 (2019-03-05) 
============================

### BEHAVIORAL CHANGE

* `incidence()` will no longer allow a non-standard `first_date` to override
  `standard = TRUE. The first call to `incidence()` specifying `first_date` 
  without `standard` will issue a warning. To use non-standard first dates, 
  specify `standard = FALSE`. To remove the warning, use
  `options(incidence.warn.first_date = FALSE)`. See
  https://github.com/reconhub/incidence/issues/87 for details.  

### MISC

* `citation("incidence")` will now give the proper citation for our article in
  F1000 research and the global DOI for archived code. See
  https://github.com/reconhub/incidence/pulls/106
* Tests have been updated to avoid randomisation errors on R 3.6.0
  See https://github.com/reconhub/incidence/issues/107


incidence 1.5.4 (2019-01-15)
============================

### BUG FIX

* `incidence()` now returns an error when supplied a character vector that is
  not formatted as (yyyy-mm-dd).
  (See https://github.com/reconhub/incidence/issues/88)
* `fit()` now returns correct coefficients when dates is POSIXt by converting to
  Date. (See https://github.com/reconhub/incidence/issues/91)
* `plot.incidence()` now plots in UTC by default for POSIXt incidence objects.
  this prevents a bug where different time zones would cause a shift in the bars
  (See https://github.com/reconhub/incidence/issues/99).

### MISC

* A test that randomly failed on CRAN has been fixed.
  (See https://github.com/reconhub/incidence/issues/95).
* Plotting tests have been updated for new version of vdiffr
  (See https://github.com/reconhub/incidence/issues/96).
* POSIXct incidence are first passed through POSIXlt when initialized.
* A more informative error message is generated for non ISO 8601 formatted 
  `first_date` and `last_date` parameters.

incidence 1.5.3 (2018-12-07)
============================

### BUG FIX

* `plot.incidence()` will now respect single groups.
  (See https://github.com/reconhub/incidence/issues/84)
* `as.data.frame.incidence()` will now respect single groups.
  (See https://github.com/reconhub/incidence/issues/84)

### MISC

* `demo("incidence-demo" package = "incidence")` has been updated to show use of
  custom colors.

incidence 1.5.2 (2018-11-30)
============================

### BUG FIX

* `print.incidence()` will now print isoweeks even if the `$interval` element is
  "week".

### MISC

* `subset.incidence()` will now give a more informative error message when the
  user specifies a group that does not exist. 
* `demo('incidence-demo', package = 'incidence')` now shows plotting with 
  `show_cases = TRUE`.
* In the the case where a date is accidentally mis-typed leading to a gross
  mis-calculation of the date range (i.e. 2018 is mis-typed as 3018), a warning
  will be issued. The default threshold is set at 18262 days (50 years), but the
  user can define their own threshold by setting the `incidence.max.days` option

incidence 1.5.1 (2018-11-14)
============================

### BUG FIX

* Two bugs regarding the ordering of groups when the user specifies a factor/
  column order have been fixed. This affects `plot.incidence()`, `incidence()`,
  and `as.data.frame.incidence()` For details, see
  https://github.com/reconhub/incidence/issues/79

incidence 1.5.0 (2018-11-01)
============================

### NEW FUNCTIONS

* `group_names()` allows the user to retrieve and set the group names.
* `get_timespan()` returns the `$timespan` element.
* `get_n()` returns the `$n` element.
* `dim()`, `nrow()`, and `ncol()` are now available for incidence objects,
  returning the dimensions of the number of bins and the number of groups.

### NEW FEATURES

* A new argument to `plot()` called `show_cases` has been added to draw borders
  around individual cases for EPIET-style curves.
  See https://github.com/reconhub/incidence/pull/72 for details.

### DOCUMENTATION UPDATES

* An example of EPIET-style bars for small data sets has been added to the 
  plot customisation vignette by @jakobschumacher.
  See https://github.com/reconhub/incidence/pull/68 for details.
* The incidence class vignette has been updated to use the available accessors.

### BUG FIX

* `estimate_peak()` no longer fails with integer dates
* `incidence()` no longer fails when providing both group information and a
  `first_date` or `last_date` parameter that is inside the bounds of the
  observed dates. Thanks to @mfaber for reporting this bug. 
  See https://github.com/reconhub/incidence/issues/70 for details.

### MISC

* code has been spread out into a more logical file structure where the 
  `internal_checks.R` file has been split into the relative components.
* A message is now printed if missing observations are present when
  creating the incidence object.

incidence 1.4.1 (2018-08-24)
============================

### BEHAVIORAL CHANGES

* The `$lm` field of the `incidence_fit` class is now named `$model` to clearly
  indicate that this can contain any model. 

### NEW FEATURES

* `incidence()` will now accept text-based intervals that are valid date
  intervals: day, week, month, quarter, and year. 

* `incidence()` now verifies that all user-supplied arguments are accurate
  and spelled correctly. 

* `fit_optim_split()` now gains a `separate_split` argument that will determine
  the optimal split separately for groups.

* A new class, `incidence_fit_list`, has been implemented to store and summarise
  `incidence_fit` objects within a nested list. This is the class returned by
  in the `$fit` element of `fit_optim_split()`.

### NEW FUNCTIONS

* `bootstrap()` will bootstrap epicurves stored as `incidence` objects.

* `find_peak()` identifies the peak date of an `incidence` objects.

* `estimate_peak()` uses bootstrap to estimate the peak time of a
  partially observed outbreak.
  
* `get_interval()` will return the numeric interval or several
  intervals in the case of intervals that can't be represented in a fixed
  number of days (e.g. months).

* `get_dates()` returns the dates or counts of days on the right,
  center, or left of the interval.

* `get_counts()` returns the matrix of case counts for each date.

* `get_fit()` returns a list of `incidence_fit` objects from an
  `incidence_fit_list` object.

* `get_info()` returns information stored in the `$info` element of an 
  `incidence_fit`/`incidence_fit_list` object. 

### DOCUMENTATION

* The new vignette `incidence_fit_class` instructs the user on how 
 `incidence_fit` and `incidence_fit_list` objects are created and accessed. 

### DEPRECATED

* In the `incidence()` function, the `iso_week` parameter is deprecated in
  favor of `standard` for a more general way of indicating that the
  interval should start at the beginning of a valid date timeframe.

### BUG FIXES

* The `$timespan` item in the incidence object from Dates was not type-stable
  and would change if subsetted. A re-working of the incidence constructor
  fixed this issue. 

* Misspelled or unrecgonized parameters passed to `incidence()` will now cause
  an error instead of being silently ignored.

* Plotting for POSIXct data has been fixed.


incidence 1.3.1 (2018-06-11)
============================

### BUG FIXES

* tweak of the plotting of `incidence` object to avoid conflicts with additional
  geoms such as `geom_ribbon`, now used in `projections::add_projections`.




incidence 1.3.0 (2018-06-01)
============================

### BUG FIXES

* fixed [issue](https://github.com/reconhub/incidence/issues/34) caused by new
  version of `ggplot2`
  
### NEW FEATURES

* the argument `n_breaks` has been added to `plot.incidence`, to specify the
  ideal number of breaks for the date legends; will work with ggplot2 > 2.2.1
  
* added the internal function `make_iso_weeks_breaks` to generate dates and
  labels for date x-axis legends using ISO weeks

* added a function `add_incidence_fit`, which can be used for adding fits to
  epicurves in a piping-friendly way

* added a function `cumulate`, which computes cumulative incidence and returns
  an `incidence` object
  



incidence 1.2.1 (2017-10-19)
============================

### BUG FIXES

* fixed issues in testing incidence plots by employing *vdiffr* package.




incidence 1.2.0 (2017-04-03)
============================

### NEW FEATURES

* new generic *as.incidence*, to create incidence objects from already computed
  incidences. Methods for: matrix, data.frame, numeric vectors

* better processing of input dates, including: automatic conversion from
  characters, issuing errors for factors, and silently converting numeric
  vectors which are essentially integers (issuing a warning otherwise)

* new vignette on
  [*conversions*](http://www.repidemicsconsortium.org/incidence/articles/conversions.html)
  to and from *incidence* objects

* new tests


### BUG FIXES

* fixed issues caused by variables which changed names in some datasets of the
  *outbreaks* package, used in the documentation

* disabled by default the isoweeks in `incidence`; this part of the code will
  break with changes made in the devel version of *ggplot2*, which is now
  required by *plotly*
  



incidence 1.1.2 (2017-03-24)
==================

### BUG FIXES

* it is now possible to subset an incidence object based on `Date` dates using
  numeric values, which are interpreted as number of intervals since the first
  date (origin = 1)

* NAs are no longer removed from the input dates, as it would cause mismatches
  with grouping factors.




incidence 1.1.1 (2017-02-15)
==================

### BUG FIXES

* adapting to new names of datasets in
  [*outbreaks*](http://repidemicsconsortium.org/outbreaks): `ebola.sim` ->
  `ebola_sim` and `ebola.sim.clean` -> `ebola_sim_clean`



incidence 1.1.0 (2016-12-13)
==================

### NEW FEATURES

* add an argument `iso_week` to incidence.Date() and incidence.POSIXt() to
  support ISO week-based incidence when computing weekly incidence.

* add an argument `labels_iso_week` to plot.incidence() to label x axis tick
  marks with ISO weeks when plotting ISO week-based weekly incidence.


<br>

incidence 1.0.1 (2016-11-23)
==================

### NEW FEATURES

* The README.Rmd / README.md now contains information about various websites for
  *incidence* as well as guidelines for posting questions on the RECON forum.

* incidence now has a dedicated website
  [http://www.repidemicsconsortium.org/incidence/](http://www.repidemicsconsortium.org/incidence/)
  generated with *pkgdown*


### MINOR IMPROVEMENTS

* Vignettes titles are now correctly displayed on CRAN (they read '*Vignette title*').


<br>

incidence 1.0.0 (2016-11-03)
==================
First release of the incidence package on CRAN!
