incidence 1.4.1.99
============================

### NEW FUNCTIONS

* `group_names()` allows the user to retrieve and set the group names
* `n_groups()` returns the number of groups.

### BUG FIX

* `estimate_peak()` no longer fails with integer dates
* `incidence()` no longer fails when providing both group information and a
  `first_date` or `last_date` parameter that is inside the bounds of the
  observed dates. Thanks to @mfaber for reporting this bug. 
  See https://github.com/reconhub/incidence/issues/70 for details.

### MISC

* code has been spread out into a more logical file structure where the 
  `internal_checks.R` file has been split into the relative components.

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

