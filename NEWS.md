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

