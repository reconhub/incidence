## Notes to CRAN

## Test environments

* ubuntu 16.04, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.5.1
* win-builder (devel [2018-10-30 r75516] and release)

## R CMD check results

0 errors | 0 warnings | 0 note

## CRAN check results

There is currently an error on r-oldrel-windows-ix86+x86_64 that appears to be a false positive:

> Error in namespaceExport(ns, exports) :
>  undefined exports: %s!=%, %s!==%, %s+%, %s<%, %s<=%, %s==%, %s===%, %s>%, %s>=%, %stri!=%,

https://www.r-project.org/nosvn/R.check/r-oldrel-windows-ix86+x86_64/incidence-00check.html

This originates from a failed build of the 'stringi' package:

https://www.r-project.org/nosvn/R.check/r-oldrel-windows-ix86+x86_64/stringi-00install.html


