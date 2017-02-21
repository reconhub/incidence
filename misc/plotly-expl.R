
## load libraries
## if not installed, use install.packages("name of the package")
## e.g. install.packages("incidence")

library(outbreaks)
library(incidence)
library(ggplot2)
library(plotly)


## generate incidence object
dat <- ebola.sim$linelist$date.of.onset
i.7.sex <- incidence(dat, interval = 7,
                     groups = ebola.sim$linelist$gender)

## generate best fit
fit <-  fit_optim_split(i.7.sex)$fit

## generate ggplot object
p <- plot(i.7.sex, fit = fit)


## generate plotly object (used for JS rendering)
p_ly <- ggplotly(p)

## to display the plot: print(p_ly)
## (implicitely called when typing p_ly and return

## to display the content: unclass(p_ly)

## Wishlist for improving the graph:

## - adjust x labels to zoom
## - modify the text when hovering
## - modify the legend, e.g. "(f,1)" -> "f"
