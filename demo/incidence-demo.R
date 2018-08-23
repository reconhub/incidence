#' Example 1 ------------------------------------------------------------------
#' 
#' **computing and manipulating stratified weekly incidence**
#' 
#' 1) import data
#'
library('outbreaks')

dat1 <- ebola_sim_clean$linelist
str(dat1)

#' 2) build an incidence object
#'
library('incidence')
library('ggplot2')

# compute weekly stratified incidence
i.7.group <- incidence(dat1$date_of_onset, 
		       interval = 7L,
                       groups = dat1$hospital)
# print incidence object
i.7.group

# plot incidence object
my_theme <- theme_bw(base_size = 16) + 
  theme(legend.position = c(.8, .8)) 
plot(i.7.group, border = "white") + my_theme

#' 3) Manipulate incidence object
#'
# exclude NA group by disabling treating NA as a separate group
i.7.group0 <- incidence(dat1$date_of_onset, interval = 7L,
                       groups = dat1$hospital, na_as_group = FALSE)

# exclude NA using [ operator
i.7.group1 <- subset(i.7.group, groups = -3)

# exclude NA group using [ operator
i.7.group2 <- i.7.group[, -3]

#' Example 2 ------------------------------------------------------------------
#'
#' **importing pre-computed daily incidence and fitting log-linear model**
#'
#' 1) Import pre-computed daily incidence
#'
# preview datasets
head(zika_girardot_2015, 3)
head(zika_sanandres_2015, 3)

# combine two datasets into one
dat2 <- merge(zika_girardot_2015, zika_sanandres_2015, by = "date", all = TRUE)

# rename variables
names(dat2)[2:3] <- c("Girardot", "San Andres")

# replace NA with 0
dat2[is.na(dat2)] <- 0

# convert pre-computed incidence in data.frame into incidence object 
# grouped by locations
i.group <- as.incidence(x = dat2[, 2:3], dates = dat2$date)

# pool incidence across two locations
i.pooled <- pool(i.group)
cowplot::plot_grid(
	plot(i.group, border = "white") + my_theme,
	plot(i.pooled, border = "white") + my_theme,
	ncol = 1,
	labels = c("(A)", "(B)"),
	label_size = 16,
	label_x = 0.06,
	label_y = 0.94
)

#' 2) Fit log-linear regression model
#' 
library('magrittr')

fos <- fit_optim_split(i.pooled)
fos$split
fos$fit
plot(i.pooled, border = "white") %>%
  add_incidence_fit(fos$fit) +
  my_theme
