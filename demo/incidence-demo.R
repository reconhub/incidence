#' Example 1 ------------------------------------------------------------------
#'
#' **computing and manipulating stratified weekly incidence**
#'
#' 1) import data
#'
library('outbreaks')

dat1 <- ebola_sim_clean$linelist
str(dat1, strict.width = "cut", width = 76)

#' 2) build an incidence object
#'
#+ incidence-curve, fig.width=9, fig.height=5
library('incidence')
library('ggplot2')

# compute weekly stratified incidence
i.7.group <- incidence(dat1$date_of_onset,
                       interval = 7L,
                       groups   = dat1$hospital)
# print incidence object
i.7.group

# plot incidence object
my_theme <- theme_bw(base_size = 12) +
  theme(legend.position = c(.8, .7)) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(color = "black"))
plot(i.7.group, border = "white") + my_theme

#' 3) Manipulate incidence object
#'
#+ incidence-early-curve, fig.width=6, fig.height=7
# plot the first 8 weeks
plot(i.7.group[1:8, ], show_cases = TRUE) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, color = "black")) +
  theme(panel.grid.minor = element_blank())

# exclude NA group by disabling treating NA as a separate group
i.7.group0 <- incidence(dat1$date_of_onset,
                        interval    = 7L,
                        groups      = dat1$hospital,
                        na_as_group = FALSE)

# exclude NA using [ operator
i.7.group1 <- subset(i.7.group, groups = -ncol(i.7.group))

# exclude NA group using [ operator
i.7.group2 <- i.7.group[, -ncol(i.7.group)]

# check the resulting incidence objects are identical
identical(i.7.group0$counts, i.7.group1$counts)
identical(i.7.group1, i.7.group2)

# check groups
colnames(i.7.group1$counts)

#' Example 2 ------------------------------------------------------------------
#'
#' **importing pre-computed daily incidence and fitting log-linear model**
#'
#' 1) Import pre-computed daily incidence
#'
#+ incidence-curve2, fig.width=9, fig.height=5
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
	plot(i.group, border = "white")  + my_theme,
	plot(i.pooled, border = "white") + my_theme,
	ncol       = 1,
	labels     = c("(A)", "(B)"),
	label_size = 16,
	label_x    = 0.06,
	label_y    = 0.94
)

#' 2) Fit log-linear regression model
#'
#+ incidence-fit, fig.width=9, fig.height=4
library('magrittr')

fos <- fit_optim_split(i.pooled)
fos$split
fos$fit
plot(i.pooled, border = "white") %>%
  add_incidence_fit(fos$fit) +
  my_theme

