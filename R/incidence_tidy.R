#' tidy up incidence outputs
#'
#' @description functions to get tibble outputs from incidence_fit objects
#'
#' @describeIn get_info_tidy generates a tibble of incidence::get_info()
#'
#' @param incidence_fit incidence_fit or incidence_fit_list object
#' @param what character string of parameter of interest
#'
#' @return incidence tibbles!
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import tibble
#' @import stringr
#' @import magrittr
#'
#' @examples
#'
#' # example outbreak --------------------------------------------------------
#'
#' library(outbreaks) #sample data
#' library(incidence) #core functions
#' # library(avallecam) #improvements
#' dat <- ebola_sim$linelist$date_of_onset
#' i.7 <- incidence(dat, interval=7)
#' # plot(i.7)
#' f1 <- fit(i.7[1:20])
#' # f1
#' f2 <- fit_optim_split(i.7)
#' # f2
#'
#' # get_info_tidy: tibble outputs from fit object ---------------------------
#'
#' # original
#' get_info(f1,what = "r")
#'
#' # new improvement
#' get_info_tidy(f1,what = "r")
#'
#' # for incidence::fit_optim_split outputs use .$fit
#'
#' # original
#' get_info(f2$fit, what = "r")
#'
#' #improvement
#' get_info_tidy(f2$fit, what = "r")
#'
#' # try more examples:
#'
#' # get_info(f2$fit, what = "r.conf")
#' # get_info_tidy(f2$fit, what = "r.conf")
#' # get_info_tidy(f2$fit, what = "doubling")
#' # get_info_tidy(f2$fit, what = "doubling.conf")
#' # get_info_tidy(f2$fit, what = "halving")
#' # get_info_tidy(f2$fit, what = "halving.conf")
#'
#' # tidy_incidence: all key parameters in tibble ----------------------------
#'
#' tidy_incidence(incidence_fit = f1)
#' tidy_incidence(incidence_fit = f2$fit)
#'
#' # glance_incidence: check model performance -------------------------------
#'
#' glance_incidence(incidence_fit = f1)
#' glance_incidence(incidence_fit = f2$fit)
#'
#' @export get_info_tidy
#' @export tidy_incidence
#' @export glance_incidence

get_info_tidy <- function(incidence_fit,what) {
  incidence_fit %>%
    get_info(what = what) %>%
    as.data.frame() %>%
    magrittr::set_colnames(str_c(colnames(.))) %>%
    rownames_to_column(var = "mark") %>%
    as_tibble() %>%
    mutate(parameter=what) %>%
    select(mark,parameter,everything()) %>%
    pivot_longer(cols = c(-mark,-parameter),
                 names_to = "key",
                 values_to = "value") %>%
    mutate(parameter=str_replace(parameter,"(.+)\\.(.+)","\\1"),
           key=case_when(
             str_detect(key,"2.5")~"conf.lower",
             str_detect(key,"97.5")~"conf.upper",
             TRUE~"estimate"
           ),
           key=str_replace_all(key,"\\.","_"))
}

#' @describeIn get_info_tidy generates a complete summary tibble from incidence fit paramteter estimates
#' @inheritParams get_info_tidy

tidy_incidence <- function(incidence_fit) {
  test_fit <- incidence_fit
  name_tibble <- tibble(names=c("r","r.conf","doubling","doubling.conf"))

  if (class(test_fit)=="incidence_fit_list") {
    name_tibble <- name_tibble %>% add_row(names=c("halving","halving.conf"))
  }

  name_tibble %>%
    mutate(output=map(.x = names,
                      .f = ~get_info_tidy(incidence_fit = test_fit,what = .x))) %>%
    unnest(cols = output) %>%
    select(-names) %>%
    pivot_wider(names_from = key,values_from = value)
}

#' @describeIn get_info_tidy generates a complete summary tibble from incidence fit model performance
#' @inheritParams get_info_tidy

glance_incidence <- function(incidence_fit) {
  test_fit <- incidence_fit
  if (class(test_fit)=="incidence_fit_list") {
    out <- expand_grid(fit=list(test_fit),
                       names=c("before","after")) %>%
      mutate(time=map2(.x = fit,.y = names,.f = ~pluck(.x,.y))) %>%
      mutate(model=map(.x = time,.f = ~pluck(.x,"model"))) %>%
      mutate(glance=map(.x = model,.f = broom::glance)) %>%
      unnest(cols = glance) %>%
      select(-fit,-time,-model)
  } else {
    out <- test_fit$model %>% broom::glance()
  }

  return(out)
}
