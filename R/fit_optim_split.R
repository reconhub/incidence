#' @export
#' @rdname fit
#'
#' @param window The size, in days, of the time window either side of the
#' split.
#'
#' @param plot A logical indicating whether a plot should be added to the
#' output (`TRUE`, default), showing the mean R2 for various splits.
#'
#' @param separate_split If groups are present, should separate split dates be
#' determined for each group? Defaults to `TRUE`, in which separate split dates
#' and thus, separate models will be constructed for each group. When `FALSE`,
#' the split date will be determined from the pooled data and modelled with the
#' groups as main effects and interactions with date.
#'
fit_optim_split <- function(x, window = x$timespan/4, plot = TRUE,
                            quiet = TRUE, separate_split = TRUE) {
  if (ncol(x$counts) > 1 && separate_split) {
    res <- vector(mode = "list", length = ncol(x$counts))
    names(res) <- colnames(x$counts)
    for (i in names(res)) {
      res[[i]] <- fit_optim_split(x[, i], separate_split = FALSE)
      # hack for groups... this should be fixed.
      res[[i]]$fit$before$info$pred$groups <- factor(i, names(res))
      res[[i]]$fit$after$info$pred$groups  <- factor(i, names(res))
    }
    attr(res, "locations") <- c(
      lapply(names(res), c, c("fit", "before")),
      lapply(names(res), c, c("fit", "after"))
    )
    class(res) <- "incidence_fit_list"
    return(res)
  }
  date.peak <- x$dates[which.max(pool(x)$counts)]
  try.since <- date.peak - window / 2
  try.until <- date.peak + window / 2
  to.keep <- x$dates >= try.since & x$dates <= try.until
  if (sum(to.keep) < 1) {
    stop("No date left to try after defining splits to try.")
  }

  splits.to.try <- x$dates[to.keep]

  f <- function(split) {
    fits <- fit(x, split=split, quiet = quiet)
    mean(vapply(fits, function(e) summary(e$lm)$`adj.r.squared`, double(1)))
  }

  results <- vapply(splits.to.try, f, double(1))

  ## shape output
  df <- data.frame(dates = splits.to.try, mean.R2 = results)
  split <- splits.to.try[which.max(results)]
  fit <- suppressWarnings(fit(x, split = split))
  out <- list(df = df,
              split = split,
              fit = fit)

  if (plot) {
    out$plot <- ggplot2::ggplot(
      df, ggplot2::aes_string(x = "dates", y = "mean.R2")) +
      ggplot2::geom_point() + ggplot2::geom_line() +
      ggplot2::geom_text(ggplot2::aes_string(label="dates"),
                         hjust=-.1, angle=35) +
      ggplot2::ylim(min=min(results)-.1, max=1)
  }

  out
}
