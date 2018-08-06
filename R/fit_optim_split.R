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
    # Calculate split for each group separately --------------------------------
    res        <- vector(mode = "list", length = ncol(x$counts))
    names(res) <- colnames(x$counts)
    for (i in names(res)) {
      res[[i]] <- fit_optim_split(x[, i], separate_split = FALSE, plot = FALSE)
    }

    # Rearrange data -----------------------------------------------------------
    # The resulting object will have the follwing structure
    # $df
    # $plot
    #     $group1 <ggplot>
    #     $group2 <ggplot>
    # $split
    # $fit <incidence_fit_list>
    #     $group1 <incidence_fit_list>
    #         $before <incidence_fit>
    #         $after  <incidence_fit>
    #     $group2 <incidence_fit_list>
    #         $before <incidence_fit>
    #         $after  <incidence_fit>
    dates  <- get_dates(x)[[1]]
    dfrows <- vapply(res, function(i) nrow(i$df), integer(1))
    out <- list(
      df = data.frame(dates = seq(dates, by = 1, length.out = sum(dfrows)),
                      mean.R2 = vector(mode = "numeric", length = sum(dfrows)),
                      groups = factor(rep(names(res), dfrows), names(res))
                     ),
      plot = ggplot2::ggplot(),
      split = seq(dates, by = 1, length.out = length(res)),
      fit = vector(mode = "list", length = length(res))
    )
    names(out$fit)   <- names(res)
    names(out$plot)  <- names(res)
    names(out$split) <- names(res)
    for (i in names(res)) {
      n <- factor(i, names(res))
      out$fit[[i]]   <- res[[i]]$fit
      out$plot[[i]]  <- res[[i]]$plot
      out$split[[i]] <- res[[i]]$split
      out$fit[[i]]$after$info$pred$groups  <- n
      out$fit[[i]]$before$info$pred$groups <- n
      out$df[out$df$groups == i, ]$dates   <- res[[i]]$df$dates
      out$df[out$df$groups == i, ]$mean.R2 <- res[[i]]$df$mean.R2
    }
    if (plot) {
      out$plot <- ggplot2::ggplot(
          out$df,
          ggplot2::aes_string(x = "dates", y = "mean.R2", color = "groups")
        ) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::geom_text(ggplot2::aes_string(label="dates"),
                           hjust = -0.1, angle = 35
        ) +
        ggplot2::ylim(min = min(out$df$mean.R2) - 0.1, max = 1)
    } else {
      out$plot <- NULL
    }

    # Adding attributes for incidence_fit_list ---------------------------------
    attr(out$fit, "locations") <- c(
      lapply(names(res), c, "before"),
      lapply(names(res), c, "after")
    )
    class(out$fit) <- "incidence_fit_list"
    return(out)
  }
  date.peak <- x$dates[which.max(pool(x)$counts)]
  try.since <- date.peak - window / 2
  try.until <- date.peak + window / 2
  to.keep <- x$dates >= try.since & x$dates <= try.until
  if (sum(to.keep) < 1) {
    stop("No date left to try after defining splits to try.")
  }

  splits.to.try <- x$dates[to.keep]
  need.to.try   <- length(splits.to.try) > 1

  f <- function(split) {
    fits <- fit(x, split = split, quiet = quiet)
    mean(vapply(fits, function(e) summary(e$lm)$`adj.r.squared`, double(1)), na.rm = TRUE)
  }

  results <- vapply(splits.to.try, f, double(1))

  ## shape output
  df <- data.frame(dates = splits.to.try, mean.R2 = results)
  split <- if (need.to.try) splits.to.try[which.max(results)] else splits.to.try
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
