get_info <- function(x, what = "r", ...) {
  UseMethod("get_info")
}

get_info.incidence_fit <- function(x, what = "r") {
  x$info[[what]]
}

get_info.incidence_fit_list <- function(x, what = "r", group = 1L, na.rm = TRUE) {
  locations <- attr(x, "locations")
  n <- length(locations)
  if (what == "pred") {
    fits <- get_fit(x)
    for (i in names(fits)) {
      fits[[i]] <- fits[[i]]$info$pred
      fits[[i]]$location <- i
      fits[[i]]$groups <- strsplit(i, "_")[[1]][[group]]
    }
    res <- do.call("rbind", fits)
    return(res)
  }
  is_matrix <- grepl("conf", what)
  the_names <- vapply(locations, paste, character(1), collapse = "_")
  need_col_names <- TRUE
  if (is_matrix) {
    res <- matrix(0.0, nrow = n, ncol = 2L)
  } else {
    res <- numeric(n)
  }
  for (i in seq_len(n)) {
    tmp <- x[[locations[[i]]]]$info[[what]]
    tmp <- if (is.null(tmp)) NA_real_ else tmp
    if (is_matrix) {
      if (need_col_names && !is.na(tmp)) {
        colnames(res) <- colnames(tmp)
        need_col_names <- FALSE
      }
      res[i, ] <- tmp
    } else {
      res[[i]] <- tmp
    }
  }
  if (is_matrix) {
    rownames(res) <- the_names
  } else {
    names(res)    <- the_names
  }
  if (na.rm) {
    nonas <- stats::complete.cases(res)
    res <- if (is_matrix) res[nonas, , drop = FALSE] else res[nonas]
  }
  res
}
