## I'm not 100% sure about these; they both leave the total sum a bit
## messed up, but I think that's OK.
rolling_mean <- function(x, n) {
  rolling(x, n, TRUE)
}

rolling_sum <- function(x, n) {
  rolling(x, n, FALSE)
}

rolling <- function(x, n, mean) {
  if (n <= 1L) {
    x
  } else {
    coef <- rep(if (mean) 1 / n else 1, n)
    stats::filter(x, coef, sides=1)[-seq_len(n - 1L)]
  }
}

first <- function(x) {
  x[[1L]]
}
last <- function(x) {
  x[[length(x)]]
}
