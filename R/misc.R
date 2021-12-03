matrix_from_vector <- function(x, ncol) {
  n_balanced <- ceiling(length(x) / ncol) * ncol
  matrix(c(x, rep(NA, n_balanced - length(x))), ncol = ncol)
}
