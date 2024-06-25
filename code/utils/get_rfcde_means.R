# PURPOSE: Define helper functions for generating RFCDE predictions for model
#          evaluation via cross-validation

# Get expectation across multiple dimensions ------------------------------

# This code is heavily based on the RFCDE predict function but fixes the error
# in handling computation of expectations across each dimension

#' Return the expectation for each dimension in the provided grid
#'
#' @param rfcde_object
#' @param newdata Dataset to get expectation values for
#' @return Matrix of expected values for each row in newdata with a column for
#' each dimension

get_rfcde_means <- function(rfcde_object, newdata) {


  if (is.vector(newdata)) {
    if (length(newdata) == rfcde_object$n_x) {
      newdata <- matrix(newdata, nrow = 1)
    } else if (rfcde_object$n_x == 1) {
      newdata <- matrix(newdata, ncol = 1)
    } else {
      stop("Prediction must have same number of covariates as training.")
    }
  }

  stopifnot(is.matrix(newdata))
  stopifnot(ncol(newdata) == rfcde_object$n_x)

  n_test <- nrow(newdata)
  n_train <- nrow(rfcde_object$z_train)
  n_dim <- ncol(rfcde_object$z_train)

  # Init the matrix to hold values:
  matrix_means <- matrix(rep(NA, n_test * n_dim), nrow = n_test, ncol = n_dim)

  # Proceed through each dimension separately:
  for (dim_i in seq_len(n_dim)) {

    # Set-up vector of weights to use:
    wts <- rep(0L, n_train)

    # Now proceed through the test data to compute each rows expected value:
    for (new_i in seq_len(n_test)) {
      wts <- RFCDE:::weights.RFCDE(rfcde_object,
                                   newdata[new_i, , drop = FALSE])
      wts <- wts * n_train / sum(wts)
      matrix_means[new_i, dim_i] <-
        stats::weighted.mean(rfcde_object$z_train[, dim_i], t(wts))
    }

  }

  return(matrix_means)

}





