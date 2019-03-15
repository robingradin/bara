#' Creates a BARA fit object
#'
#' The function is applied to the training set and returns a BaraFit
#' object that can be used to adjust batch effects in subsequently acquired
#' test sets.
#'
#' @param x Matrix, the training data set with samples in rows.
#' @param ref Numeric vector, index of the reference samples.
#' @param ndim Integer, describes the number of SVD dimensions to retain.
#'     If NULL, the all possible dimensions are calculated.
#' @param loss Numeric between 0 and 1. Describes the maximum amount of
#'     variance that can be lost during compression.
#' @param scale_var Logical, should the variance be scaled? Default is FALSE.
#' @param verbose Logical, should runtime messages be printed? Default is TRUE.
#' @return An object of class BaraFit.
#' \describe{
#'   \item{\env{x}}{Compressed and reconstructed training data set.
#'     Can be used to define subsequent prediction model.}
#'   \item{\env{ref}}{Index of reference samples.}
#'   \item{\env{means}}{Mean values of the columns of the input matrix x.}
#'   \item{\env{sds}}{Standard deviations of the columns of the input matrix x.}
#'   \item{\env{right_vectors}}{Right singular vectors of the input matrix x.}
#'   \item{\env{ref_means}}{Mean values of the reference samples in the
#'       compressed data space.}
#'   \item{\env{call}}{Function call.}
#' }
#' @export
bara_fit <- function(x, ref, ndim = NULL, loss = NULL,
                     scale_var = FALSE, verbose = TRUE){
  user_call = sys.call()
  # Check input
  x <- check_matrix(x = x, verbose = verbose)
  ref <- check_ref(ref = ref, n_samples = nrow(x), verbose = verbose)
  var_names <- colnames(x)
  # Center and (maybe) standardize data
  c_and_s <- center_and_scale(x = x, scale_var = scale_var)
  x <- c_and_s$x
  # Calculate the singular value decomposition
  right_vectors <- get_right_vectors(x = x, ndim = ndim, loss = loss,
                           verbose = verbose)
  # Compress, estimate reference means and reconstruct the training set
  pcs <- compress_data(right_vectors = right_vectors, x = x)
  ref_means <- colMeans(x = pcs[ref, , drop = FALSE])
  x_reconstructed <- reconstruct_data(right_vectors = right_vectors, pcs = pcs)
  colnames(x_reconstructed) <- var_names
  x_reconstructed <- add_mean_and_sd(x = x_reconstructed,
                                     means = c_and_s$means,
                                     sds = c_and_s$sds)
  # Create the BaraFit object and return it
  bara_fit_obj <- list(
    x = x_reconstructed,
    ref = ref,
    means = c_and_s$means,
    sds = c_and_s$sds,
    right_vectors = right_vectors,
    ref_means = ref_means,
    call = user_call
  )
  class(bara_fit_obj) <- 'BaraFit'
  return(bara_fit_obj)
}

#' Adjusts a test set using the set of reference samples
#'
#' @param bara_fit Object of class BaraFit, created using the training set.
#' @param x Matrix, the test data set with samples in rows.
#' @param ref Numeric vector, index of the reference samples in the test set.
#' @param batch Character/Numeric/Factor. The vector should represent
#'     the batch belongings for the samples in the test set x. If a vector is
#'     supplied, each batch is independently adjusted. Therefore, each batch
#'     must contain at least 1 reference sample.
#' @param verbose Logical, should runtime messages be printed? Default is TRUE.
#' @return A matrix corresponding to the normalized test set.
#' @export
bara_adjust <- function(bara_fit, x, ref, batch = NULL, verbose = TRUE){
  # Check input
  if (!methods::is(bara_fit, 'BaraFit')) {
    stop(
      'bara_fit must be of class BaraFit, not ',
      paste0(class(bara_fit), collapse = ', ')
    )
  }
  x <- check_matrix(x = x, verbose = verbose)
  ref <- check_ref(ref = ref, n_samples = nrow(x), verbose = verbose)
  batch <- check_batch(batch = batch, ref = ref, n_samples = nrow(x))
  if (any(colnames(x) != colnames(bara_fit$x))) {
    stop(
      'The colnames of x must correspond to the colnames of the training set',
      ' used to define the BaraFit object.'
    )
  }
  # "Center" and scale the data
  x <- apply_center_and_scale(bara_fit = bara_fit, x = x)
  # Adjust the test set
  x_adjusted <- adjust_test_set(bara_fit = bara_fit, x = x,
                                ref = ref, batch = batch)
  # Add the means and the standard deviations
  x_adjusted <- add_mean_and_sd(x = x_adjusted, means = bara_fit$means,
                                sds = bara_fit$sds)
  colnames(x_adjusted) <- colnames(bara_fit$x)
  return(x_adjusted)
}

#' Makes sure that x is a numeric matrix.
#'
#' @param x Input data, should be a matrix.
#' @param verbose Logical, should messages be printed? Default is TRUE.
check_matrix <- function(x, verbose = TRUE){
  if (!is.matrix(x)) {
    if (verbose) {
      message(
        paste(
          'The input x was not recognized as a matrix.',
          ' It will be converted to a matrix.'
        )
      )
    }
    x <- as.matrix(x)
  }
  if (any(dim(x) == 0)) {
    stop(
      'The input matrix x must have dimensions > 0.'
    )
  }
  if (typeof(x) != 'double') {
    if (verbose) {
      message(
        paste(
          'The input x was not recognized as a numeric type.',
          ' It will be converted to a numeric matrix.'
        )
      )
    }
    storage.mode(x) <- 'numeric'
  }
  if (is.null(colnames(x))) {
    if (verbose) {
      message(
        'No colnames were found for x, sequential names will be given'
      )
    }
    colnames(x) <- paste('var', seq(from = 1, to = ncol(x), by = 1), sep = '')
  }
  return(x)
}

#' Validates the format of the reference samples
#'
#' @param ref Numeric, reference samples.
#' @param n_samples Numeric, number of samples in the data matrix.
#' @param verbose Logical, should runtime messages be printed? Default is TRUE.
check_ref <- function(ref, n_samples, verbose = TRUE){
  # Check the type
  if (!is.numeric(ref)) {
    stop(
      paste(
        'ref argument must be numeric. Not ',
        paste0(class(ref), collapse = ', '),
        sep = ''
      )
    )
  }
  # Make sure that it is within the dimensions of x
  within_limits <- ref >= 1 & ref <= n_samples
  if (any(!within_limits)) {
    warning(
      paste(
        'Some of the ref values does not match the dimensions of x. ',
        'ref will be truncated to only contain samples between 1 and ',
        n_samples,
        sep = ''
      )
    )
    ref <- ref[within_limits]
  }
  return(ref)
}

#' Verifies the input format of the batch arguments.
#'
#' @param batch Character/Numeric/Factor. The vector should represent
#'     the batch belongings for the samples in the test set x. If a vector is
#'     supplied, each batch is independently adjusted. Therefore, each batch
#'     must contain at least 1 reference sample.
#' @param ref Numeric vector, index of the reference samples in the test set.
#' @param n_samples Numeric, number of samples in x.
#' @return Character vector.
check_batch <- function(batch, ref, n_samples){
  if (!is.null(batch)) {
    batch <- as.character(batch)
    if (length(batch) != n_samples) {  # Correct length
      stop(
        'The batch vector must be of equal length compared to the number',
        ' of samples in x.'
      )
    }
    unique_batches <- unique(batch)
    unique_ref_batches <- unique(batch[ref])
    if (!all(unique_batches %in% unique_ref_batches)) {  # Refs in all batches
      stop(
        'Reference samples could not be found in every batch. The following',
        ' batches were identified without reference samples: ',
        paste0(setdiff(unique_batches, unique_ref_batches), collapse = ', ')
      )
    }
  } else{
    batch <- rep('one_batch', n_samples)
  }
  return(batch)
}

#' Centers and scales the matrix and returns mean and sd vectors.
#'
#' @param x Matrix, the training data set with samples in rows.
#' @param scale_var Logical, should the variance be scaled? Default is FALSE.
center_and_scale <- function(x, scale_var = FALSE){
  x <- scale(x = x, center = TRUE, scale = scale_var)
  means <- attr(x = x, which = 'scaled:center')
  if (scale_var) {
    sds <- attr(x = x, which = 'scaled:scale')
  } else{
    sds <- NULL
  }
  list(
    x = x,
    means = means,
    sds = sds
  )
}

#' Adds the mean values and multiplies the data by the standard deviations
#' previously estimated from the training set.
#'
#' @param x Matrix, dataset to be "unscaled".
#' @param means Numeric vector, mean values to be added.
#' @param sds Numeric vector, standard deviations to be rescaled.
add_mean_and_sd <- function(x, means, sds){
  if (!is.null(sds)) {
    x <- sweep(
      x = x, MARGIN = 2, STATS = sds, FUN = '*'
    )
  }
  x <- sweep(
    x = x, MARGIN = 2, STATS = means, FUN = '+'
  )
  return(x)
}

#' Centers and scales the test data using the values estimated
#' from the training set.
#'
#' @param bara_fit Object of class BaraFit, created using the training set.
#' @param x Matrix, data to be scaled.
#' @return Matrix, the input data  scaled using the values in bara_fit.
apply_center_and_scale <- function(bara_fit, x){
  if (is.null(bara_fit$sds)) {
    x <- scale(x, center = bara_fit$means, scale = FALSE)
  } else{
    x <- scale(x, center = bara_fit$means, scale = bara_fit$sds)
  }
  return(x)
}

#' Calculate the SVD and returns the right singular vectors required
#' for the compression.
#'
#' @param x Matrix, samples in rows.
#' @param ndim Integer, describes the number of SVD dimensions to retain.
#'     If NULL, the number of dimensions is based on loss. If both ndim
#'     and loss is NULL, all dimensions are returned.
#' @param loss Numeric between 0 and 1. Describes the maximum amount of
#'     variance that can be lost during compression.
#' @param verbose Logical, should runtime messages be printed? Default is TRUE.
get_right_vectors <- function(x, ndim, loss, verbose = TRUE){
  svd_param <- check_svd_params(x = x, ndim = ndim, loss = loss)
  if (svd_param == 'loss' || svd_param == 'none'){
    usv <- svd(x = x)
    right_vectors <- usv$v
    if (svd_param == 'loss') {
      explained <- cumsum(usv$d^2 / sum(usv$d^2))
      ndim <- which(explained >= (1 - loss))[1]
      right_vectors <- right_vectors[, seq(from = 1, to = ndim, by = 1)]
    }
  } else{
    usv <- svd(x = x, nv = ndim)
    right_vectors <- usv$v
  }
  attr(right_vectors, 'explained_variance') <- cumsum(usv$d^2 / sum(usv$d^2))
  return(right_vectors)
}


#' Get SVD dimension identification type
#'
#' @param x Matrix, samples in rows.
#' @param ndim Integer, describes the number of SVD dimensions to retain.
#'     If NULL, the number of dimensions is based on loss. If both ndim
#'     and loss is NULL, all dimensions are returned.
#' @param loss Numeric between 0 and 1. Describes the maximum amount of
#'     variance that can be lost during compression.
check_svd_params <- function(x, ndim, loss){
  param_to_use <- ''
  dim_limit <- min(dim(x))
  if (!is.null(ndim)) {
    if (ndim < 1) {
      stop(sprintf('ndim must be greater than 1. ndim = %i', as.integer(ndim)))
    } else if (ndim > dim_limit) {
      stop(
        sprintf(
          'ndim must be below or equal to min(dim(x)).'
        )
      )
    } else{
      param_to_use <- 'ndim'
    }
  } else{
    if (!is.null(loss)) {
      if (loss < 0 || loss >= 1) {
        stop('loss must be between 0 and 1 (0 <= loss < 1).')
      } else{
        param_to_use <- 'loss'
      }
    } else{
      param_to_use <- 'none'
    }
  }
  param_to_use
}

#' Compress a dataset
#'
#' @param right_vectors Matrix, the right singular vectors of the singular
#'     value decomposition performed on the training set.
#' @param x Matrix, a dataset to be projected onto singular vectors.
#' @return Matrix of the projected dataset.
compress_data <- function(right_vectors, x){
  x %*% right_vectors
}

#' Reconstructs the data to its original dimension
#'
#' @param right_vectors Matrix, the right singular vectors of the singular
#'     value decomposition performed on the training set.
#' @param pcs Matrix, a compressed dataset to be reconstructed.
#' @return Matrix of the reconstructed dataset.
reconstruct_data <- function(right_vectors, pcs) {
  pcs %*% t(right_vectors)
}

#' Adjusts test data to make the mean values of the reference samples equal
#'
#' @param bara_fit Object of class BaraFit, created using the training set.
#' @param x Matrix, data to be compressed and adjusted (the test set), already
#'     centered and scaled.
#' @param ref Numeric, reference samples for the test set.
#' @param batch Characters, representing the batch belongings of the
#'     test set x.
#' @return Matrix, the adjusted test set.
adjust_test_set <- function(bara_fit, x, ref, batch){
  # Compress the test set.
  pcs <- compress_data(right_vectors = bara_fit$right_vectors, x = x)
  # Adjust each batch independently
  unique_batches <- unique(batch)
  ref_lgl <- generate_ref_lgl(ref = ref, n_samples = nrow(x))
  for (i in seq_along(unique_batches)) {
    # Find index of the batch and the batch-specific reference samples
    batch_idx <- batch == unique_batches[i]
    batch_ref_idx <- batch_idx & ref_lgl
    # Estimate batch values and correction factors
    batch_means <- colMeans(x = pcs[batch_ref_idx, , drop = FALSE])
    corrections <- bara_fit$ref_means - batch_means
    # Correct the batch
    pcs[batch_idx, ] <- sweep(
      x = pcs[batch_idx, , drop = FALSE],
      MARGIN = 2,
      STATS = corrections,
      FUN = '+'
    )
  }
  # Reconstruct the data
  x_adjusted <- reconstruct_data(right_vectors = bara_fit$right_vectors,
                                 pcs = pcs)
  return(x_adjusted)
}

#' Generate a logical vector where TRUE represent the index of
#' a reference sample
#'
#' @param ref Numeric vector, index of reference samples.
#' @param n_samples Numeric, number of samples in the data (rows of matrix x).
#' @return Logical vector where TRUE entries represent reference samples.
generate_ref_lgl <- function(ref, n_samples){
  lgl_vector <- rep(FALSE, n_samples)
  lgl_vector[ref] <- TRUE
  return(lgl_vector)
}

#' Centers, possibly scales, compresses, reconstructs and restores
#' scale and mean of the training set
#'
#' @param bara_fit Object of class BaraFit, created using the training set.
#' @param x Matrix, data to be processed without adjustment
#' @return Matrix of compressed and reconstructed input x.
squish_train <- function(bara_fit, x) {
  var_names <- colnames(x)
  # Compress training set
  x <- apply_center_and_scale(bara_fit = bara_fit, x = x)
  pcs <- compress_data(
    right_vectors = bara_fit$right_vectors,
    x = x
  )
  x <- reconstruct_data(
    right_vectors = bara_fit$right_vectors,
    pcs = pcs
  )
  x <- add_mean_and_sd(x = x, means = bara_fit$means,
                                sds = bara_fit$sds)
  colnames(x) <- var_names
  return(x)
}

#' Decreases the number of dimensions retained in a BaraFit object
#'
#' @param bara_fit An object of class BaraFit.
#' @param x Matrix, the training set used to define the original BaraFit
#'     object.
#' @param dim_to_keep Numeric, describes the number of dimensions to retain.
#' @return An object of class BaraFit with a decreased number of dimensions
#'     retained.
truncate_bara_fit <- function(bara_fit, x, dim_to_keep){
  if (!methods::is(bara_fit, 'BaraFit')) {
    stop(
      'truncate_bara_fit() requires an object of class BaraFit as input.'
    )
  }
  if (dim_to_keep < 1){
    stop(
      'dim_to_keep must be a positive integer.'
    )
  }
  n_dim <- ncol(bara_fit$right_vectors)
  if (n_dim < dim_to_keep) {
    warning(
      'The BaraFit object contains less right singluar vectors',
      ' compared to desired compression level.'
    )
    return(bara_fit)
  } else{
    seq_dim <- seq(from = 1, to = dim_to_keep, by = 1)
    bara_fit$right_vectors <- bara_fit$right_vectors[, seq_dim, drop = FALSE]
    bara_fit$ref_means <- bara_fit$ref_means[seq_dim]
    bara_fit$x <- squish_train(bara_fit = bara_fit, x = x)
  }
  return(bara_fit)
}
