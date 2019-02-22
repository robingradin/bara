#' This function can be used to optimize the number of dimensions to
#' retain when normalizing with BARA.
#'
#' This function is used to estimate the optimal number of dimensions
#' to retain when normalizing with BARA. The optimization is performed using
#' an external test set, ideally affected by batch effects.
#'
#' @param x_train Matrix, training data set. Samples in rows.
#' @param ref_train Numeric vector, index of reference samples in the
#'     training set.
#' @param y_train Response variables corresponding to the training set.
#' @param x_test Matrix, test data set. Samples in rows.
#' @param ref_test Numeric vector, index of reference samples in the
#'     test set.
#' @param y_test Response variable corresponding to the test set.
#' @param fit_fun Function, fits a prediction object from the training
#'     data. The function must take the following arguments:
#'     \itemize{
#'     \item \code{x}:  A matrix corresponding to the compressed training data.
#'     \item \code{y}:  The response variable corresponding to y_train.
#'     \item \code{...}:  See details for additional arguments
#'       passed to the function.
#'     }
#' @param pred_fun Function, classifies the samples in the test set and
#'     returns the predictions. The function must have the following
#'     parameters:
#'     \itemize{
#'       \item \code{object}:  The object returned by \code{fit_fun}.
#'       \item \code{x}:  A matrix corresponding to the compressed test set.
#'       \item \code{...}:  See details for additional arguments.
#'     }
#' @param perf_fun Function, estimates the performance of the generated
#'     predictions. The performance should be returned as a numeric value.
#'     This parameter can also be left to the default value of NULL if no
#'     performance scores should be calculated. If a function is passed.
#'     it must have the the following parameters:
#'     \itemize{
#'       \item \code{y}: The response variable corresponding to the
#'         \code{y_test} argument.
#'       \item \code{pred}: The predictions generated with \code{pred_fun}.
#'       \item \code{...}:  See details for additional arguments.
#'
#'     }
#' @param batch_test Character/numeric/factor, describing the batch
#'     belongings of the test set.
#' @param perf_objective Character, either \code{"maximize"} or
#'     \code{"minimize"}. Should the performance metric be maximized
#'     or minimized?
#' @param max_dim Numeric, determines the maximum number of dimensions
#'     to retain. This parameter can be decreased to reduce computational
#'     time. Default value is \code{100}.
#' @param frac_rm Numeric between 0 and 1. How large fraction of the dimensions
#'     should be dropped in each iteration. For example, if
#'      \code{frac_rm = 0.05}, the number of dimensions retained decreases with
#'      5 percent in each iteration.
#' @param fit_args List with arguments passed to \code{fit_fun}.
#' @param pred_args List with arguments passed to \code{pred_fun}.
#' @param perf_args List with arguments passed to \code{perf_fun}.
#' @param scale_var Logical, should the variances be standardized.
#'     The default value is \code{FALSE}
#' @param verbose Logical, should runtime messages be displayed? Default
#'     value is \code{TRUE}
#' @param cores Integer, number of cores to use for computations.
#' @param .export Variables that any of \code{fit_fun}, \code{pred_fun}
#'     or \code{fit_fun} depends on. Any input is passed on to foreach.
#' @param .packages Packages that any of \code{fit_fun}, \code{pred_fun}
#'     or \code{fit_fun} depends on. Any input is passed on to foreach.
#' @param seed Integer, seed for reproducible computations.
#' @return An object of class BaraOpt containing:
#'     \itemize{
#'       \item \code{fit}: A BaraFit object with the optimal dimensions retained.
#'         \code{best_dim} Numeric, the number of dimensions retained for the
#'             optimal prediction performance.
#'       \item \code{iterations}: List, results from the iterative process,
#'           including predictions and prediciton models for the number of
#'           dimensions examined.
#'
#'     }
#' @export
#' @importFrom doRNG %dorng%
#' @importFrom foreach %dopar%
bara_optimize_ext <- function(x_train, ref_train, y_train,
                              x_test, ref_test, y_test, fit_fun,
                              pred_fun, perf_fun = NULL, batch_test = NULL,
                              perf_objective = c('maximize', 'minimize'),
                              max_dim = 100, frac_rm = 0.05, fit_args = NULL,
                              pred_args = NULL, perf_args = NULL,
                              scale_var = FALSE, verbose = TRUE,
                              cores = 1, .export = NULL, .packages = NULL,
                              seed = 16438){
  # Check the inputs
  x_train <- check_matrix(x = x_train, verbose = verbose)
  x_test <- check_matrix(x = x_test, verbose = verbose)
  ref_train <- check_ref(ref = ref_train, n_samples = nrow(x_train),
                         verbose = verbose)
  ref_test <- check_ref(ref = ref_test, n_samples = nrow(x_test),
                        verbose = verbose)
  batch_test <- check_batch(batch = batch_test, ref = ref_test,
                            n_samples = nrow(x_test))
  perf_objective <- match.arg(arg = perf_objective,
                              choices = c('maximize', 'minimize'))
  if (max_dim > min(dim(x_train))) {
    warning(
      'max_dim must be less than min(dim(x_train)). Its value has been',
      ' decreased to : ',
      min(dim(x_train))
    )
    max_dim <- min(dim(x_train))
  }
  # Get the dimension sizes for the iterations
  dims_to_retain <- get_dim_sizes(frac_rm = frac_rm, n_var = max_dim)
  # Create the BaraFit object
  fit <- bara_fit(x = x_train, ref = ref_train, ndim = max_dim,
                  scale_var = scale_var, verbose = verbose)
  # Set up the potentially parallel backend
  if (cores > 1) {
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl = cl, cores = cores)
  } else {
    foreach::registerDoSEQ()
  }
  on.exit({
    try(
      expr = {
        parallel::stopCluster(cl = cl)
        doParallel::stopImplicitCluster()
      }, silent = TRUE
    )
  })
  # Start the iteration
  i <- numeric()
  results <- foreach::foreach(i = seq_along(dims_to_retain),
                              .packages = c('bara', .packages),
                              .export = .export) %dorng% {
    # Update the fit object to retain the examined number of dimensions.
    tentative_fit <- truncate_bara_fit(bara_fit = fit, x = x_train, dim_to_keep = dims_to_retain[i])
    # Adjust the test set
    x_test_it <- bara_adjust(
      bara_fit = tentative_fit, x = x_test,
      ref = ref_test,batch = batch_test, verbose = verbose
    )
    # Model
    mdl <- do.call(
      what = fit_fun,
      args = c(
        list(
          x = tentative_fit$x,
          y = y_train
        ),
        fit_args
      )
    )
    # Classify
    pred <- do.call(
      what = pred_fun,
      args = c(
        list(
          object = mdl,
          x = x_test_it
        ),
        pred_args
      )
    )
    # Evaluate
    if (!is.null(perf_fun)) {
      perf <- do.call(
        what = perf_fun,
        args = c(
          list(
            y = y_test,
            pred = pred
          ),
          perf_args
        )
      )
    } else {
      perf <- NA_real_
    }
    list(
      mdl = mdl,
      pred = pred,
      perf = perf,
      dim = dims_to_retain[i]
    )
  }
  # Find optimal value
  performances <- purrr::map_dbl(results, ~.$perf)
  dims <- purrr::map_dbl(results, ~.$dim)
  best_dim <- get_best_dimension(perf = performances, dims = dims,
                                 objective = perf_objective)
  if (!is.na(best_dim)) {
    fit <- truncate_bara_fit(bara_fit = fit, x = x_train, dim_to_keep = best_dim)
  }
  output <- list(
    fit = fit,
    best_dim = best_dim,
    iterations = results
  )
  class(output) <- 'BaraOpt'
  return(output)
}

#' Calculates the number of dimensions to retain given a
#' fraction decrease at each iteration
#'
#' @param frac_rm Numeric between 0 and 1. Determines how many
#'     dimensions to dropped in each iteration.
#' @param n_var Numeric, number of variables in the dataset.
#' @return Numeric vector describing the number of dimensions to
#'     retain in each iteration.
get_dim_sizes <- function (frac_rm, n_var){
  if (frac_rm >= 1 | frac_rm < 0) {
    stop(sprintf("Fraction must be between 0 and 1. (0 <= frac_rm < 1)"))
  } else if (frac_rm == 0) {
    dims_retained <- seq(from = n_var, to = 1, by = -1)
  } else {
    dims_retained = n_var
    while (dims_retained[1] > 1) {
      n_rm = ifelse(frac_rm != 0, dims_retained[1] * frac_rm, 1)
      if (n_rm < 1) {
        n_rm = 1
      }
      dims_retained = c(ceiling(dims_retained[1] - n_rm), dims_retained)
    }
    dims_retained <- rev(dims_retained)
  }
  return(dims_retained)
}


#' Determines which dimensions optimizes the performance score.
#'
#' @param perf Numeric vector, represents the performance scores.
#' @param dims Numeric vector, describes the number of dimensions retained.
#' @param objective Character, one of \code{maximize} or \code{minimize}.
#'     Determines if the performance score should be maximized or minimized.
#' @return The lowest number of dimensions retained to achive the optimal
#'     performance score.
get_best_dimension <- function(perf, dims, objective){
  if (!is.numeric(perf)) {
    warning(
      'No optimal compression could be identified. The performance',
      ' scores were not recognized as numeric values.'
    )
    return(NA_real_)
  } else if (all(is.na(perf))) {
    warning(
      'No performance scores were obtained. ',
      'Only prediction results are returned.'
    )
  } else if (any(is.na(perf))) {
    warning(
      'Some missing values were found in the performance scores.',
      ' Dimensions with missing values are excluded from the optimization.'
    )
    idx_rm <- is.na(perf)
    perf <- perf[!idx_rm]
    dims <- dims[!idx_rm]
  }
  if (objective == 'maximize') {
    best_performance <- max(perf)
  } else if (objective == 'minimize') {
    best_performance <- min(perf)
  } else{
    stop('Unrecognized optimization criteria.')
  }
  best_dim <- dims[which(perf == best_performance)[1]]
  return(best_dim)
}
