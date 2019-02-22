context('Input control and helper functions')

test_that('correct method is found for SVD calculation.', {
  expect_equal(
    check_svd_params(
      x = matrix(data = rnorm(10), nrow = 2),
      ndim = 1,
      loss = 0.1
    ),
    expected = 'ndim'
  )
  expect_error(
    check_svd_params(
      x = matrix(data = rnorm(10), nrow = 2),
      ndim = 10,
      loss = 0.1
    )
  )
  expect_error(
    check_svd_params(
      x = matrix(data = rnorm(10), nrow = 2),
      ndim = 0,
      loss = 0.1
    )
  )
  expect_equal(
    check_svd_params(
      x = matrix(data = rnorm(10), nrow = 2),
      ndim = NULL,
      loss = 0.1
    ),
    expected = 'loss'
  )
  expect_error(
    check_svd_params(
      x = matrix(data = rnorm(10), nrow = 2),
      ndim = NULL,
      loss = 1
    )
  )
  expect_equal(
    check_svd_params(
      x = matrix(data = rnorm(10), nrow = 2),
      ndim = NULL,
      loss = NULL
    ),
    expected = 'none'
  )
})

test_that('the logical vector representing reference samples is ok.', {
  expect_equal(
    generate_ref_lgl(ref = c(1, 2, 3), 5),
    expected = c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  expect_equal(
    generate_ref_lgl(ref = c(1, 2, 3, 4, 5), 5),
    expected = rep(TRUE, 5)
  )
})

test_that('the batch_check is ok.', {
  expect_equal(
    check_batch(batch = c(1, 1, 2, 2), ref = c(1, 3), n_samples = 4),
    expected = c('1', '1', '2', '2')
  )
  expect_error(
    check_batch(batch = c(1, 1, 2, 2), ref = 1, n_samples = 4)
  )
  expect_error(
    check_batch(batch = c(1, 1, 2, 2), ref = c(1, 2), n_samples = 5)
  )
})

test_that('the ref_check is ok.', {
  expect_equal(
    check_ref(ref = 1, n_samples = 10),
    expected = 1
  )
  expect_equal(
    check_ref(ref = c(1, 11), n_samples = 10),
    expected = 1,
  )
  expect_warning(
    check_ref(ref = c(1, 11), n_samples = 10)
  )
  expect_error(
    check_ref(ref = 'sample', n_samples = 10)
  )
})

test_that('the matrix_check is ok.', {
  expect_equal(
    check_matrix(x = matrix(1, dimnames = list('A', 'B'))),
    expected = matrix(1, dimnames = list('A', 'B'))
  )
  expect_message(
    check_matrix(1)
  )
  expect_equal(
    check_matrix(1),
    expected = matrix(1, dimnames = list(NULL, 'var1'))
  )
  expect_equal(
    check_matrix('1'),
    expected = matrix(1, dimnames = list(NULL, 'var1'))
  )
})
