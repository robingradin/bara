# Tests

context('Optimization functions')

test_that('optimal number of dimensions if found.', {
  expect_equal(
    get_best_dimension(perf = 1:5, dim = 1:5, objective = 'maximize'),
    expected = 5
  )
  expect_equal(
    get_best_dimension(perf = 1:5, dim = 1:5, objective = 'minimize'),
    expected = 1
  )
  expect_equal(
    get_best_dimension(perf = c(1:4, NA), dim = 1:5,
                                objective = 'maximize'),
    expected = 4
  )
  expect_equal(
    get_best_dimension(perf = c(NA, 2:5), dim = 1:5,
                                objective = 'maximize'),
    expected = 5
  )
  expect_equal(
    get_best_dimension(perf = c(1:4, NA), dim = 1:5,
                                objective = 'minimize'),
    expected = 1
  )
  expect_equal(
    get_best_dimension(perf = c(NA, 2:5), dim = 1:5,
                                objective = 'minimize'),
    expected = 2
  )
  expect_equal(
    get_best_dimension(perf = rep(5, 5), dim = 1:5,
                       objective = 'maximize'),
    expected = 1
  )
  expect_equal(
    get_best_dimension(perf = rep(5, 5), dim = 1:5,
                       objective = 'minimize'),
    expected = 1
  )
  expect_equal(
    get_best_dimension(perf = rep(NA_real_, 5), dim = 1:5,
                       objective = 'minimize'),
    expected = NA_real_
  )
})

test_that('correct dimensions to explore is identified.', {
  expect_equal(
    get_dim_sizes(frac_rm = 0, n_var = 10),
    expected = 10:1
  )
  expect_equal(
    get_dim_sizes(frac_rm = 0.5, n_var = 8),
    expected = c(8, 4, 2, 1)
  )
  expect_error(
    get_dim_sizes(frac_rm = 1, n_var = 10)
  )
  expect_error(
    get_dim_sizes(frac_rm = 0, n_var = -10)
  )
})
