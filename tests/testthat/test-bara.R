test_that("batches without reference samples can be normalized", {
  x <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), ncol = 3)
  
  colnames(x) <- c('a', 'b', 'c')
  ref = 1
  fit <- bara_fit(x = x, ref = ref, ndim = 3, scale_var = FALSE)
  
  b1 <- NULL
  b2 <- c('1', '1', '2')
  
  expect_equal(
    object = bara_adjust(fit, x, ref, b1),
    expected = x
  )
  expect_error(
    bara_adjust(fit, x, ref, b2)
  )
  x_expected <- x
  x_expected[3, ] <- NA_real_
  expect_equal(
    object = bara_adjust(fit, x, ref, b2, force = TRUE),
    expected = x_expected
  )
})
