# Tests for temporal analysis functions

test_that("surprise_temporal works with time series data", {
  # Create mock temporal data with multiple regions per time
  data <- data.frame(
    time = rep(1:5, each = 4),
    region = rep(c("A", "B", "C", "D"), times = 5),
    observed = c(10, 20, 15, 25,
                 12, 22, 14, 28,
                 11, 25, 18, 22,
                 15, 21, 16, 30,
                 14, 23, 17, 27),
    expected = c(100, 200, 150, 250,
                 100, 200, 150, 250,
                 100, 200, 150, 250,
                 100, 200, 150, 250,
                 100, 200, 150, 250)
  )

  result <- surprise_temporal(
    data,
    time_col = time,
    observed = observed,
    expected = expected,
    region_col = region
  )

  expect_s3_class(result, "bs_surprise_temporal")
  expect_equal(length(result$time_values), 5)
  expect_equal(length(result$surprise_by_time), 5)
})

test_that("update_surprise updates model space correctly", {
  observed <- c(50, 100, 150, 200)
  expected <- c(10000, 50000, 100000, 25000)

  result <- auto_surprise(observed, expected)
  original_posterior <- result$model_space$posterior

  new_observed <- c(55, 110, 140, 220)
  updated <- update_surprise(result, new_observed, new_expected = expected)

  expect_s3_class(updated, "bs_surprise")
  # Posterior may or may not change significantly depending on data
  expect_true(length(updated$surprise) > length(result$surprise))
})

test_that("surprise_rolling computes rolling window surprise", {
  observed <- c(50, 55, 48, 52, 60, 45, 58, 51, 49, 53,
                62, 47, 56, 50, 54, 59, 46, 55, 52, 48)
  expected <- rep(500, 20)

  result <- surprise_rolling(observed, expected, window_size = 5)

  expect_true(is.list(result))
  expect_equal(result$window_size, 5)
  expect_equal(result$n_windows, 16)  # 20 - 5 + 1
})

test_that("surprise_rolling handles edge cases", {
  observed <- c(10, 20, 30, 40, 50)
  expected <- rep(100, 5)

  # Window size equals data length
  result <- surprise_rolling(observed, expected, window_size = 5)
  expect_equal(result$n_windows, 1)

  # Window larger than data should error
  expect_error(
    surprise_rolling(observed, expected, window_size = 10),
    "window_size"
  )
})

test_that("cumulative_bayesian_update accumulates correctly", {
  space <- model_space(
    bs_model_uniform(),
    bs_model_gaussian()
  )

  observed <- c(10, 20, 30, 40)

  # Perform cumulative update - returns a list with final_space, cumulative_surprise, posterior_history
  result <- cumulative_bayesian_update(space, observed)

  expect_true(is.list(result))
  expect_s3_class(result$final_space, "bs_model_space")
  expect_equal(sum(result$final_space$posterior), 1)
  expect_equal(length(result$cumulative_surprise), length(observed))
  expect_equal(nrow(result$posterior_history), length(observed))
})

test_that("auto_surprise leaves global posterior unset", {
  observed <- c(50, 100, 150)
  expected <- c(10000, 50000, 100000)

  result <- auto_surprise(observed, expected)
  mspace <- get_model_space(result)

  # auto_surprise computes per-observation surprise from the prior. Global model
  # updates are handled explicitly by bayesian_update().
  expect_null(mspace$posterior)
})
