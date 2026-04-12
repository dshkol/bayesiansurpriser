# Tests for core surprise computation

test_that("kl_divergence returns 0 for equal distributions", {
  p <- c(0.25, 0.25, 0.25, 0.25)
  expect_equal(kl_divergence(p, p), 0)
})

test_that("kl_divergence returns positive value for different distributions", {
  posterior <- c(0.9, 0.1)
  prior <- c(0.5, 0.5)
  kl <- kl_divergence(posterior, prior)

  expect_true(kl > 0)
})

test_that("kl_divergence handles edge cases", {
  # One probability is 0 in posterior (should be fine)
  posterior <- c(1.0, 0.0)
  prior <- c(0.5, 0.5)
  kl <- kl_divergence(posterior, prior)
  expect_equal(kl, 1)  # 1 bit of information

  # Posterior > 0 where prior = 0 should return Inf with warning
  expect_warning(
    kl <- kl_divergence(c(0.5, 0.5), c(1.0, 0.0)),
    "prior is zero"
  )
  expect_equal(kl, Inf)
})

test_that("log_sum_exp is numerically stable", {
  # Large values that would overflow with naive exp
  x <- c(1000, 1001, 1002)
  result <- log_sum_exp(x)

  # Should be close to 1002 + log(1 + exp(-1) + exp(-2))
  expected <- 1002 + log(1 + exp(-1) + exp(-2))
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("bayesian_update changes posterior", {
  space <- model_space(
    bs_model_uniform(),
    bs_model_gaussian()
  )

  observed <- c(10, 20, 30, 40, 50)
  updated <- bayesian_update(space, observed)

  expect_false(is.null(updated$posterior))
  expect_equal(sum(updated$posterior), 1)
  # Posterior should differ from prior after update
  expect_false(all(updated$posterior == updated$prior))
})

test_that("compute_surprise returns valid result", {
  space <- model_space(
    bs_model_uniform(),
    bs_model_gaussian()
  )

  observed <- c(10, 20, 30, 40, 50)
  result <- compute_surprise(space, observed)

  expect_s3_class(result, "bs_surprise")
  expect_equal(length(result$surprise), length(observed))
  expect_true(all(result$surprise >= 0))  # KL is non-negative
})

test_that("compute_surprise returns signed values when requested", {
  expected <- c(10, 20, 30, 40, 50)
  space <- model_space(
    bs_model_uniform(),
    bs_model_baserate(expected)
  )

  observed <- c(15, 25, 20, 35, 60)  # Some higher, some lower than expected
  result <- compute_surprise(space, observed, expected = expected, return_signed = TRUE)

  expect_false(is.null(result$signed_surprise))
  # Signed surprise should reflect direction of deviation
  # Higher than expected should be non-negative
  expect_true(result$signed_surprise[1] >= 0)  # 15 > 10
  # Lower than expected should be non-positive
  expect_true(result$signed_surprise[3] <= 0)  # 20 < 30
})

test_that("auto_surprise works with minimal inputs", {
  observed <- c(50, 100, 150, 200)
  result <- auto_surprise(observed)

  expect_s3_class(result, "bs_surprise")
  expect_equal(length(result$surprise), 4)
})

test_that("auto_surprise uses baserate and funnel when expected provided", {
  observed <- c(50, 100, 150, 200)
  expected <- c(10000, 50000, 100000, 25000)
  result <- auto_surprise(observed, expected)

  # Should have 3 models: uniform, baserate, funnel
  expect_equal(result$model_space$n_models, 3)
})

test_that("auto_surprise matches explicit model-space computation", {
  observed <- c(50, 100, 150, 200)
  expected <- c(10000, 50000, 100000, 25000)

  expected_result <- compute_surprise(
    model_space(
      bs_model_uniform(),
      bs_model_baserate(expected),
      bs_model_funnel(expected)
    ),
    observed = observed,
    expected = expected
  )

  result <- auto_surprise(observed, expected)

  expect_equal(result$surprise, expected_result$surprise)
  expect_equal(result$signed_surprise, expected_result$signed_surprise)
  expect_null(result$model_space$posterior)
})
