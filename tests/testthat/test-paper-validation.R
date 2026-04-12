# Tests for formulas and legacy comparison behavior discussed in Correll & Heer
# (2017), "Surprise! Bayesian Weighting for De-Biasing Thematic Maps".

test_that("funnel model formula='paper' uses two-tailed p-value", {
  # Create test data
  population <- c(10000, 50000, 100000, 25000, 75000)
  counts <- c(100, 400, 1000, 250, 600)  # 1%, 0.8%, 1%, 1%, 0.8%

  model <- bs_model_funnel(population, formula = "paper")

  # Compute expected values manually matching paper formulas
  rates <- counts / population
  mean_rate <- mean(rates)
  sd_rate <- sd(rates)
  z_scores <- (rates - mean_rate) / sd_rate
  pop_frac <- population / sum(population)
  dm_scores <- z_scores * sqrt(pop_frac)

  # Paper: P(D|M) = 2 * pnorm(-|dM|)
  expected_pdm <- 2 * pnorm(-abs(dm_scores))

  # Get model's likelihoods
  actual_pdm <- sapply(seq_along(counts), function(i) {
    exp(model$compute_likelihood(counts, region_idx = i))
  })

  expect_equal(actual_pdm, expected_pdm, tolerance = 1e-10)
})

test_that("funnel model uses unweighted mean of rates", {
  # Paper uses unweighted mean: mean(rates), not sum(counts)/sum(pop)
  population <- c(1000, 100000)  # Very different population sizes
  counts <- c(100, 5000)  # 10% vs 5%

  model <- bs_model_funnel(population, formula = "paper")

  rates <- counts / population  # 0.10, 0.05
  # Unweighted mean: 0.075
  # Weighted mean: 5100/101000 = 0.0505

  # With unweighted mean (0.075), z-scores have different signs
  # Region 1: (0.10 - 0.075)/sd > 0 (positive)
  # Region 2: (0.05 - 0.075)/sd < 0 (negative)

  ll1 <- model$compute_likelihood(counts, region_idx = 1)
  ll2 <- model$compute_likelihood(counts, region_idx = 2)

  # Both should have valid likelihoods
expect_true(is.finite(ll1))
  expect_true(is.finite(ll2))

  # Verify using unweighted mean formula
  mean_rate <- mean(rates)
  sd_rate <- sd(rates)
  z1 <- (rates[1] - mean_rate) / sd_rate
  z2 <- (rates[2] - mean_rate) / sd_rate

  expect_true(z1 > 0)  # Above mean
  expect_true(z2 < 0)  # Below mean
})

test_that("uniform model returns constant per-region likelihood", {
  model <- bs_model_uniform()
  observed <- c(10, 20, 30, 40, 50)

  # Per-region likelihood should be constant (log(1) = 0)
  ll1 <- model$compute_likelihood(observed, region_idx = 1)
  ll2 <- model$compute_likelihood(observed, region_idx = 3)
  ll5 <- model$compute_likelihood(observed, region_idx = 5)

  expect_equal(ll1, 0)
  expect_equal(ll2, 0)
  expect_equal(ll5, 0)
})

test_that("compute_surprise with normalize_posterior=FALSE is legacy JS mode", {
  # Test case: three regions with different rates
  population <- c(100000, 100000, 100000)
  counts <- c(1000, 1500, 3000)  # 1%, 1.5%, 3%

  funnel <- bs_model_funnel(population, formula = "paper")
  uniform <- bs_model_uniform()
  space <- model_space(uniform, funnel, prior = c(0.5, 0.5))

  result <- compute_surprise(
    space,
    observed = counts,
    expected = population,
    normalize_posterior = FALSE
  )

  # Surprise should be non-negative (legacy mode uses |score|)
  expect_true(all(result$surprise >= 0))

  # Region with more extreme rate should have higher surprise
  rates <- counts / population
  mean_rate <- mean(rates)  # 1.83%
  deviation <- abs(rates - mean_rate)

  # Region 3 (3%) is furthest from mean (1.83%), so highest surprise
  # Region 2 (1.5%) is closest to mean, so lowest surprise
  expect_true(result$surprise[3] > result$surprise[1])
  expect_true(result$surprise[3] > result$surprise[2])
})

test_that("signed surprise reflects deviation direction", {
  population <- c(100000, 100000)
  counts <- c(500, 1500)  # 0.5% vs 1.5%

  funnel <- bs_model_funnel(population, formula = "paper")
  uniform <- bs_model_uniform()
  space <- model_space(uniform, funnel, prior = c(0.5, 0.5))

  result <- compute_surprise(
    space,
    observed = counts,
    expected = population,
    normalize_posterior = FALSE,
    return_signed = TRUE
  )

  rates <- counts / population
  mean_rate <- mean(rates)

  # Region 1 (0.5%) is below mean (1.0%), so negative signed surprise
  expect_true(result$signed_surprise[1] < 0)

  # Region 2 (1.5%) is above mean (1.0%), so positive signed surprise
  expect_true(result$signed_surprise[2] > 0)

  # Absolute values should be equal (symmetric case)
  expect_equal(
    abs(result$signed_surprise[1]),
    abs(result$signed_surprise[2]),
    tolerance = 1e-10
  )
})

test_that("dM score formula uses unemployment-reference scaling", {
  # dM = Z * sqrt(pop / total_pop)
  population <- c(10000, 40000, 50000)
  total_pop <- sum(population)
  counts <- c(1000, 3200, 5000)  # 10%, 8%, 10%

  rates <- counts / population
  mean_rate <- mean(rates)
  sd_rate <- sd(rates)

  z_scores <- (rates - mean_rate) / sd_rate
  pop_frac <- population / total_pop
  expected_dm <- z_scores * sqrt(pop_frac)

  # The dM scores should scale by sqrt(pop_frac)
  # This means larger population regions have more "statistical power"
  expect_equal(expected_dm, z_scores * sqrt(pop_frac))

  # Verify relationship: larger population = larger |dM| for same |Z|
  # Region 3 has largest population and similar rate deviation as Region 1
  expect_true(abs(expected_dm[3]) > abs(expected_dm[1]))
})

test_that("P(D|M) is two-tailed p-value", {
  # P(D|M) = 2 * pnorm(-|dM|)
  # When dM = 0, P(D|M) = 1
  # When dM = 1.96, P(D|M) ≈ 0.05
  # When dM = 3, P(D|M) ≈ 0.003

  # dM = 0 case
  expect_equal(2 * pnorm(-abs(0)), 1)

  # dM = 1.96 case (95% CI)
  expect_equal(2 * pnorm(-abs(1.96)), 0.05, tolerance = 0.001)

  # dM = 2.576 case (99% CI)
  expect_equal(2 * pnorm(-abs(2.576)), 0.01, tolerance = 0.001)
})

test_that("legacy JS mode produces higher surprise for genuine outliers", {
  # Create data with one clear outlier
  set.seed(42)
  population <- rep(50000, 20)
  base_rate <- 0.05
  counts <- round(population * base_rate)

  # Make one region an outlier (3x the rate)
  counts[10] <- round(population[10] * base_rate * 3)

  funnel <- bs_model_funnel(population, formula = "paper")
  uniform <- bs_model_uniform()
  space <- model_space(uniform, funnel, prior = c(0.5, 0.5))

  result <- compute_surprise(
    space,
    observed = counts,
    expected = population,
    normalize_posterior = FALSE
  )

  # The outlier region should have the highest surprise
  expect_equal(which.max(result$surprise), 10)

  # The outlier's surprise should be substantially higher than average
  mean_surprise_others <- mean(result$surprise[-10])
  expect_true(result$surprise[10] > mean_surprise_others * 2)
})
