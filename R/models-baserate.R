# Base Rate Model
#
# Model comparing observed values to expected base rates.

#' Create a Base Rate Model
#'
#' Creates a model that compares observed events to expected rates based on
#' a known baseline (e.g., population). This addresses "base rate bias" where
#' patterns in visualizations are dominated by underlying factors like population
#' density.
#'
#' @param expected Numeric vector of expected values or proportions.
#'   E.g., population counts, area sizes, or any prior expectation.
#' @param normalize Logical; normalize expected to sum to 1?
#' @param name Optional name for the model
#'
#' @return A `bs_model_baserate` object
#'
#' @details
#' Under the base rate model, expected proportions are defined by the
#' `expected` vector. The likelihood measures how well observed data
#' matches these expected proportions:
#'
#' \deqn{P(D|BaseRate) = 1 - \frac{1}{2} \sum_i |O_i - E_i|}
#'
#' For example, if region A has 10% of the population, we expect 10% of events.
#' Regions with event rates matching their population share show low surprise;
#' regions with disproportionate rates show high surprise.
#'
#' This is the primary tool for de-biasing choropleth maps.
#'
#' @export
#' @examples
#' # Population-weighted base rate
#' population <- c(10000, 50000, 100000, 25000)
#' model <- bs_model_baserate(population)
#'
#' # Use in model space
#' space <- model_space(
#'   bs_model_uniform(),
#'   bs_model_baserate(population)
#' )
bs_model_baserate <- function(expected, normalize = TRUE, name = NULL) {
  if (length(expected) == 0) {
    cli_abort("{.arg expected} cannot be empty.")
  }

  # Store normalized expected proportions
  if (normalize && sum(expected, na.rm = TRUE) > 0) {
    expected_prop <- normalize_prob(expected)
  } else {
    expected_prop <- expected
  }

  likelihood_fn <- function(observed, region_idx = NULL, expected_override = NULL, ...) {
    exp_rates <- expected_override %||% expected_prop

    # Ensure same length
    if (length(exp_rates) != length(observed)) {
      if (length(exp_rates) == 1) {
        exp_rates <- rep(exp_rates, length(observed))
      } else {
        cli_abort("Length mismatch: observed ({length(observed)}) vs expected ({length(exp_rates)})")
      }
    }

    if (!is.null(region_idx)) {
      # Per-region likelihood
      total <- sum(observed, na.rm = TRUE)
      if (total == 0) return(0)

      # Expected count for this region based on its proportion
      expected_count <- total * exp_rates[region_idx]

      obs_i <- observed[region_idx]
      if (is.na(obs_i)) return(-Inf)

      # Log-likelihood using Poisson
      stats::dpois(round(obs_i), lambda = pmax(expected_count, 0.5), log = TRUE)
    } else {
      # Global likelihood
      observed_prop <- normalize_prob(observed)
      exp_rates_norm <- normalize_prob(exp_rates)

      # Total variation distance from expected
      tvd <- 0.5 * sum(abs(observed_prop - exp_rates_norm), na.rm = TRUE)
      log(pmax(1 - tvd, 1e-10))
    }
  }

  new_bs_model(
    type = "baserate",
    params = list(expected = expected, expected_prop = expected_prop, normalize = normalize),
    likelihood_fn = likelihood_fn,
    name = name %||% "Base Rate"
  )
}

#' Create Base Rate Model from Column
#'
#' Convenience function to create a base rate model from a data frame column.
#'
#' @param data Data frame or sf object
#' @param column Name of column containing expected values
#' @param ... Additional arguments passed to [bs_model_baserate()]
#'
#' @return A `bs_model_baserate` object
#'
#' @export
#' @examples
#' df <- data.frame(expected = c(100, 200, 300))
#' model <- bs_model_baserate_col(df, "expected")
#' model$name
bs_model_baserate_col <- function(data, column, ...) {
  if (!column %in% names(data)) {
    cli_abort("Column {.val {column}} not found in data.")
  }

  expected <- data[[column]]
  bs_model_baserate(expected, name = paste("Base Rate:", column), ...)
}
