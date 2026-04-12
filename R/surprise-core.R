# Core Mathematical Functions for Bayesian Surprise
#
# This file contains the fundamental mathematical operations:
# - KL-divergence calculation
# - Bayesian updating
# - Numerically stable helpers

#' Kullback-Leibler Divergence
#'
#' Computes the KL-divergence from prior to posterior distribution,
#' which measures "surprise" in the Bayesian framework.
#'
#' @param posterior Numeric vector of posterior probabilities
#' @param prior Numeric vector of prior probabilities (same length as posterior)
#' @param base Base of logarithm (default: 2 for bits)
#'
#' @return Numeric scalar: the KL-divergence value (always non-negative)
#'
#' @details
#' KL-divergence is defined as:
#' \deqn{D_{KL}(P || Q) = \sum_i P_i \log(P_i / Q_i)}
#'
#' where P is the posterior and Q is the prior. The divergence is 0 when
#' posterior equals prior (no surprise), and increases as they differ.
#'
#' Zero probabilities are handled by excluding those terms (convention that
#' 0 * log(0) = 0).
#'
#' @export
#' @examples
#' # No surprise when prior equals posterior
#' kl_divergence(c(0.5, 0.5), c(0.5, 0.5))
#'
#' # High surprise when distributions differ
#' kl_divergence(c(0.9, 0.1), c(0.5, 0.5))
#'
#' # Maximum surprise when posterior is certain
#' kl_divergence(c(1.0, 0.0), c(0.5, 0.5))
kl_divergence <- function(posterior, prior, base = 2) {
  # Input validation
  if (length(posterior) != length(prior)) {
    cli_abort("{.arg posterior} and {.arg prior} must have the same length.")
  }

  # Handle zero probabilities
  # Use convention that 0 * log(0/x) = 0 and x * log(x/0) = Inf
  idx <- posterior > 0 & prior > 0

  if (!any(idx)) {
    return(0)
  }

  # Check for impossible updates (posterior > 0 where prior = 0)
  if (any(posterior > 0 & prior == 0)) {
    cli_warn("Posterior has positive mass where prior is zero; returning Inf.")
    return(Inf)
  }

  sum(posterior[idx] * log(posterior[idx] / prior[idx]) / log(base))
}

#' Log-Sum-Exp (Numerically Stable)
#'
#' Computes log(sum(exp(x))) in a numerically stable way.
#'
#' @param x Numeric vector of log values
#'
#' @return Numeric scalar: log(sum(exp(x)))
#'
#' @details
#' Uses the identity: log(sum(exp(x))) = max(x) + log(sum(exp(x - max(x))))
#' This avoids overflow when x contains large positive values.
#'
#' @export
#' @examples
#' # Direct computation would overflow
#' x <- c(1000, 1001, 1002)
#' log_sum_exp(x)  # Returns ~1002.41
log_sum_exp <- function(x) {
  if (length(x) == 0) return(-Inf)
  if (all(is.infinite(x) & x < 0)) return(-Inf)

  max_x <- max(x[is.finite(x)])
  max_x + log(sum(exp(x - max_x)))
}

#' Bayesian Update of Model Space
#'
#' Updates the prior probability distribution over models given observed data,
#' using Bayes' rule.
#'
#' @param model_space A `bs_model_space` object
#' @param observed Numeric vector of observed values
#' @param region_idx Optional integer index for region-specific likelihood
#' @param ... Additional arguments passed to likelihood functions
#'
#' @return Updated `bs_model_space` with posterior probabilities
#'
#' @details
#' Applies Bayes' rule:
#' \deqn{P(M|D) \propto P(D|M) \cdot P(M)}
#'
#' where P(D|M) is the likelihood of data D given model M, and P(M) is the prior.
#'
#' @export
#' @examples
#' # Create a model space
#' space <- model_space(
#'   bs_model_uniform(),
#'   bs_model_gaussian()
#' )
#'
#' # Update with observed data
#' observed <- c(10, 20, 30, 40, 50)
#' updated <- bayesian_update(space, observed)
#' print(updated)
bayesian_update <- function(model_space, observed, region_idx = NULL, ...) {
  if (!inherits(model_space, "bs_model_space")) {
    cli_abort("{.arg model_space} must be a {.cls bs_model_space} object.")
  }

  # Compute log-likelihoods for each model
  log_likelihoods <- vapply(
    model_space$models,
    function(m) {
      ll <- m$compute_likelihood(observed, region_idx = region_idx, ...)
      if (!is.finite(ll)) {
        cli_warn("Non-finite log-likelihood from model {.val {m$name}}.")
      }
      ll
    },
    numeric(1)
  )

  # Log-posterior (unnormalized): log P(M|D) = log P(D|M) + log P(M) + const
  log_prior <- log(model_space$prior)
  log_posterior_unnorm <- log_likelihoods + log_prior

  # Normalize using log-sum-exp for numerical stability
  log_normalizer <- log_sum_exp(log_posterior_unnorm)
  posterior <- exp(log_posterior_unnorm - log_normalizer)

  # Ensure proper normalization (fix any floating point issues)
  posterior <- posterior / sum(posterior)

  # Update model space
  model_space$posterior <- posterior
  names(model_space$posterior) <- names(model_space$prior)

  model_space
}

#' Compute Per-Region Surprise
#'
#' Computes the surprise (KL-divergence) for each observation/region,
#' measuring how much each data point updates beliefs about the model space.
#'
#' @param model_space A `bs_model_space` object
#' @param observed Numeric vector of observed values (one per region)
#' @param expected Numeric vector of expected values (optional, for signed surprise)
#' @param return_signed Logical; compute signed surprise?
#' @param return_posteriors Logical; return per-region posteriors?
#' @param return_contributions Logical; return per-model contributions?
#' @param normalize_posterior Logical; if TRUE (default), normalizes posteriors
#'   to sum to 1 before computing KL divergence. This is the standard Bayesian
#'   Surprise calculation. If FALSE, uses unnormalized posterior weights
#'   (`P(D|M) * P(M)`) for comparison with the original Correll & Heer
#'   JavaScript demo output.
#' @param ... Additional arguments passed to likelihood functions
#'
#' @return A `bs_surprise` object
#'
#' @details
#' For each region i, computes:
#' 1. The posterior P(M|D_i) given just that region's data
#' 2. The KL-divergence from prior to posterior (surprise)
#' 3. Optionally, the sign based on deviation direction
#'
#' `normalize_posterior = FALSE` is a legacy replication mode for the original
#' JavaScript demo's per-region map calculation. It is not a proper KL
#' divergence between probability distributions and should not be used as the
#' default method for new analyses.
#'
#' @export
compute_surprise <- function(model_space,
                              observed,
                              expected = NULL,
                              return_signed = TRUE,
                              return_posteriors = FALSE,
                              return_contributions = FALSE,
                              normalize_posterior = TRUE,
                              ...) {
  if (!inherits(model_space, "bs_model_space")) {
    cli_abort("{.arg model_space} must be a {.cls bs_model_space} object.")
  }

  n <- length(observed)
  n_models <- model_space$n_models

  # Initialize output vectors
  surprise_values <- numeric(n)
  signed_values <- if (return_signed) numeric(n) else NULL
  posteriors <- if (return_posteriors) matrix(0, n, n_models) else NULL
  contributions <- if (return_contributions) matrix(0, n, n_models) else NULL

  if (return_posteriors || return_contributions) {
    colnames(posteriors) <- names(model_space$models)
    if (!is.null(contributions)) colnames(contributions) <- names(model_space$models)
  }


  # Compute expected values for signed surprise
  # The sign indicates whether observation is above or below expectation
  if (return_signed) {
    if (is.null(expected)) {
      # Use mean as default expected value
      expected_for_sign <- rep(mean(observed, na.rm = TRUE), n)
    } else {
      if (normalize_posterior) {
        # Standard approach: expected count = pop * overall_rate (weighted mean)
        overall_rate <- sum(observed, na.rm = TRUE) / sum(expected, na.rm = TRUE)
        expected_for_sign <- expected * overall_rate
      } else {
        # Legacy JS-comparison mode: sign based on rate - mean(rates).
        # SignedSurprise = sign(rate - mean_rate) * Surprise
        rates <- observed / expected
        mean_rate <- mean(rates, na.rm = TRUE)
        # Store deviation in rate space, not count space
        expected_for_sign <- expected * mean_rate
      }
    }
  } else {
    expected_for_sign <- NULL
  }

  prior <- model_space$prior

  # Compute per-region surprise
  for (i in seq_len(n)) {
    if (is.na(observed[i])) {
      surprise_values[i] <- NA
      if (return_signed) signed_values[i] <- NA
      next
    }

    # Compute log-likelihoods for this region
    log_likelihoods <- vapply(
      model_space$models,
      function(m) m$compute_likelihood(observed, region_idx = i, ...),
      numeric(1)
    )

    # Compute posterior for this region
    log_prior <- log(prior)
    log_posterior_unnorm <- log_likelihoods + log_prior

    if (normalize_posterior) {
      # Standard Bayesian: normalize to proper probability distribution
      log_normalizer <- log_sum_exp(log_posterior_unnorm)
      region_posterior <- exp(log_posterior_unnorm - log_normalizer)
      region_posterior <- region_posterior / sum(region_posterior)
    } else {
      # Legacy JS-comparison mode: use unnormalized weights P(D|M) * P(M).
      region_posterior <- exp(log_posterior_unnorm)
    }

    # Compute KL-divergence (surprise)
    # Note: when normalize_posterior=FALSE, this is a legacy score rather than
    # a true KL divergence between probability distributions.
    kl_value <- kl_divergence(region_posterior, prior)

    surprise_values[i] <- if (normalize_posterior) {
      pmax(kl_value, 0)
    } else {
      # The original JS map used |score| because the unnormalized score can be
      # negative.
      abs(kl_value)
    }

    # Store posteriors if requested
    if (return_posteriors) {
      posteriors[i, ] <- region_posterior
    }

    # Compute per-model contributions if requested
    if (return_contributions) {
      # Contribution = posterior_i * log(posterior_i / prior_i)
      idx <- region_posterior > 0 & prior > 0
      contributions[i, idx] <- region_posterior[idx] *
        log(region_posterior[idx] / prior[idx]) / log(2)
    }

    # Compute signed surprise
    if (return_signed) {
      deviation <- observed[i] - expected_for_sign[i]
      signed_values[i] <- sign(deviation) * surprise_values[i]
    }
  }

  # Create result object
  new_bs_surprise(
    surprise = surprise_values,
    signed_surprise = signed_values,
    model_space = model_space,
    posteriors = posteriors,
    model_contributions = contributions,
    data_info = list(
      n = n,
      observed_range = range(observed, na.rm = TRUE),
      expected_range = if (!is.null(expected)) range(expected, na.rm = TRUE) else NULL
    )
  )
}

#' Global Bayesian Update Across All Regions
#'
#' Performs a cumulative Bayesian update, updating the prior after each
#' observation. This is useful for streaming/temporal data.
#'
#' @param model_space A `bs_model_space` object
#' @param observed Numeric vector of observed values (in order)
#' @param ... Additional arguments passed to likelihood functions
#'
#' @return A list containing:
#'   - `final_space`: The model space after all updates
#'   - `cumulative_surprise`: Vector of cumulative surprise after each observation
#'   - `posterior_history`: Matrix of posteriors after each update
#'
#' @export
cumulative_bayesian_update <- function(model_space, observed, ...) {
  n <- length(observed)
  n_models <- model_space$n_models

  cumulative_surprise <- numeric(n)
  posterior_history <- matrix(0, n, n_models)
  colnames(posterior_history) <- names(model_space$models)

  current_space <- model_space

  for (i in seq_len(n)) {
    if (is.na(observed[i])) {
      cumulative_surprise[i] <- NA
      posterior_history[i, ] <- current_space$prior
      next
    }

    # Save prior before update
    prior_before <- current_space$prior

    # Update with this observation
    current_space <- bayesian_update(current_space, observed, region_idx = i, ...)

    # Compute surprise from this update
    cumulative_surprise[i] <- kl_divergence(current_space$posterior, prior_before)

    # Save posterior
    posterior_history[i, ] <- current_space$posterior

    # Set posterior as new prior for next iteration
    current_space$prior <- current_space$posterior
  }

  list(
    final_space = current_space,
    cumulative_surprise = cumulative_surprise,
    posterior_history = posterior_history
  )
}
