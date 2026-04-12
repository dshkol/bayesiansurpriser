# S3 Class Definitions for bayesiansurpriser
#
# This file defines the core S3 classes used throughout the package:
# - bs_model: Base class for probability models
# - bs_model_space: Container for multiple models with prior/posterior
# - bs_surprise: Result of surprise computation
# - bs_surprise_sf: Surprise result with sf geometry

# Model Classes ----------------------------------------------------------------

#' Create a new model object
#'
#' Internal constructor for model objects. Users should use the specific
#' model constructors like [bs_model_uniform()], [bs_model_baserate()], etc.
#'
#' @param type Character string indicating model type
#' @param params List of model parameters
#' @param likelihood_fn Function to compute log-likelihood
#' @param name Optional human-readable name for the model
#'
#' @return A `bs_model` object
#' @noRd
new_bs_model <- function(type, params, likelihood_fn, name = NULL) {
  stopifnot(
    is.character(type),
    length(type) == 1,
    is.list(params),
    is.function(likelihood_fn)
  )

  structure(
    list(
      type = type,
      name = name %||% type,
      params = params,
      compute_likelihood = likelihood_fn
    ),
    class = c(paste0("bs_model_", type), "bs_model")
  )
}

#' @export
print.bs_model <- function(x, ...) {
  cat("<bs_model:", x$type, ">\n")
  if (!is.null(x$name) && x$name != x$type) {
    cat("  Name:", x$name, "\n")
  }
  if (length(x$params) > 0) {
    cat("  Parameters:\n")
    for (nm in names(x$params)) {
      val <- x$params[[nm]]
      if (is.numeric(val) && length(val) > 5) {
        cat("   ", nm, ": [", length(val), "values]\n")
      } else if (is.numeric(val)) {
        cat("   ", nm, ":", paste(round(val, 4), collapse = ", "), "\n")
      } else {
        cat("   ", nm, ":", as.character(val), "\n")
      }
    }
  }
  invisible(x)
}

# Model Space Classes ----------------------------------------------------------

#' Create a new model space object
#'
#' Internal constructor for model space objects. Users should use [model_space()].
#'
#' @param models List of `bs_model` objects
#' @param prior Numeric vector of prior probabilities (must sum to 1)
#' @param names Optional character vector of model names
#'
#' @return A `bs_model_space` object
#' @noRd
new_bs_model_space <- function(models, prior = NULL, names = NULL) {
  n_models <- length(models)

  # Validate models
  if (!all(vapply(models, inherits, logical(1), "bs_model"))) {
    cli_abort("All elements of {.arg models} must be {.cls bs_model} objects.")
  }

  # Set default uniform prior
  if (is.null(prior)) {
    prior <- rep(1 / n_models, n_models)
  }

  # Validate prior
  if (length(prior) != n_models) {
    cli_abort("{.arg prior} must have length {n_models}, not {length(prior)}.")
  }
  if (abs(sum(prior) - 1) > .Machine$double.eps^0.5) {
    cli_abort("{.arg prior} must sum to 1, not {sum(prior)}.")
  }
 if (any(prior < 0)) {
    cli_abort("{.arg prior} must contain non-negative values.")
  }

  # Set names
  if (is.null(names)) {
    names <- vapply(models, function(m) m$name, character(1))
  }
  names(models) <- names
  names(prior) <- names

  structure(
    list(
      models = models,
      prior = prior,
      posterior = NULL,
      n_models = n_models
    ),
    class = "bs_model_space"
  )
}

#' @export
print.bs_model_space <- function(x, ...) {
  cat("<bs_model_space>\n")
  cat("  Models:", x$n_models, "\n")
  for (i in seq_along(x$models)) {
    nm <- names(x$models)[i]
    prior_p <- round(x$prior[i], 4)
    post_p <- if (!is.null(x$posterior)) round(x$posterior[i], 4) else NA
    if (is.na(post_p)) {
      cat("   ", i, ". ", nm, " (prior: ", prior_p, ")\n", sep = "")
    } else {
      cat("   ", i, ". ", nm, " (prior: ", prior_p, ", posterior: ", post_p, ")\n", sep = "")
    }
  }
  invisible(x)
}

# Surprise Result Classes ------------------------------------------------------

#' Create a new surprise result object
#'
#' Internal constructor for surprise results. Created by [compute_surprise()].
#'
#' @param surprise Numeric vector of surprise values (KL-divergence)
#' @param signed_surprise Numeric vector of signed surprise (optional)
#' @param model_space The `bs_model_space` used for computation
#' @param posteriors Matrix of posterior probabilities (n_obs x n_models)
#' @param model_contributions Matrix of per-model surprise contributions
#' @param data_info List with metadata about input data
#'
#' @return A `bs_surprise` object
#' @noRd
new_bs_surprise <- function(surprise,
                            signed_surprise = NULL,
                            model_space,
                            posteriors = NULL,
                            model_contributions = NULL,
                            data_info = list()) {
  stopifnot(
    is.numeric(surprise),
    inherits(model_space, "bs_model_space")
  )

  structure(
    list(
      surprise = surprise,
      signed_surprise = signed_surprise,
      model_space = model_space,
      posteriors = posteriors,
      model_contributions = model_contributions,
      data_info = data_info
    ),
    class = "bs_surprise"
  )
}

#' @export
print.bs_surprise <- function(x, ...) {
  cat("<bs_surprise>\n")
  cat("  Observations:", length(x$surprise), "\n")
  cat("  Surprise range:", round(min(x$surprise, na.rm = TRUE), 4), "to",
      round(max(x$surprise, na.rm = TRUE), 4), "\n")
  if (!is.null(x$signed_surprise)) {
    cat("  Signed surprise range:", round(min(x$signed_surprise, na.rm = TRUE), 4), "to",
        round(max(x$signed_surprise, na.rm = TRUE), 4), "\n")
  }
  cat("  Models:", x$model_space$n_models, "\n")
  invisible(x)
}

#' @export
summary.bs_surprise <- function(object, ...) {
  cat("Bayesian Surprise Summary\n")
  cat("=========================\n\n")

  cat("Surprise Statistics:\n")
  print(summary(object$surprise))
  cat("\n")

  if (!is.null(object$signed_surprise)) {
    cat("Signed Surprise Statistics:\n")
    print(summary(object$signed_surprise))
    cat("\n")
  }

  cat("Model Space:\n")
  print(object$model_space)

  invisible(object)
}

# Surprise SF Classes ----------------------------------------------------------

#' Create a new surprise_sf result object
#'
#' Internal constructor for sf objects with surprise results attached.
#'
#' @param sf_data An sf object with surprise columns added
#' @param surprise_result A `bs_surprise` object
#'
#' @return A `bs_surprise_sf` object (inherits from sf)
#' @noRd
new_bs_surprise_sf <- function(sf_data, surprise_result) {
  stopifnot(
    inherits(sf_data, "sf"),
    inherits(surprise_result, "bs_surprise")
  )

  structure(
    sf_data,
    surprise_result = surprise_result,
    class = c("bs_surprise_sf", class(sf_data))
  )
}

#' @export
print.bs_surprise_sf <- function(x, ..., n = 6L) {
  cat("Bayesian Surprise Map\n")
  cat("=====================\n")

  result <- attr(x, "surprise_result")
  if (!is.null(result)) {
    cat("Models:", result$model_space$n_models, "\n")
    cat("Surprise range:",
        round(min(result$surprise, na.rm = TRUE), 4), "to",
        round(max(result$surprise, na.rm = TRUE), 4), "\n")
    if (!is.null(result$signed_surprise)) {
      cat("Signed surprise range:",
          round(min(result$signed_surprise, na.rm = TRUE), 4), "to",
          round(max(result$signed_surprise, na.rm = TRUE), 4), "\n")
    }
  }
  cat("\n")

  # Print as sf
  NextMethod()
}

# Surprise Temporal Classes ----------------------------------------------------

#' Create a new temporal surprise result object
#'
#' Internal constructor for temporal surprise results.
#'
#' @param surprise_by_time List of `bs_surprise` objects by time period
#' @param time_values Vector of time values
#' @param cumulative_surprise Matrix of cumulative surprise over time
#' @param model_space The `bs_model_space` used
#' @param data_info List with metadata
#'
#' @return A `bs_surprise_temporal` object
#' @noRd
new_bs_surprise_temporal <- function(surprise_by_time,
                                      time_values,
                                      cumulative_surprise = NULL,
                                      model_space,
                                      data_info = list()) {
  structure(
    list(
      surprise_by_time = surprise_by_time,
      time_values = time_values,
      cumulative_surprise = cumulative_surprise,
      model_space = model_space,
      data_info = data_info
    ),
    class = "bs_surprise_temporal"
  )
}

#' @export
print.bs_surprise_temporal <- function(x, ...) {
  cat("<bs_surprise_temporal>\n")
  cat("  Time periods:", length(x$time_values), "\n")
  cat("  Time range:", min(x$time_values), "to", max(x$time_values), "\n")
  cat("  Models:", x$model_space$n_models, "\n")
  invisible(x)
}

# Helper Functions -------------------------------------------------------------
#' Extract surprise values from result objects
#'
#' @param x A `bs_surprise`, `bs_surprise_sf`, or `bs_surprise_temporal` object
#' @param type Which surprise to extract: "surprise" or "signed"
#' @param ... Additional arguments (unused)
#'
#' @return Numeric vector of surprise values
#' @export
get_surprise <- function(x, type = c("surprise", "signed"), ...) {
  type <- match.arg(type)
  UseMethod("get_surprise")
}

#' @export
get_surprise.bs_surprise <- function(x, type = c("surprise", "signed"), ...) {
  type <- match.arg(type)
  if (type == "signed") {
    x$signed_surprise %||% x$surprise
  } else {
    x$surprise
  }
}

#' @export
get_surprise.bs_surprise_sf <- function(x, type = c("surprise", "signed"), ...) {

  type <- match.arg(type)
  result <- attr(x, "surprise_result")
  if (is.null(result)) {
    if (type == "signed" && "signed_surprise" %in% names(x)) {
      return(x$signed_surprise)
    }
    return(x$surprise)
  }
  get_surprise.bs_surprise(result, type)
}

#' @export
get_surprise.bs_surprise_df <- function(x, type = c("surprise", "signed"), ...) {
  type <- match.arg(type)
  if (type == "signed" && "signed_surprise" %in% names(x)) {
    return(x$signed_surprise)
  }
  x$surprise
}

#' Get the model space from a surprise result
#'
#' @param x A `bs_surprise` or `bs_surprise_sf` object
#' @param ... Additional arguments (unused)
#'
#' @return A `bs_model_space` object
#' @export
get_model_space <- function(x, ...) {
  UseMethod("get_model_space")
}

#' @export
get_model_space.bs_surprise <- function(x, ...) {
  x$model_space
}

#' @export
get_model_space.bs_surprise_sf <- function(x, ...) {
  result <- attr(x, "surprise_result")
  if (!is.null(result)) {
    result$model_space
  } else {
    NULL
  }
}
