# Main Surprise Computation Functions
#
# User-facing functions for computing Bayesian surprise.

#' Compute Bayesian Surprise
#'
#' Main function to compute Bayesian surprise for spatial or tabular data.
#' This measures how much each observation updates beliefs about a set of models,
#' highlighting unexpected patterns while de-biasing against known factors.
#'
#' @param data Data frame, tibble, or sf object
#' @param observed Column name (unquoted or string) or numeric vector of observed values
#' @param expected Column name or vector of expected values (for base rate model).
#'   If NULL and models include base rate, computed from observed.
#' @param sample_size Column name or vector of sample sizes (for funnel model).
#'   Defaults to `expected` if not provided.
#' @param models Model specification. Can be:
#'   - A `bs_model_space` object
#'   - A character vector of model types: "uniform", "baserate", "gaussian",
#'     "sampled", "funnel"
#'   - A list of `bs_model` objects
#' @param prior Numeric vector of prior probabilities for models.
#'   Only used when `models` is a character vector or list.
#' @param signed Logical; compute signed surprise?
#' @param normalize_posterior Logical; if TRUE (default), normalizes posteriors
#'   before computing KL divergence. This is the standard Bayesian Surprise
#'   calculation. If FALSE, uses the unnormalized per-region posterior weights
#'   used by the original Correll & Heer JavaScript demo; this option is
#'   retained only for legacy comparison.
#' @param ... Additional arguments passed to model likelihood functions
#'
#' @return For data frames: the input with `surprise` (and optionally
#'   `signed_surprise`) columns added, plus a `surprise_result` attribute.
#'   For sf objects: a `bs_surprise_sf` object.
#'
#' @export
#' @examples
#' # Using sf package's NC data
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'
#' # Basic usage with default models
#' result <- surprise(nc, observed = SID74, expected = BIR74)
#'
#' # With specific model types
#' result <- surprise(nc,
#'   observed = "SID74",
#'   expected = "BIR74",
#'   models = c("uniform", "baserate", "funnel")
#' )
#'
#' # With custom model space
#' space <- model_space(
#'   bs_model_uniform(),
#'   bs_model_baserate(nc$BIR74)
#' )
#' result <- surprise(nc, observed = SID74, models = space)
#'
#' # View results
#' plot(result, which = "signed_surprise")
surprise <- function(data,
                     observed,
                     expected = NULL,
                     sample_size = NULL,
                     models = c("uniform", "baserate", "funnel"),
                     prior = NULL,
                     signed = TRUE,
                     normalize_posterior = TRUE,
                     ...) {
  UseMethod("surprise")
}

#' @export
#' @rdname surprise
surprise.data.frame <- function(data,
                                 observed,
                                 expected = NULL,
                                 sample_size = NULL,
                                 models = c("uniform", "baserate", "funnel"),
                                 prior = NULL,
                                 signed = TRUE,
                                 normalize_posterior = TRUE,
                                 ...) {
  # Extract observed values
  obs_vals <- extract_column(data, rlang::enquo(observed))

  # Extract expected values if provided
  exp_vals <- NULL
  expected_quo <- rlang::enquo(expected)
  if (!rlang::quo_is_null(expected_quo)) {
    exp_vals <- extract_column(data, expected_quo)
  }

  # Extract sample sizes if provided
  size_vals <- NULL
  sample_size_quo <- rlang::enquo(sample_size)
  if (!rlang::quo_is_null(sample_size_quo)) {
    size_vals <- extract_column(data, sample_size_quo)
  } else if (!is.null(exp_vals)) {
    size_vals <- exp_vals
  }

  # Build or validate model space
  model_space <- build_model_space_from_spec(models, exp_vals, size_vals, prior)

  # Compute surprise
  result <- compute_surprise(
    model_space = model_space,
    observed = obs_vals,
    expected = exp_vals,
    return_signed = signed,
    normalize_posterior = normalize_posterior,
    ...
  )

  # Add results to data
  data$surprise <- result$surprise
  if (signed && !is.null(result$signed_surprise)) {
    data$signed_surprise <- result$signed_surprise
  }

  # Return augmented data with attribute
  structure(data, surprise_result = result, class = c("bs_surprise_df", class(data)))
}

#' @export
#' @rdname surprise
surprise.tbl_df <- function(data, ...) {
  result <- surprise.data.frame(data, ...)
  class(result) <- c("bs_surprise_df", "tbl_df", "tbl", "data.frame")
  result
}

#' @export
print.bs_surprise_df <- function(x, ...) {
  cat("Bayesian Surprise Result\n")
  cat("========================\n")
  result <- attr(x, "surprise_result")
  if (!is.null(result)) {
    cat("Models:", result$model_space$n_models, "\n")
    cat("Surprise range:",
        round(min(result$surprise, na.rm = TRUE), 4), "to",
        round(max(result$surprise, na.rm = TRUE), 4), "\n")
  }
  cat("\n")
  NextMethod()
}

# Helper Functions -------------------------------------------------------------

#' Extract Column from Data
#'
#' Handles both quoted and unquoted column names, as well as direct vectors.
#'
#' @param data Data frame
#' @param col Quosure or string or numeric vector
#'
#' @return Numeric vector
#' @noRd
extract_column <- function(data, col) {
  # Handle direct numeric input
  if (is.numeric(col)) {
    return(col)
  }

  # Handle quosure (unquoted column name)
  if (rlang::is_quosure(col)) {
    # Check if it's a symbol (column name)
    if (rlang::quo_is_symbol(col)) {
      col_name <- rlang::as_name(col)
      if (!col_name %in% names(data)) {
        cli_abort("Column {.val {col_name}} not found in data.")
      }
      return(data[[col_name]])
    }

    # Try to evaluate
    val <- tryCatch(
      rlang::eval_tidy(col, data),
      error = function(e) {
        # Maybe it's a string
        expr <- rlang::quo_get_expr(col)
        if (is.character(expr) && length(expr) == 1) {
          return(data[[expr]])
        }
        cli_abort("Cannot extract column: {e$message}")
      }
    )

    # If val is a string, treat it as a column name
    if (is.character(val) && length(val) == 1 && val %in% names(data)) {
      return(data[[val]])
    }

    return(val)
  }

  # Handle string column name
  if (is.character(col) && length(col) == 1) {
    if (!col %in% names(data)) {
      cli_abort("Column {.val {col}} not found in data.")
    }
    return(data[[col]])
  }

  cli_abort("Cannot extract column from {.cls {class(col)}} input.")
}

#' Build Model Space from Specification
#'
#' Converts various model specifications into a `bs_model_space`.
#'
#' @param spec Model specification
#' @param expected Expected values (for baserate/funnel)
#' @param sample_size Sample sizes (for funnel)
#' @param prior Prior probabilities
#'
#' @return A `bs_model_space` object
#' @noRd
build_model_space_from_spec <- function(spec, expected = NULL, sample_size = NULL, prior = NULL) {
  # Already a model space
  if (inherits(spec, "bs_model_space")) {
    return(spec)
  }

  # List of models
  if (is.list(spec) && all(vapply(spec, inherits, logical(1), "bs_model"))) {
    return(model_space(spec, prior = prior))
  }

  # Character vector of model types
  if (is.character(spec)) {
    models <- lapply(spec, function(type) {
      switch(type,
        uniform = bs_model_uniform(),
        baserate = {
          if (is.null(expected)) {
            cli_warn("No expected values for baserate model; using uniform.")
            bs_model_uniform()
          } else {
            bs_model_baserate(expected)
          }
        },
        gaussian = bs_model_gaussian(),
        sampled = bs_model_sampled(),
        kde = bs_model_sampled(),
        funnel = {
          ss <- sample_size %||% expected
          if (is.null(ss)) {
            cli_warn("No sample sizes for funnel model; skipping.")
            NULL
          } else {
            bs_model_funnel(ss)
          }
        },
        cli_abort("Unknown model type: {.val {type}}")
      )
    })
    # Remove NULLs
    models <- Filter(Negate(is.null), models)
    if (length(models) == 0) {
      cli_abort("No valid models could be created.")
    }
    return(model_space(models, prior = prior))
  }

  cli_abort("Cannot build model space from {.cls {class(spec)}}.")
}

#' Compute Surprise with Automatic Model Selection
#'
#' Simplified interface that automatically selects appropriate models and
#' computes per-observation surprise from the model priors.
#'
#' @param observed Numeric vector of observed values
#' @param expected Numeric vector of expected values (optional)
#' @param sample_size Numeric vector of sample sizes (optional)
#' @param include_gaussian Include Gaussian model?
#' @param include_sampled Include KDE model?
#' @param signed Compute signed surprise?
#' @param ... Additional arguments
#'
#' @return A `bs_surprise` object
#'
#' @export
#' @examples
#' observed <- c(50, 100, 150, 200, 75)
#' expected <- c(10000, 50000, 100000, 25000, 15000)
#' result <- auto_surprise(observed, expected)
auto_surprise <- function(observed,
                           expected = NULL,
                           sample_size = NULL,
                           include_gaussian = FALSE,
                           include_sampled = FALSE,
                           signed = TRUE,
                           ...) {
  models <- list(bs_model_uniform())

  if (!is.null(expected)) {
    models <- c(models, list(bs_model_baserate(expected)))
  }

  ss <- sample_size %||% expected
  if (!is.null(ss)) {
    models <- c(models, list(bs_model_funnel(ss)))
  }

  if (include_gaussian) {
    models <- c(models, list(bs_model_gaussian()))
  }

  if (include_sampled) {
    models <- c(models, list(bs_model_sampled()))
  }

  space <- model_space(models)

  result <- compute_surprise(
    model_space = space,
    observed = observed,
    expected = expected,
    return_signed = signed,
    ...
  )

  result
}
