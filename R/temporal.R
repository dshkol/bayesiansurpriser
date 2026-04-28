# Temporal and Streaming Surprise Functions
#
# Functions for computing surprise over time series and streaming data.

#' Compute Temporal Surprise
#'
#' Computes surprise over time, allowing beliefs to update as new data arrives.
#' This is useful for detecting changes in patterns over time and for
#' streaming data applications.
#'
#' @param data Data frame with time-indexed observations
#' @param time_col Column name for time variable (unquoted or string)
#' @param observed Column name for observed values
#' @param expected Column name for expected values (optional)
#' @param region_col Column name for region/spatial identifier (optional).
#'   If provided, computes per-region surprise at each time point.
#' @param models Model specification (see [surprise()])
#' @param update_prior Logical; should prior be updated after each time step?
#' @param window_size For rolling window analysis: number of time periods to include.
#'   If NULL, uses all prior data.
#' @param signed Compute signed surprise?
#' @param ... Additional arguments passed to [compute_surprise()]
#'
#' @return A `bs_surprise_temporal` object containing:
#'   - `surprise_by_time`: List of surprise results for each time period
#'   - `time_values`: Vector of time values
#'   - `cumulative_surprise`: Matrix of cumulative surprise
#'   - `model_space`: The model space used
#'
#' @export
#' @examples
#' # Create temporal data
#' df <- data.frame(
#'   year = rep(2010:2020, each = 5),
#'   region = rep(letters[1:5], 11),
#'   events = rpois(55, lambda = 50),
#'   population = rep(c(10000, 50000, 100000, 25000, 15000), 11)
#' )
#'
#' # Compute temporal surprise
#' result <- surprise_temporal(df,
#'   time_col = year,
#'   observed = events,
#'   expected = population,
#'   region_col = region
#' )
#'
#' # View results
#' print(result)
surprise_temporal <- function(data,
                               time_col,
                               observed,
                               expected = NULL,
                               region_col = NULL,
                               models = c("uniform", "baserate", "funnel"),
                               update_prior = TRUE,
                               window_size = NULL,
                               signed = TRUE,
                               ...) {
  # Extract column values
  time_vals <- extract_column(data, rlang::enquo(time_col))
  obs_vals <- extract_column(data, rlang::enquo(observed))

  exp_vals <- NULL
  expected_quo <- rlang::enquo(expected)
  if (!rlang::quo_is_null(expected_quo)) {
    exp_vals <- extract_column(data, expected_quo)
  }

  region_vals <- NULL
  region_quo <- rlang::enquo(region_col)
  if (!rlang::quo_is_null(region_quo)) {
    region_vals <- extract_column(data, region_quo)
  }

  # Get unique time values in order
  time_unique <- sort(unique(time_vals))
  n_times <- length(time_unique)

  # Initialize storage
  surprise_by_time <- vector("list", n_times)
  names(surprise_by_time) <- as.character(time_unique)

  # Build initial model space
  if (!is.null(region_vals)) {
    # For region-based analysis, get expected values for one time slice
    first_time_idx <- time_vals == time_unique[1]
    exp_slice <- if (!is.null(exp_vals)) exp_vals[first_time_idx] else NULL
    size_slice <- exp_slice
  } else {
    exp_slice <- exp_vals
    size_slice <- exp_vals
  }

  current_space <- build_model_space_from_spec(models, exp_slice, size_slice, prior = NULL)

  # Process each time period
  for (t in seq_along(time_unique)) {
    time_val <- time_unique[t]
    time_idx <- time_vals == time_val

    # Get data for this time period
    obs_t <- obs_vals[time_idx]
    exp_t <- if (!is.null(exp_vals)) exp_vals[time_idx] else NULL

    # Update model space for this time period if needed
    if (!is.null(exp_t) && t > 1) {
      # Rebuild model space with current expected values
      current_space <- build_model_space_from_spec(
        models, exp_t, exp_t,
        prior = if (update_prior) current_space$posterior else NULL
      )
    }

    # Compute surprise for this time period
    result_t <- compute_surprise(
      model_space = current_space,
      observed = obs_t,
      expected = exp_t,
      return_signed = signed,
      ...
    )

    # Store results with region labels if available
    if (!is.null(region_vals)) {
      result_t$regions <- region_vals[time_idx]
    }

    surprise_by_time[[t]] <- result_t

    # Update prior for next iteration if requested
    if (update_prior && !is.null(result_t$model_space$posterior)) {
      current_space$prior <- result_t$model_space$posterior
    }
  }

  # Compute cumulative surprise matrix
  if (!is.null(region_vals)) {
    regions_unique <- unique(region_vals)
    n_regions <- length(regions_unique)
    cumulative_surprise <- matrix(NA, n_times, n_regions)
    rownames(cumulative_surprise) <- as.character(time_unique)
    colnames(cumulative_surprise) <- as.character(regions_unique)

    for (t in seq_along(time_unique)) {
      result_t <- surprise_by_time[[t]]
      for (r in seq_along(regions_unique)) {
        reg <- regions_unique[r]
        idx <- which(result_t$regions == reg)
        if (length(idx) > 0) {
          cumulative_surprise[t, r] <- result_t$surprise[idx[1]]
        }
      }
    }
  } else {
    cumulative_surprise <- matrix(
      vapply(surprise_by_time, function(x) sum(x$surprise, na.rm = TRUE), numeric(1)),
      ncol = 1
    )
    rownames(cumulative_surprise) <- as.character(time_unique)
  }

  # Create temporal result object
  new_bs_surprise_temporal(
    surprise_by_time = surprise_by_time,
    time_values = time_unique,
    cumulative_surprise = cumulative_surprise,
    model_space = current_space,
    data_info = list(
      n_times = n_times,
      n_obs = length(obs_vals),
      has_regions = !is.null(region_vals),
      n_regions = if (!is.null(region_vals)) length(unique(region_vals)) else 1
    )
  )
}

#' Update Surprise with New Data (Streaming)
#'
#' Updates an existing surprise result with new observations.
#' Useful for real-time or streaming data applications.
#'
#' @param surprise_result An existing `bs_surprise` or `bs_surprise_temporal` object
#' @param new_observed New observed values
#' @param new_expected New expected values (optional)
#' @param time_value Time value for the new observation (for temporal)
#' @param update_prior Update prior with current posterior?
#'
#' @return Updated surprise result
#'
#' @export
#' @examples
#' # Initial computation
#' observed <- c(50, 100, 150)
#' expected <- c(10000, 50000, 100000)
#' result <- auto_surprise(observed, expected)
#'
#' # Update with new data
#' new_obs <- c(200, 75)
#' new_exp <- c(80000, 20000)
#' result <- update_surprise(result, new_obs, new_exp)
update_surprise <- function(surprise_result,
                             new_observed,
                             new_expected = NULL,
                             time_value = NULL,
                             update_prior = TRUE) {
  UseMethod("update_surprise")
}

#' @export
update_surprise.bs_surprise <- function(surprise_result,
                                         new_observed,
                                         new_expected = NULL,
                                         time_value = NULL,
                                         update_prior = TRUE) {
  # Get current model space
  mspace <- surprise_result$model_space

  # Update prior if requested
  if (update_prior && !is.null(mspace$posterior)) {
    mspace$prior <- mspace$posterior
  }

  # Rebuild model space if new_expected is provided (for new observations).
  # Preserve the model types from the existing space so a custom selection
  # is not silently overridden. Skip the rebuild if any model type is outside
  # the spec-builder vocabulary, since rebuilding would error.
  if (!is.null(new_expected)) {
    existing_types <- vapply(mspace$models, `[[`, character(1), "type")
    rebuildable <- c("uniform", "baserate", "gaussian", "sampled", "kde", "funnel")
    if (all(existing_types %in% rebuildable)) {
      mspace <- build_model_space_from_spec(
        existing_types,
        new_expected,
        new_expected,
        prior = mspace$prior
      )
    }
  }

  # First do global Bayesian update to get posterior
  updated_mspace <- bayesian_update(mspace, new_observed)

  # Compute surprise for new data
  new_result <- compute_surprise(
    model_space = updated_mspace,
    observed = new_observed,
    expected = new_expected,
    return_signed = !is.null(surprise_result$signed_surprise)
  )

  # Combine results
  combined <- new_bs_surprise(
    surprise = c(surprise_result$surprise, new_result$surprise),
    signed_surprise = if (!is.null(surprise_result$signed_surprise)) {
      c(surprise_result$signed_surprise, new_result$signed_surprise)
    } else {
      NULL
    },
    model_space = new_result$model_space,
    posteriors = NULL,  # Would need to combine properly
    model_contributions = NULL,
    data_info = list(
      n = length(surprise_result$surprise) + length(new_result$surprise),
      n_updates = (surprise_result$data_info$n_updates %||% 0) + 1
    )
  )

  combined
}

#' @export
update_surprise.bs_surprise_temporal <- function(surprise_result,
                                                  new_observed,
                                                  new_expected = NULL,
                                                  time_value = NULL,
                                                  update_prior = TRUE) {
  if (is.null(time_value)) {
    # Use next time value
    time_value <- max(surprise_result$time_values) + 1
  }

  # Get current model space with updated prior
  mspace <- surprise_result$model_space
  if (update_prior && !is.null(mspace$posterior)) {
    mspace$prior <- mspace$posterior
  }

  # Rebuild model space if expected values changed, preserving the existing
  # model selection. Skip the rebuild for spaces containing model types the
  # spec builder doesn't know about.
  if (!is.null(new_expected)) {
    existing_types <- vapply(mspace$models, `[[`, character(1), "type")
    rebuildable <- c("uniform", "baserate", "gaussian", "sampled", "kde", "funnel")
    if (all(existing_types %in% rebuildable)) {
      mspace <- build_model_space_from_spec(
        existing_types,
        new_expected,
        new_expected,
        prior = mspace$prior
      )
    }
  }

  # Compute surprise for new time period
  new_result <- compute_surprise(
    model_space = mspace,
    observed = new_observed,
    expected = new_expected,
    return_signed = TRUE
  )

  # Add to temporal result
  surprise_result$surprise_by_time <- c(
    surprise_result$surprise_by_time,
    list(new_result)
  )
  names(surprise_result$surprise_by_time)[length(surprise_result$surprise_by_time)] <-
    as.character(time_value)

  surprise_result$time_values <- c(surprise_result$time_values, time_value)

  # Update cumulative surprise matrix
  new_row <- matrix(new_result$surprise, nrow = 1)
  rownames(new_row) <- as.character(time_value)
  surprise_result$cumulative_surprise <- rbind(
    surprise_result$cumulative_surprise,
    new_row
  )

  surprise_result$model_space <- new_result$model_space
  surprise_result$data_info$n_times <- surprise_result$data_info$n_times + 1

  surprise_result
}

#' Get Surprise at Specific Time
#'
#' Extracts surprise values for a specific time period from temporal results.
#'
#' @param temporal_result A `bs_surprise_temporal` object
#' @param time Time value to extract
#'
#' @return A `bs_surprise` object for that time period
#'
#' @export
get_surprise_at_time <- function(temporal_result, time) {
  if (!inherits(temporal_result, "bs_surprise_temporal")) {
    cli_abort("{.arg temporal_result} must be a bs_surprise_temporal object.")
  }

  idx <- which(temporal_result$time_values == time)
  if (length(idx) == 0) {
    cli_abort("Time value {.val {time}} not found.")
  }

  temporal_result$surprise_by_time[[idx]]
}

#' Create Animation-Ready Data from Temporal Results
#'
#' Converts temporal surprise results into a format suitable for animation
#' (e.g., with gganimate).
#'
#' @param temporal_result A `bs_surprise_temporal` object
#' @param include_posterior Include posterior probabilities in output?
#'
#' @return A data frame with columns: time, region (if applicable),
#'   surprise, signed_surprise, and optionally model posteriors
#'
#' @export
surprise_animate <- function(temporal_result, include_posterior = FALSE) {
  if (!inherits(temporal_result, "bs_surprise_temporal")) {
    cli_abort("{.arg temporal_result} must be a bs_surprise_temporal object.")
  }

  # Build data frame from time slices
  result_list <- lapply(seq_along(temporal_result$time_values), function(t) {
    time_val <- temporal_result$time_values[t]
    result_t <- temporal_result$surprise_by_time[[t]]

    df <- data.frame(
      time = time_val,
      surprise = result_t$surprise,
      signed_surprise = result_t$signed_surprise %||% result_t$surprise
    )

    if (!is.null(result_t$regions)) {
      df$region <- result_t$regions
    }

    if (include_posterior && !is.null(result_t$posteriors)) {
      posterior_df <- as.data.frame(result_t$posteriors)
      names(posterior_df) <- paste0("posterior_", names(result_t$model_space$models))
      df <- cbind(df, posterior_df)
    }

    df
  })

  do.call(rbind, result_list)
}

#' Rolling Window Surprise
#'
#' Computes surprise using a rolling window of observations.
#'
#' @param observed Numeric vector of observed values (time-ordered)
#' @param expected Numeric vector of expected values
#' @param window_size Number of observations in the window
#' @param step Step size for moving the window
#' @param models Model specification
#' @param ... Additional arguments passed to [compute_surprise()]
#'
#' @return A list with surprise values for each window position
#'
#' @export
surprise_rolling <- function(observed,
                              expected = NULL,
                              window_size = 10,
                              step = 1,
                              models = c("uniform", "baserate", "funnel"),
                              ...) {
  n <- length(observed)
  if (window_size > n) {
    cli_abort("window_size ({window_size}) cannot exceed data length ({n}).")
  }

  # Window start positions
  starts <- seq(1, n - window_size + 1, by = step)
  n_windows <- length(starts)

  results <- vector("list", n_windows)

  for (i in seq_along(starts)) {
    start <- starts[i]
    end <- start + window_size - 1

    obs_window <- observed[start:end]
    exp_window <- if (!is.null(expected)) expected[start:end] else NULL

    mspace <- build_model_space_from_spec(models, exp_window, exp_window, prior = NULL)

    results[[i]] <- list(
      start = start,
      end = end,
      result = compute_surprise(
        model_space = mspace,
        observed = obs_window,
        expected = exp_window,
        ...
      )
    )
  }

  list(
    windows = results,
    window_size = window_size,
    step = step,
    n_windows = n_windows
  )
}
