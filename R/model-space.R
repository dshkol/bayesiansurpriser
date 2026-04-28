# Model Space Creation and Management
#
# Functions for creating and managing collections of models.

#' Create a Model Space
#'
#' Combines multiple models into a model space with prior probabilities.
#' The model space represents the set of hypotheses about how data is generated.
#'
#' @param ... `bs_model` objects or a list of models
#' @param prior Numeric vector of prior probabilities (must sum to 1).
#'   If NULL (default), uses uniform prior.
#' @param names Optional character vector of names for models
#'
#' @return A `bs_model_space` object
#'
#' @export
#' @examples
#' # Create model space with uniform prior
#' space <- model_space(
#'   bs_model_uniform(),
#'   bs_model_gaussian()
#' )
#'
#' # Create with custom prior
#' space <- model_space(
#'   bs_model_uniform(),
#'   bs_model_baserate(c(0.2, 0.3, 0.5)),
#'   prior = c(0.3, 0.7)
#' )
#'
#' # Create from list
#' models <- list(bs_model_uniform(), bs_model_gaussian())
#' space <- model_space(models)
model_space <- function(..., prior = NULL, names = NULL) {
  models <- list(...)

  # Handle case where a single list is passed
  if (length(models) == 1 && is.list(models[[1]]) &&
      !inherits(models[[1]], "bs_model")) {
    models <- models[[1]]
  }

  if (length(models) == 0) {
    cli_abort("At least one model must be provided.")
  }

  new_bs_model_space(models, prior, names)
}

#' Default Model Space
#'
#' Creates a default model space suitable for most choropleth/thematic maps.
#' Includes uniform, base rate, and funnel models.
#'
#' @param expected Numeric vector of expected values (e.g., population).
#'   Used for base rate and funnel models.
#' @param sample_size Numeric vector of sample sizes. Defaults to `expected`
#'   if not provided.
#' @param include_gaussian Logical; include Gaussian model?
#' @param prior Prior probabilities for models. Default is uniform.
#'
#' @return A `bs_model_space` object
#'
#' @export
#' @examples
#' # Default space with population as expected
#' population <- c(10000, 50000, 100000, 25000)
#' space <- default_model_space(population)
#'
#' # Include Gaussian model
#' space <- default_model_space(population, include_gaussian = TRUE)
default_model_space <- function(expected,
                                 sample_size = expected,
                                 include_gaussian = FALSE,
                                 prior = NULL) {
  models <- list(
    bs_model_uniform(),
    bs_model_baserate(expected),
    bs_model_funnel(sample_size)
  )

  if (include_gaussian) {
    models <- c(models, list(bs_model_gaussian()))
  }

  model_space(models, prior = prior)
}

#' Add Model to Space
#'
#' Adds a new model to an existing model space.
#'
#' @param model_space A `bs_model_space` object
#' @param model A `bs_model` object to add
#' @param prior_weight Prior probability for the new model. The existing
#'   priors are rescaled to accommodate.
#'
#' @return Updated `bs_model_space`
#'
#' @export
#' @examples
#' space <- model_space(bs_model_uniform())
#' space <- add_model(space, bs_model_gaussian(), prior_weight = 0.3)
add_model <- function(model_space, model, prior_weight = NULL) {
  if (!inherits(model_space, "bs_model_space")) {
    cli_abort("{.arg model_space} must be a {.cls bs_model_space} object.")
  }
  if (!inherits(model, "bs_model")) {
    cli_abort("{.arg model} must be a {.cls bs_model} object.")
  }

  new_models <- c(model_space$models, list(model))

  if (is.null(prior_weight)) {
    # Uniform weight for new model
    prior_weight <- 1 / (model_space$n_models + 1)
  }

  # Rescale existing priors
  old_priors <- model_space$prior * (1 - prior_weight)
  new_prior <- c(old_priors, prior_weight)

  model_space(new_models, prior = new_prior)
}

#' Remove Model from Space
#'
#' Removes a model from an existing model space.
#'
#' @param model_space A `bs_model_space` object
#' @param which Integer index or character name of model to remove
#'
#' @return Updated `bs_model_space`
#'
#' @export
remove_model <- function(model_space, which) {
  if (!inherits(model_space, "bs_model_space")) {
    cli_abort("{.arg model_space} must be a {.cls bs_model_space} object.")
  }

  if (is.character(which)) {
    idx <- match(which, names(model_space$models))
    if (is.na(idx)) {
      cli_abort("Model {.val {which}} not found in model space.")
    }
  } else {
    idx <- which
  }

  if (idx < 1 || idx > model_space$n_models) {
    cli_abort("Invalid model index: {idx}")
  }

  new_models <- model_space$models[-idx]
  new_prior <- normalize_prob(model_space$prior[-idx])

  model_space(new_models, prior = new_prior)
}

#' Set Prior Probabilities
#'
#' Updates the prior probabilities of a model space.
#'
#' @param model_space A `bs_model_space` object
#' @param prior Numeric vector of new prior probabilities (must sum to 1)
#'
#' @return Updated `bs_model_space`
#'
#' @export
set_prior <- function(model_space, prior) {
  if (!inherits(model_space, "bs_model_space")) {
    cli_abort("{.arg model_space} must be a {.cls bs_model_space} object.")
  }

  if (length(prior) != model_space$n_models) {
    cli_abort("{.arg prior} must have length {model_space$n_models}.")
  }

  prior <- normalize_prob(prior)
  model_space$prior <- prior
  names(model_space$prior) <- names(model_space$models)

  model_space
}

#' Get Model Names
#'
#' @param model_space A `bs_model_space` object
#' @return Character vector of model names
#' @export
model_names <- function(model_space) {
  names(model_space$models)
}

#' Get Number of Models
#'
#' @param model_space A `bs_model_space` object
#' @return Integer number of models
#' @export
n_models <- function(model_space) {
  model_space$n_models
}
