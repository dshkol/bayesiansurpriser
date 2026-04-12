#' bayesiansurpriser: Bayesian Surprise for De-Biasing Thematic Maps
#'
#' @description
#' Implements Bayesian Surprise calculations for data visualization, inspired by
#' Correll & Heer (2017) "Surprise! Bayesian Weighting for De-Biasing Thematic
#' Maps" (IEEE InfoVis 2016).
#'
#' The technique measures "surprise" using KL-divergence between prior and
#' posterior probability distributions across a model space. This approach
#' can help analyze three key biases in thematic maps:
#'
#' 1. **Base rate bias**: Visual prominence dominated by underlying rates
#'    (e.g., population density)
#' 2. **Sampling error bias**: Sparse regions show misleadingly high variability
#' 3. **Renormalization bias**: Dynamic scaling suppresses important patterns
#'
#' @section Main Functions:
#' - [surprise()]: Compute Bayesian surprise for spatial or tabular data
#' - [st_surprise()]: Convenience wrapper for sf objects
#' - [surprise_temporal()]: Compute surprise over time series
#'
#' @section Model Constructors:
#' - [bs_model_uniform()]: Uniform/equiprobable model
#' - [bs_model_baserate()]: Base rate model (e.g., population-weighted)
#' - [bs_model_gaussian()]: Parametric Gaussian model
#' - [bs_model_sampled()]: Non-parametric KDE model
#' - [bs_model_funnel()]: de Moivre funnel model for sampling bias
#' - [model_space()]: Combine models into a model space
#'
#' @section ggplot2 Integration:
#' - [stat_surprise()]: Compute surprise as a ggplot2 stat
#' - [geom_surprise()]: Convenience wrapper for surprise maps
#' - [scale_fill_surprise()]: Sequential color scale
#' - [scale_fill_surprise_diverging()]: Diverging scale for signed surprise
#'
#' @references
#' Correll, M., & Heer, J. (2017). Surprise! Bayesian Weighting for De-Biasing
#' Thematic Maps. IEEE Transactions on Visualization and Computer Graphics,
#' 23(1), 651-660. \doi{10.1109/TVCG.2016.2598839}
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang %||%
#' @importFrom rlang .data
#' @importFrom stats sd dnorm dpois dmultinom density approx weighted.mean
#' @importFrom utils modifyList
#' @importFrom ggplot2 waiver
#' @importFrom MASS kde2d bandwidth.nrd
#' @importFrom cli cli_abort cli_warn cli_inform
#' @importFrom scales muted
## usethis namespace: end
NULL
