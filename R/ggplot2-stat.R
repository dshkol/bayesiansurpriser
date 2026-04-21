# ggplot2 Stat for Bayesian Surprise
#
# Computes surprise as a ggplot2 stat layer.

#' StatSurprise ggproto Object
#'
#' @format A ggproto object.
#' @keywords internal
#' @export
StatSurprise <- ggplot2::ggproto("StatSurprise", ggplot2::Stat,
  required_aes = c("observed"),
  optional_aes = c("expected", "sample_size"),

  default_aes = ggplot2::aes(
    fill = ggplot2::after_stat(surprise)
  ),

  setup_params = function(data, params) {
    params$models <- params$models %||% c("uniform", "baserate", "funnel")
    params$signed <- params$signed %||% TRUE
    params
  },

  compute_group = function(data, scales, models = c("uniform", "baserate", "funnel"),
                           signed = TRUE, na.rm = FALSE) {
    # Extract values
    observed <- data$observed
    expected <- data$expected
    sample_size <- data$sample_size %||% expected

    # Build model space
    mspace <- build_model_space_from_spec(models, expected, sample_size, prior = NULL)

    # Compute surprise
    result <- compute_surprise(
      model_space = mspace,
      observed = observed,
      expected = expected,
      return_signed = signed
    )

    # Add to data
    data$surprise <- result$surprise
    if (signed && !is.null(result$signed_surprise)) {
      data$signed_surprise <- result$signed_surprise
    }

    data
  }
)

#' Compute Surprise as ggplot2 Stat
#'
#' This stat computes Bayesian surprise for sf geometries, allowing you to
#' visualize surprise directly in ggplot2.
#'
#' @param mapping Aesthetic mapping created by [ggplot2::aes()].
#'   Required aesthetics are `geometry` (from sf) and `observed`.
#'   Optional aesthetics include `expected` and `sample_size`.
#' @param data Data (typically an sf object)
#' @param geom Geometry to use (default: "sf")
#' @param position Position adjustment
#' @param na.rm Remove NA values?
#' @param show.legend Show legend?
#' @param inherit.aes Inherit aesthetics from ggplot?
#' @param models Character vector of model types to use.
#'   Options: "uniform", "baserate", "gaussian", "sampled", "funnel"
#' @param signed Logical; compute signed surprise?
#' @param ... Additional arguments passed to the layer
#'
#' @return A ggplot2 layer
#'
#' @section Computed Variables:
#' \describe{
#'   \item{surprise}{Bayesian surprise (KL-divergence)}
#'   \item{signed_surprise}{Signed surprise (if signed = TRUE)}
#' }
#'
#' @export
#' @examples
#' library(ggplot2)
#' library(sf)
#'
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'
#' # Basic surprise map - geometry must be mapped explicitly
#' ggplot(nc) +
#'   stat_surprise(aes(geometry = geometry, observed = SID74, expected = BIR74)) +
#'   scale_fill_surprise()
stat_surprise <- function(mapping = NULL, data = NULL,
                          geom = "sf", position = "identity",
                          na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE,
                          models = c("uniform", "baserate", "funnel"),
                          signed = TRUE,
                          ...) {
  c(
    ggplot2::layer(
      stat = StatSurprise,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        models = models,
        signed = signed,
        ...
      )
    ),
    ggplot2::coord_sf(default = TRUE)
  )
}

#' StatSurpriseSf ggproto Object (for sf integration)
#'
#' A specialized stat that works with sf geometries.
#'
#' @format A ggproto object.
#' @keywords internal
#' @export
StatSurpriseSf <- ggplot2::ggproto("StatSurpriseSf", ggplot2::StatSf,
  required_aes = c("geometry", "observed"),
  optional_aes = c("expected", "sample_size"),

  default_aes = ggplot2::aes(
    fill = ggplot2::after_stat(surprise)
  ),

  compute_group = function(data, scales, coord,
                           models = c("uniform", "baserate", "funnel"),
                           signed = TRUE, na.rm = FALSE) {
    # Extract values
    observed <- data$observed
    expected <- data$expected
    sample_size <- data$sample_size %||% expected

    # Build model space
    mspace <- build_model_space_from_spec(models, expected, sample_size, prior = NULL)

    # Compute surprise
    result <- compute_surprise(
      model_space = mspace,
      observed = observed,
      expected = expected,
      return_signed = signed
    )

    # Add surprise columns
    data$surprise <- result$surprise
    if (signed && !is.null(result$signed_surprise)) {
      data$signed_surprise <- result$signed_surprise
    }

    # Call parent to handle sf geometry
    ggplot2::ggproto_parent(ggplot2::StatSf, StatSurpriseSf)$compute_group(
      data, scales, coord
    )
  }
)

#' Stat for Surprise with sf Geometries
#'
#' @inheritParams stat_surprise
#' @return A list containing a ggplot2 layer using [StatSurpriseSf] and
#'   [ggplot2::coord_sf()]. The stat computes `surprise` and, when `signed =
#'   TRUE`, `signed_surprise`, which are available to downstream geoms via
#'   `after_stat()`.
#' @export
stat_surprise_sf <- function(mapping = NULL, data = NULL,
                              geom = ggplot2::GeomSf,
                              position = "identity",
                              na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE,
                              models = c("uniform", "baserate", "funnel"),
                              signed = TRUE,
                              ...) {
  c(
    ggplot2::layer(
      stat = StatSurpriseSf,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        models = models,
        signed = signed,
        ...
      )
    ),
    ggplot2::coord_sf(default = TRUE)
  )
}
