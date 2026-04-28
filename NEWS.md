# bayesiansurpriser 0.1.1

* `update_surprise()` now preserves the model selection from the original
  result when `new_expected` is supplied. Previously the model space was
  silently rebuilt as `c("uniform", "baserate", "funnel")`, dropping any
  custom models. Spaces containing model types outside the spec-builder
  vocabulary (e.g. `gaussian_mixture`, `bootstrap`) are left unchanged.
* `graphics` and `grDevices` are now declared in `Imports`, matching the
  qualified calls already used in plot methods.
* DESCRIPTION cleanup: removed redundant `Author` and `Maintainer` fields
  (`Authors@R` is the canonical source) and dropped unused `stats`
  imports (`dmultinom`, `weighted.mean`).
* Internal: tidied stray indentation and removed dead `!is.null(enquo())`
  guards in `surprise.data.frame()`.

# bayesiansurpriser 0.1.0

* Initial CRAN submission.
* Implements Bayesian Surprise calculations over explicit model spaces for
  tabular and spatial data.
* Provides model constructors for uniform, base-rate, Gaussian, sampled, and
  funnel model assumptions.
* Adds `sf` helpers, `ggplot2` stats/geoms/scales, and temporal update helpers.
* Includes vignettes for core workflows, spatial data, visualization, temporal
  analysis, and unevaluated API-backed census examples.
