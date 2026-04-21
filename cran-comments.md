## Test environments

* local macOS Sequoia 15.4.1, R 4.5.0

## R CMD check results

0 errors | 0 warnings | 3 notes

The notes are environment-related from running `R CMD check --as-cran` without
full external services available locally:

* CRAN incoming feasibility checks could not access CRAN/Bioconductor because
  the local environment had no internet access
* current time could not be verified locally
* HTML validation was skipped because HTML Tidy was not recent enough locally

## Resubmission

This is a resubmission in response to CRAN reviewer feedback.

The following changes were made:

* added a DOI-form reference link in `DESCRIPTION`
* added a `\\value` section for `stat_surprise_sf()`, documenting the returned
  layer object and the computed variables
* replaced commented examples in `bs_model_baserate_col()` and
  `bs_model_funnel_col()` with runnable toy examples

## API-backed vignettes

The tidycensus and cancensus workflow vignettes include examples that require
external API keys. Those chunks are not evaluated during CRAN checks. They are
rendered for the pkgdown documentation site when the relevant API keys are
available in the build environment.
