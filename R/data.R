# Dataset Documentation
#
# This file contains roxygen2 documentation for the package datasets.

#' Canadian Mischief Crime Data by Province
#'
#' Crime data for Canadian provinces and territories, showing mischief offenses.
#' This dataset is adapted from the original Bayesian Surprise paper's Canada
#' example and is useful for exploring base-rate effects: Ontario and Quebec
#' dominate raw counts because of population, while model-based surprise scores
#' ask which provinces are unusual relative to the chosen model space.
#'
#' @format A data frame with 13 rows and 6 variables:
#' \describe{
#'   \item{name}{Province or territory name}
#'   \item{population}{Population count}
#'   \item{mischief_count}{Number of mischief offenses}
#'   \item{rate_per_100k}{Mischief rate per 100,000 population}
#'   \item{pop_proportion}{Proportion of total Canadian population}
#'   \item{mischief_proportion}{Proportion of total mischief offenses}
#' }
#'
#' @source Correll & Heer (2017). Surprise! Bayesian Weighting for De-Biasing
#'   Thematic Maps. IEEE InfoVis.
#'
#' @examples
#' data(canada_mischief)
#'
#' # Basic exploration
#' head(canada_mischief)
#'
#' # Compute surprise
#' result <- auto_surprise(
#'   observed = canada_mischief$mischief_count,
#'   expected = canada_mischief$population
#' )
#'
#' # See which provinces are most surprising under the selected models
#' canada_mischief$surprise <- result$surprise
#' canada_mischief$signed_surprise <- result$signed_surprise
#' canada_mischief[order(-abs(canada_mischief$signed_surprise)), ]
"canada_mischief"

#' Example County Data with Simulated Events
#'
#' A simulated dataset of 50 counties with population and event counts.
#' Some counties are designated as "hot spots" (higher than expected rates)
#' and "cold spots" (lower than expected rates) for testing and examples.
#'
#' @format A data frame with 50 rows and 7 variables:
#' \describe{
#'   \item{county_id}{Unique county identifier}
#'   \item{name}{County name}
#'   \item{population}{Population count}
#'   \item{events}{Number of events (e.g., crimes, incidents)}
#'   \item{expected}{Expected number of events based on population}
#'   \item{is_hotspot}{Logical; TRUE if county has elevated rates}
#'   \item{is_coldspot}{Logical; TRUE if county has suppressed rates}
#' }
#'
#' @examples
#' data(example_counties)
#'
#' # Compute surprise
#' result <- auto_surprise(
#'   observed = example_counties$events,
#'   expected = example_counties$population
#' )
#'
#' example_counties$surprise <- result$surprise
#'
#' # Hot spots and cold spots should have higher surprise
#' with(example_counties, tapply(surprise, is_hotspot, mean))
#' with(example_counties, tapply(surprise, is_coldspot, mean))
"example_counties"
