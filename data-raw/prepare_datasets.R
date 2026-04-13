# Script to prepare example datasets for bayesiansurpriser package
#
# Data sources:
# - Canada mischief data: from bayesian-surprise paper
# - US unemployment data: from bayesian-surprise paper

# Canada Mischief Data ---------------------------------------------------------
# Crime data by Canadian province/territory

canada_mischief <- data.frame(
  name = c("Alberta", "British Columbia", "Manitoba", "New Brunswick",
           "Newfoundland & Labrador", "Northwest Territories", "Nova Scotia",
           "Nunavut", "Ontario", "Prince Edward Island", "Quebec",
           "Saskatchewan", "Yukon Territory"),
  population = c(3645257, 4400057, 1208268, 751171, 514536, 41462, 921727,
                 31906, 12851821, 140204, 7903001, 1033381, 33897),
  mischief_count = c(47829, 45991, 25393, 6925, 8041, 7981, 10783,
                     4048, 64116, 1750, 39307, 28716, 1583),
  stringsAsFactors = FALSE
)

# Compute derived values
canada_mischief$rate_per_100k <- canada_mischief$mischief_count /
  canada_mischief$population * 100000
canada_mischief$pop_proportion <- canada_mischief$population /
  sum(canada_mischief$population)
canada_mischief$mischief_proportion <- canada_mischief$mischief_count /
  sum(canada_mischief$mischief_count)

# Save
usethis::use_data(canada_mischief, overwrite = TRUE)


# Simulated County Data --------------------------------------------------------
# A simulated dataset for examples and testing

set.seed(42)
n_counties <- 50

example_counties <- data.frame(
  county_id = 1:n_counties,
  name = paste("County", 1:n_counties),
  population = round(exp(rnorm(n_counties, log(50000), 0.8))),
  stringsAsFactors = FALSE
)

# Generate events with some interesting patterns:
# - Base rate proportional to population
# - Some counties are "hot spots"
# - Some counties are "cold spots"
base_rate <- 0.005  # 5 per 1000 population
hot_spots <- c(5, 15, 25)  # Triple the rate
cold_spots <- c(10, 20, 30)  # Half the rate

expected_events <- example_counties$population * base_rate
multiplier <- rep(1, n_counties)
multiplier[hot_spots] <- 3
multiplier[cold_spots] <- 0.5

example_counties$events <- rpois(n_counties, lambda = expected_events * multiplier)
example_counties$expected <- expected_events
example_counties$is_hotspot <- 1:n_counties %in% hot_spots
example_counties$is_coldspot <- 1:n_counties %in% cold_spots

# Save
usethis::use_data(example_counties, overwrite = TRUE)


# Document datasets ------------------------------------------------------------
# Run after creating datasets:
# devtools::document()
