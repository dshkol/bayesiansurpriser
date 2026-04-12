# bayesiansurpriser

Bayesian Surprise for De-Biasing Thematic Maps in R

## Overview

`bayesiansurpriser` implements Bayesian Surprise calculations for thematic maps, inspired by Correll & Heer's "Surprise! Bayesian Weighting for De-Biasing Thematic Maps" (IEEE InfoVis 2016). The default calculation normalizes posterior model probabilities and measures how much each observation updates beliefs about a specified model space.

The package provides seamless integration with:
- **sf**: Simple Features for spatial data
- **ggplot2**: Grammar of graphics for visualization
- Temporal/streaming data analysis

## Installation

```r
# Install from GitHub (development version)
# install.packages("devtools")
devtools::install_github("dshkol/bayesiansurpriser")
```
## Quick Start

```r
library(bayesiansurpriser)
library(sf)
library(ggplot2)

# Load sample spatial data
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

# Compute Bayesian surprise
result <- surprise(nc, observed = SID74, expected = BIR74)

# View results
print(result)

# Plot with ggplot2
ggplot(result) +
  geom_sf(aes(fill = signed_surprise)) +
  scale_fill_surprise_diverging() +
  labs(title = "Bayesian Surprise: NC SIDS Data") +
  theme_minimal()
```

## Key Features

### Five Model Types

1. **Uniform Model** (`bs_model_uniform()`): Assumes equiprobable events
2. **Base Rate Model** (`bs_model_baserate()`): Compares to expected rates (e.g., population)
3. **Gaussian Model** (`bs_model_gaussian()`): Parametric model for outlier detection
4. **Sampled Model** (`bs_model_sampled()`): Non-parametric KDE model
5. **de Moivre Funnel** (`bs_model_funnel()`): Accounts for sampling variation

### sf Integration

```r
# Works directly with sf objects
result <- st_surprise(nc, observed = SID74, expected = BIR74)
plot(result)
```

### ggplot2 Integration

```r
# Custom geom and scales
ggplot(nc) +
  geom_surprise(aes(observed = SID74, expected = BIR74)) +
  scale_fill_surprise()

# Signed surprise with diverging colors
ggplot(nc) +
  geom_surprise(aes(observed = SID74, expected = BIR74), fill_type = "signed") +
  scale_fill_surprise_diverging()
```

### Temporal Analysis

```r
# Compute surprise over time
result <- surprise_temporal(data,
  time_col = year,
  observed = events,
  expected = population
)

# Update with streaming data
result <- update_surprise(result, new_data)
```

## The Problem: Three Biases

Traditional thematic maps suffer from three key biases:

1. **Base Rate Bias**: Visual prominence dominated by population density
2. **Sampling Error Bias**: Sparse regions show misleadingly high variability
3. **Renormalization Bias**: Dynamic scaling suppresses important patterns

Bayesian Surprise can help address these biases by comparing observations against explicit models, such as population base rates and sampling-variation models.

## How It Works

The default method uses KL-divergence to measure "surprise":

```
Surprise = KL(P(M|D) || P(M))
         = Σ P(M_i|D) * log(P(M_i|D) / P(M_i))
```

Where:
- `P(M)` = Prior probability of model M
- `P(M|D)` = Posterior probability after observing data D
- High surprise = data significantly updates our beliefs

The original JavaScript demo associated with the paper used an unnormalized
per-region score for some map outputs. This package keeps that behavior only as
an explicit legacy comparison option (`normalize_posterior = FALSE`); new
analyses should use the normalized default.

## References

Correll, M., & Heer, J. (2017). Surprise! Bayesian Weighting for De-Biasing Thematic Maps. *IEEE Transactions on Visualization and Computer Graphics*, 23(1), 651-660. https://doi.org/10.1109/TVCG.2016.2598839

## License

MIT
