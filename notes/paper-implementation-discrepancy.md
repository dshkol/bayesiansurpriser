# Paper vs Implementation Discrepancy in Correll & Heer (2017)

## Summary

The Correll & Heer (2017) "Surprise Maps" paper describes mathematically correct
Bayesian surprise computation with normalized posteriors, but the JavaScript
implementation uses unnormalized posteriors for per-region surprise values.

## Paper States (Correct)

Line 260 of template.tex:
> "As P(M) is a belief about the likelihood of a model, and our assumption is
> that M represents our universe of plausible models, we normalize such that
> Σ Mi = 1"

Line 201:
> P(M|D) ∝ P(D|M) P(M)

The paper correctly describes normalized posteriors for KL divergence.

## JavaScript Implementation (Deviates)

The code has TWO different surprise functions:

### `surprise()` (lines 110-137) - Global updates
- CORRECTLY normalizes posteriors before KL:
```javascript
var total = dl.sum(pmds);
pmds[i] /= total;  // Normalized
```

### `surpriseMap()` (lines 71-108) - Per-region values
- Does NOT normalize posteriors:
```javascript
pmds.push(models[k].pm * pdms[k]);  // Unnormalized
// Commented out: pmds.push(models[k].pm*(pdms[k]/dl.sum(pdms)));
kl += pmds[k] * (log(pmds[k] / models[k].pm) / log(2));
```

Note: Line 96 has a COMMENTED-OUT normalized version, suggesting this was
a deliberate choice, though undocumented.

## Implications

1. The per-region surprise values in the paper's figures and demos use
   unnormalized posteriors
2. This differs from both:
   - The paper's stated methodology
   - The original Itti & Baldi (2005) Bayesian surprise formulation
3. No external literature discusses this discrepancy

## R Package Approach

The `bayesiansurpriser` R package keeps both behaviors separate:

- `normalize_posterior = TRUE` (default): Uses proper Bayesian posterior
  normalization before computing KL divergence. This is the method-valid
  Bayesian Surprise calculation.
- `normalize_posterior = FALSE` (legacy comparison): Replicates the JavaScript
  `surpriseMap()` behavior with unnormalized posterior weights. This is useful
  for auditing historical outputs, but it is not a KL divergence between two
  probability distributions.

## Validation

With `normalize_posterior = FALSE`, the R package achieves:
- Correlation = 1.0 with paper's unemployment.csv pre-computed values
- Max difference ~5e-8 (floating point precision)

## References

- Paper: https://idl.uw.edu/papers/surprise-maps
- Code: https://github.com/uwdata/bayesian-surprise
- Original Bayesian Surprise: Itti & Baldi (2005) NIPS
