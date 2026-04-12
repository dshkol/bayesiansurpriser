# Package Hooks
#
# .onLoad and .onAttach hooks for bayesiansurpriser package.

.onLoad <- function(libname, pkgname) {
  # Register S3 methods for sf if available
  if (requireNamespace("sf", quietly = TRUE)) {
    # Methods are already registered via S3method() in NAMESPACE
  }

  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "bayesiansurpriser: Bayesian Surprise for De-Biasing Thematic Maps\n",
    "Inspired by Correll & Heer (2017) - IEEE InfoVis"
  )
}
