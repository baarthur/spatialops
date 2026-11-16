
# spatialops

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/baarthur/spatialops/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/baarthur/spatialops/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Spatialops: spatial operations that come in handy

Built mainly upon the `{sf}` and `{tidyverse}` frameworks, the functions in this package focus on reusable spatial operations. Some use cases:

* calculate the distance between the closest `y` feature from an `x` set of features with `dist_nearest`


The MapHub-specific helpers `maphub_to_sf()`, `maphub_tidy_lines()`, and `maphub_tidy_stations()` remain available while their long-term home is decided. `maphub_to_sf()` is used by the published *Rapid Transit, Labor Market, and Business Growth* data workflow.
## Installation

You can install the development version of spatialops with `remotes`:

``` r
remotes::install_github("https://github.com/baarthur/spatialops")
```

## Example

Find the distance to the closest subway station in each neighborhood in Fortaleza (Brazil). Very useful in hedonic models and other (spatial) econometric analysis.

``` r
library(spatialops)
data("fortaleza")
data("metrofor")

# select only stations
metrofor <- dplyr::filter(metrofor, railway == "station")

fortaleza <- fortaleza %>% 
  dist_nearest(metrofor, name = dist_metro)
```

