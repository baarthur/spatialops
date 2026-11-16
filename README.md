
# spatialops

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/baarthur/spatialops/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/baarthur/spatialops/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Spatialops: spatial operations that come in handy

Built mainly upon the `{sf}` and `{tidyverse}` frameworks, the functions in this package focus on reusable spatial operations. Some use cases:

* read shapefiles directly from a `.zip` whithout creating a mess in your folders with `shp_extract_read`
* calculate the distance between the closest `y` feature from an `x` set of features with `dist_nearest`


The MapHub-specific helpers were removed from the current API. The Mendeley Data deposit *Rapid Transit, Labor Market, and Business Growth* uses `maphub_to_sf()`; for that call, install the pre-removal revision with `remotes::install_github("baarthur/spatialops@5d025ddd5255606eeb7d2f25a0fc7af4e8c01ce0")`. The MapHub helpers are planned for a separate personal package.

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

