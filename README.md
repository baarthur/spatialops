
# spatialops

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/baarthur/spatialops/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/baarthur/spatialops/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Spatialops: spatial operations that come in handy

Built mainly upon the `{sf}` and `{tidyverse}` frameworks, the functions in this package range from simple reading/tidying wrappers to specific spatial operations and random utilities I have used along my journey. Some use cases: 

* read shapefiles directly from a `.zip` whithout creating a mess in your folders with `shp_extract_read`
* calculate the distance between the closest `y` feature from an `x` set of features with `dist_nearest`
* need to manually tidy your shapefiles (e.g. nudge a geometry)? No problem, load them into [MapHub](https://maphub.net/) and then convert the description into useful columns using `maphub_to_sf`
* `gg`plot quantile regression coefficients with `coef_rqs`


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

