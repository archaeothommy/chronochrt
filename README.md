
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ChronochRt

<!-- badges: start -->

[![pipeline
status](https://gitlab.com/roset/chronochrt/badges/master/pipeline.svg)](https://gitlab.com/roset/chronochrt/-/commits/master)
[![coverage
report](https://gitlab.com/roset/chronochrt/badges/master/coverage.svg)](https://gitlab.com/roset/chronochrt/-/commits/master)
<!-- badges: end -->

ChronochRt offers an easy way to draw chronological charts from tables.
It aims to provide an environment suitable for persons new to R.

## Installation

ChronochRt is currently in the state of beta testing. You can install it
from gitlab with:

``` r
 devtools::install_gitlab("roset/chronochrt")
```

Please help us to improve ChronochRt by please file an issue
[here](https://gitlab.com/roset/chronochrt/issues).

## Features

  - Slim and intuitive chronological data set
  - Import tabular data files and
  - Import Excel files (requires the package
    [readxl](https://readxl.tidyverse.org/))
  - Possibility to display up to 2 chronological systems within the same
    region (e.g. long and short chronologies)
  - Layout of the chronological chart optimised for easy readability and
    comprehensibility
  - Years in BCE must be negative - that’s all you need to care about
    dates
  - Handling of unsecured dates
  - Handling of gaps, e.g. abandonment phases of sites
  - Optional text labels
  - Export of the chronological chart in various file formats (raster
    and vector graphics)
  - Easy customisation of the chronological chart
  - Based on the [tidyverse](https://www.tidyverse.org/): Seamless
    integration in pipes, enhanced customsation with
    [ggplot2](https://ggplot2.tidyverse.org/)

In development for the CRAN release:

  - geom\_chronochRt for full flexibility within ggplot2
  - Image labels to e.g. display key finds or show typological
    developments

Is there a feature missing? Please let us know
[here](https://gitlab.com/roset/chronochrt/issues).

## Getting started

  - Cheatsheet
  - [Vignettes](https://gitlab.com/roset/chronochrt/-/tree/master/vignettes)

## Funding

<div style="width:150px">

![EU
flag](https://europa.eu/european-union/sites/europaeu/files/docs/body/flag_yellow_low.jpg)

</div>

This project has received funding from the European Union’s Horizon 2020
research and innovation programme under the Marie Skłodowska-Curie grant
agreement No 766311.
