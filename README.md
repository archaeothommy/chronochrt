
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ChronochRt

<!-- badges: start -->

[![R-CMD-check](https://github.com/archaeothommy/chronochrt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/archaeothommy/chronochrt/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/archaeothommy/chronochrt/graph/badge.svg)](https://app.codecov.io/gh/archaeothommy/chronochrt)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/chronochrt)](https://cran.r-project.org/package=chronochrt)
[![CRAN_downloads](https://cranlogs.r-pkg.org/badges/grand-total/chronochrt)](https://cran.r-project.org/package=chronochrt)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5154018.svg)](https://doi.org/10.5281/zenodo.5154018)
<!-- badges: end -->

ChronochRt offers an easy way to draw chronological charts from tables.
It aims to provide an intuitive environment for anyone new to R and
includes [ggplot2](https://ggplot2.tidyverse.org/) geoms and theme for
chronological charts.

## Installation

You can install the released version of ChronochRt from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("chronochrt")
```

or install the development version with the newest bugfixes and features
from gitlab with:

``` r
 devtools::install_github("archaeothommy/chronochrt")
```

You are very welcome to contribute to the package through forking the
repository or creating an issue to suggest changes.

## Features

- Slim structure of chronological datasets
- Import tabular data files
- Import Excel files (requires the package
  [readxl](https://readxl.tidyverse.org/))
- Possibility to display up to 2 chronological systems within the same
  region (e.g. long and short chronologies)
- Layout of the chronological chart optimised for easy readability and
  comprehensibility
- Years in BCE must be negative - that’s all you need to care about
  dates
- Handling of insecure dates
- Handling of gaps, e.g. abandonment phases of sites
- Optional text labels
- Optional image labels to e.g. display key finds or show typological
  developments
- Geoms for the chronological chart and image labels
- Export of the chronological chart in different file formats (raster
  and vector graphics)
- Easy customisation of the chronological chart
- Based on the [tidyverse](https://www.tidyverse.org/): Seamless
  integration in pipes, enhanced customisation with
  [ggplot2](https://ggplot2.tidyverse.org/)

Is there a feature missing? Please let us know [by creating an
issue](https://github.com/archaeothommy/chronochrt/issues/new/choose).

## Example

``` r
library(ChronochRt)

data <- add_chron(region = "A",
                  name = c("A", "A1", "A2", "B"),
                  start = c(-200, -200, 0, -100),
                  end = c(200, 0, 200, 100),
                  level = c(1,2,2,1),
                  add = FALSE,
                  new_table = TRUE)

plot_chronochrt(data)
```

## Getting started

- [Cheatsheet](https://github.com/archaeothommy/chronochrt/blob/master/inst/ChronochRt_Cheatsheet.pdf)
- [Vignettes](https://github.com/archaeothommy/chronochrt/tree/master/vignettes)

## Funding for the development of ChronochRt 0.0.5

<table width="100%" cellspacing="0" cellpadding="0" border="0">
<tbody>
<tr>
<td valign="bottom">
<img src="https://upload.wikimedia.org/wikipedia/commons/b/b7/Flag_of_Europe.svg"  width="200">
</td>
<td valign="bottom" halign="left">
This project has received funding from the European Union’s Horizon 2020
research and innovation programme under the Marie Skłodowska-Curie grant
agreement No 766311.
</td>
</tr>
</tbody>
</table>
