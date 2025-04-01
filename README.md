
<!-- README.md is generated from README.Rmd. Please edit that file -->

# glassdoor

[![JHU Data
Science](https://johnmuschelli.com/jhudsl/inst/jhudsl_logo.png)](https://jhudatascience.org/)
<!-- badges: start -->
[![R-CMD-check](https://github.com/muschellij2/glassdoor/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/muschellij2/glassdoor/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](http://www.r-pkg.org/badges/version/glassdoor)](https://cran.r-project.org/package=glassdoor)  
<!-- badges: end -->

<br> <!-- ![Sticker](sticker.png) -->
<img src="man/figures/sticker.jpg" width="100">

The goal of glassdoor is to provide a Glassdoor API for R

## Installation

You can install glassdoor from github with:

``` r
# install.packages("devtools")
devtools::install_github("muschellij2/glassdoor")
```

## Glassdoor API Keys

In order to use the `glassdoor` package, you need an API key from
Glassdoor (either
<https://www.glassdoor.com/developer/register_input.htm> or click Get
API key from <https://www.glassdoor.com/developer/index.htm>).

After that is done, you need to set up the API keys. The keys are
grabbed using

``` r
Sys.getenv("GLASSDOOR_PID")
Sys.getenv("GLASSDOOR_PAT")
```

so you can set these either using `~/.Renviron` (will work generally) or
your standard `.profile` or `.bash_profile` (if you work with command
lines).

I recommend `.Renviron` as that works well with RStudio.
