
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Overview

Grant is an R package designed to provide a comprehensive exploration of
grant opportunities in the United States. The package contains a dataset
with detailed information about various grants, including categories,
award ceilings, award floors, eligibility and application deadlines.

## Installation

You can install the development version of grant from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")

remotes::install_github("ETC5523-2024/assignment-4-packages-and-shiny-apps-sarah-liu17", subdir = "grant")
```

## Example

This is an example to explore the grants by category

``` r
summarize_grants_by_category(grant_opp)
#> # A tibble: 24 × 3
#>    category          number_of_grants total_funding
#>    <chr>                        <int>         <dbl>
#>  1 environment                    216   33925797553
#>  2 transportation                  15   14826348808
#>  3 science                        384   11336601836
#>  4 housing                         19    5712380367
#>  5 energy                          59    5360308000
#>  6 education                      453    5034515000
#>  7 employment                      15    3409030857
#>  8 health                        1065    3005438979
#>  9 natural_resources               78    2463959199
#> 10 other                           85    2321036324
#> # ℹ 14 more rows
```

## Shiny interactive app to explore the grants

``` r
run_grant_app()
```

There are different options of interactive features inside the app,
which allow the users to first investigate about grant opportunities
according to their expectations of eligibility, closing date and
category. Users could then develop a clear understanding of the award
range they are applying for to meet their capital needs by looking at
the categories in the bar chart and the table for specific grants. The
top funded grants are included at last to provide an overall picture of
what areas are most invested by the government to secure users’
fundings.
