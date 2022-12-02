
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statsum

<!-- badges: start -->
<!-- badges: end -->

The goal of statsum is to produce summary statistics (numeric minimum,
first quartile (Q1), median, third quartile (Q3), maximum, range, and
interquartile range (IQR), and standard deviation) on the numeric
variable/column in the data

## Installation

You can install the development version of statsum like so:

``` r
# install.packages("devtools")
devtools::install_github("PaulLiPaulLi/statsum")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(statsum)
## basic example code
Population_Stat <- stat_sum(c(1000, 2000, 4500, 50000, 6000, 3500))
#>                        25%               75%                            
#>  1000.00 50000.00  2375.00  4000.00  5625.00  1000.00 50000.00  3250.00 
#>          
#> 19106.72
```
