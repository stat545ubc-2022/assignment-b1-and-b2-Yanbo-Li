Assignment_b1_b2
================
Paul Li
2022-11-08

## Setup

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(datateachr)
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

## Exercise 1 & 2: Make a Function and Document Your Function

``` r
#' Title Summary Statistics for factors of interest grouped by other factors of interest
#' Description Produce summary statistics on the factor of interest to be summarized on each group defined and specified by the grouping factor of interest
#' @param data is the dataset in use
#' @param group_factor is the variable/column to be grouped of interest in the data
#' @param summarize_factor is the variable/column to be summarized with different summary statistics
#'
#' @return the numeric minimum, first quartile (Q1), median, third quartile (Q3), maximum, range, and interquartile range (IQR), and standard deviation of the factor of interest to be summarized for all groups defined by the grouping factor of interest
#' @export
#'
#' @examples
#' neighbourhood_diameter_summary <- stat_summary(vancouver_trees, "neighbourhood_name", "diameter")
stat_summary <- function(data, group_factor, summarize_factor) {
  if(!is.character(data[[group_factor]]) || is.factor(data[[group_factor]])) {
    stop("I am so sorry, but this function only works for character or factor input for the factor to be grouped 
         \n", 
         "You have provided the group_factor object of class: ", class(data[[group_factor]])[1] 
         )
  }
  if (!is.numeric(data[[summarize_factor]])){
    stop("I am so sorry, but this function only works for numeric input for the factor to be summarized
         \n", 
         "You have provided the summarize_factor object of class: ", class(data[[summarize_factor]])[1]
         )
  }
  data %>% 
    group_by(.data[[group_factor]], na.rm=T) %>% 
    summarize(Mean = mean(.data[[summarize_factor]], na.rm=T),
              Minimum = min(.data[[summarize_factor]], na.rm=T),
              Q1 = quantile(.data[[summarize_factor]], 0.25, na.rm=T),
              Median = median(.data[[summarize_factor]], na.rm=T),
              Q3 = quantile(.data[[summarize_factor]], 0.75, na.rm=T),
              Maximum = max(.data[[summarize_factor]], na.rm=T),
              Range = range(.data[[summarize_factor]], na.rm=T),
              IQR = IQR(.data[[summarize_factor]], na.rm=T),
              Standard_deviation = sd(.data[[summarize_factor]], na.rm = T))}
```

## Exercise 3: Example

Working example producing the summary statistics for the `diameter` for
all groups in `neighbourhood_name` (aka all neighbourhoods)

``` r
neighbourhood_diameter_summary <- stat_summary(vancouver_trees, "neighbourhood_name", "diameter")
```

    ## `summarise()` has grouped output by 'neighbourhood_name', 'na.rm'. You can
    ## override using the `.groups` argument.

Working example producing the summary statistics for the `diameter` in
different `street_side_name`

``` r
street_side_diameter_summary <- stat_summary(vancouver_trees, "street_side_name", "diameter")
```

    ## `summarise()` has grouped output by 'street_side_name', 'na.rm'. You can
    ## override using the `.groups` argument.

Failed example because of wrong input data type (input data type for
`group_factor` placeholder is not character or factor;
`summarize_factor` is still numeric or double)

``` r
## replace character or factor with date here
stat_summary(vancouver_trees, "date_planted", "diameter")
```

    ## Error in stat_summary(vancouver_trees, "date_planted", "diameter"): I am so sorry, but this function only works for character or factor input for the factor to be grouped 
    ##          
    ## You have provided the group_factor object of class: Date

``` r
## replace character or factor with numeric (double) here
stat_summary(vancouver_trees, "height_range_id", "diameter")
```

    ## Error in stat_summary(vancouver_trees, "height_range_id", "diameter"): I am so sorry, but this function only works for character or factor input for the factor to be grouped 
    ##          
    ## You have provided the group_factor object of class: numeric

Failed example because of wrong input data type (input data type for
`summarize_factor` placeholder is not numeric; input data type for
`group_factor` placeholder is still character or factor)

``` r
## replace numeric with character here
stat_summary(vancouver_trees, "curb", "neighbourhood_name")
```

    ## Error in stat_summary(vancouver_trees, "curb", "neighbourhood_name"): I am so sorry, but this function only works for numeric input for the factor to be summarized
    ##          
    ## You have provided the summarize_factor object of class: character

``` r
## replace numeric with date here
stat_summary(vancouver_trees, "street_side_name", "date_planted")
```

    ## Error in stat_summary(vancouver_trees, "street_side_name", "date_planted"): I am so sorry, but this function only works for numeric input for the factor to be summarized
    ##          
    ## You have provided the summarize_factor object of class: Date

Failed example with empty input specified for second placeholder (aka
`group_factor`)

``` r
stat_summary(vancouver_trees, "", "diameter")
```

    ## Error in stat_summary(vancouver_trees, "", "diameter"): I am so sorry, but this function only works for character or factor input for the factor to be grouped 
    ##          
    ## You have provided the group_factor object of class: NULL

Failed example with empty input specified for third placeholder (aka
`summarize_factor`)

``` r
stat_summary(vancouver_trees, "curb", "")
```

    ## Error in stat_summary(vancouver_trees, "curb", ""): I am so sorry, but this function only works for numeric input for the factor to be summarized
    ##          
    ## You have provided the summarize_factor object of class: NULL

Failed example with wrong input column name specified

``` r
stat_summary(vancouver_trees, "curbs", "diameter")
```

    ## Error in stat_summary(vancouver_trees, "curbs", "diameter"): I am so sorry, but this function only works for character or factor input for the factor to be grouped 
    ##          
    ## You have provided the group_factor object of class: NULL

Failed example without data specified (no data)

``` r
stat_summary("curb", "diameter")
```

    ## Error in data[[group_factor]]: subscript out of bounds

## Exercise 4: Function Tests

``` r
test_that("stat_summary tests", {
  expect_error(stat_summary(vancouver_trees, "", "diameter"))
  expect_error(stat_summary(vancouver_trees, "curb", ""))
  expect_error(stat_summary(vancouver_trees, "curb", "street_side_name"))
  expect_error(stat_summary(vancouver_trees, "diameter", "diameter"))
  expect_error(stat_summary("diameter", "diameter"))
  expect_error(stat_summary(vancouver_trees, "curbs", "diameter"))
})
```

    ## Test passed 🎊
