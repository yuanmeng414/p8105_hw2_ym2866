Hw2
================
Yuan Meng
2021-10-7

# Problem 1

``` r
library(tidyverse)
library(readxl)
read_excel("/Users/yuanmeng/Desktop/Trash-Wheel-Collection-Totals-8-6-19.xlsx")
```

    ## # A tibble: 406 × 17
    ##    Dumpster Month       Year Date                `Weight (tons)` `Volume (cubic …
    ##       <dbl> <chr>      <dbl> <dttm>                        <dbl>            <dbl>
    ##  1        1 May         2014 2014-05-16 00:00:00            4.31               18
    ##  2        2 May         2014 2014-05-16 00:00:00            2.74               13
    ##  3        3 May         2014 2014-05-16 00:00:00            3.45               15
    ##  4        4 May         2014 2014-05-17 00:00:00            3.1                15
    ##  5        5 May         2014 2014-05-17 00:00:00            4.06               18
    ##  6        6 May         2014 2014-05-20 00:00:00            2.71               13
    ##  7        7 May         2014 2014-05-21 00:00:00            1.91                8
    ##  8        8 May         2014 2014-05-28 00:00:00            3.7                16
    ##  9       NA May  Total    NA NA                            26.0               116
    ## 10        9 June        2014 2014-06-05 00:00:00            2.52               14
    ## # … with 396 more rows, and 11 more variables: Plastic Bottles <dbl>,
    ## #   Polystyrene <dbl>, Cigarette Butts <dbl>, Glass Bottles <dbl>,
    ## #   Grocery Bags <dbl>, Chip Bags <dbl>, Sports Balls <dbl>,
    ## #   Homes Powered* <dbl>, ...15 <chr>, ...16 <lgl>, ...17 <lgl>
