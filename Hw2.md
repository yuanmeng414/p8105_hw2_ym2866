Hw2
================
Yuan Meng
2021-10-7

``` r
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readxl)
```

# Problem 1

``` r
trash_data = read_excel("/Users/yuanmeng/Desktop/Trash-Wheel-Collection-Totals-7-2020-2.xlsx", range = "A2:N535") #Read Excel file and omit non-data entries
trash_data
```

    ## # A tibble: 533 × 14
    ##    Dumpster Month       Year Date                `Weight (tons)` `Volume (cubic …
    ##    <chr>    <chr>      <dbl> <dttm>                        <dbl>            <dbl>
    ##  1 1        May         2014 2014-05-16 00:00:00            4.31               18
    ##  2 2        May         2014 2014-05-16 00:00:00            2.74               13
    ##  3 3        May         2014 2014-05-16 00:00:00            3.45               15
    ##  4 4        May         2014 2014-05-17 00:00:00            3.1                15
    ##  5 5        May         2014 2014-05-17 00:00:00            4.06               18
    ##  6 6        May         2014 2014-05-20 00:00:00            2.71               13
    ##  7 7        May         2014 2014-05-21 00:00:00            1.91                8
    ##  8 8        May         2014 2014-05-28 00:00:00            3.7                16
    ##  9 <NA>     May  Total    NA NA                            26.0               116
    ## 10 9        June        2014 2014-06-05 00:00:00            2.52               14
    ## # … with 523 more rows, and 8 more variables: Plastic Bottles <dbl>,
    ## #   Polystyrene <dbl>, Cigarette Butts <dbl>, Glass Bottles <dbl>,
    ## #   Grocery Bags <dbl>, Chip Bags <dbl>, Sports Balls <dbl>,
    ## #   Homes Powered* <dbl>

``` r
trash_data= janitor::clean_names(trash_data) 
names(trash_data) #use reasonable variable names
```

    ##  [1] "dumpster"           "month"              "year"              
    ##  [4] "date"               "weight_tons"        "volume_cubic_yards"
    ##  [7] "plastic_bottles"    "polystyrene"        "cigarette_butts"   
    ## [10] "glass_bottles"      "grocery_bags"       "chip_bags"         
    ## [13] "sports_balls"       "homes_powered"

``` r
drop_na(trash_data, dumpster) #omit rows that do not include dumpster-specific data
```

    ## # A tibble: 454 × 14
    ##    dumpster month  year date                weight_tons volume_cubic_yards
    ##    <chr>    <chr> <dbl> <dttm>                    <dbl>              <dbl>
    ##  1 1        May    2014 2014-05-16 00:00:00        4.31                 18
    ##  2 2        May    2014 2014-05-16 00:00:00        2.74                 13
    ##  3 3        May    2014 2014-05-16 00:00:00        3.45                 15
    ##  4 4        May    2014 2014-05-17 00:00:00        3.1                  15
    ##  5 5        May    2014 2014-05-17 00:00:00        4.06                 18
    ##  6 6        May    2014 2014-05-20 00:00:00        2.71                 13
    ##  7 7        May    2014 2014-05-21 00:00:00        1.91                  8
    ##  8 8        May    2014 2014-05-28 00:00:00        3.7                  16
    ##  9 9        June   2014 2014-06-05 00:00:00        2.52                 14
    ## 10 10       June   2014 2014-06-11 00:00:00        3.76                 18
    ## # … with 444 more rows, and 8 more variables: plastic_bottles <dbl>,
    ## #   polystyrene <dbl>, cigarette_butts <dbl>, glass_bottles <dbl>,
    ## #   grocery_bags <dbl>, chip_bags <dbl>, sports_balls <dbl>,
    ## #   homes_powered <dbl>

``` r
mutate(trash_data, sports_balls = round(sports_balls)) #round the number of sports balls to the nearest integer
```

    ## # A tibble: 533 × 14
    ##    dumpster month       year date                weight_tons volume_cubic_yards
    ##    <chr>    <chr>      <dbl> <dttm>                    <dbl>              <dbl>
    ##  1 1        May         2014 2014-05-16 00:00:00        4.31                 18
    ##  2 2        May         2014 2014-05-16 00:00:00        2.74                 13
    ##  3 3        May         2014 2014-05-16 00:00:00        3.45                 15
    ##  4 4        May         2014 2014-05-17 00:00:00        3.1                  15
    ##  5 5        May         2014 2014-05-17 00:00:00        4.06                 18
    ##  6 6        May         2014 2014-05-20 00:00:00        2.71                 13
    ##  7 7        May         2014 2014-05-21 00:00:00        1.91                  8
    ##  8 8        May         2014 2014-05-28 00:00:00        3.7                  16
    ##  9 <NA>     May  Total    NA NA                        26.0                 116
    ## 10 9        June        2014 2014-06-05 00:00:00        2.52                 14
    ## # … with 523 more rows, and 8 more variables: plastic_bottles <dbl>,
    ## #   polystyrene <dbl>, cigarette_butts <dbl>, glass_bottles <dbl>,
    ## #   grocery_bags <dbl>, chip_bags <dbl>, sports_balls <dbl>,
    ## #   homes_powered <dbl>

``` r
precipitation_19 = read_excel("/Users/yuanmeng/Desktop/Trash-Wheel-Collection-Totals-7-2020-2.xlsx", sheet = 6, range = "A2:B14") #omit rows without precipitation data
precipitation_18 = read_excel("/Users/yuanmeng/Desktop/Trash-Wheel-Collection-Totals-7-2020-2.xlsx", sheet = 7, range = "A2:B14") #omit rows without precipitation data
precipitation_19 = mutate(precipitation_19, year = "2019") #add a variable for year
precipitation_18 = mutate(precipitation_18, year = "2018") #add a variable for year
precipitation_19
```

    ## # A tibble: 12 × 3
    ##    Month Total year 
    ##    <dbl> <dbl> <chr>
    ##  1     1  3.1  2019 
    ##  2     2  3.64 2019 
    ##  3     3  4.47 2019 
    ##  4     4  1.46 2019 
    ##  5     5  3.58 2019 
    ##  6     6  0.42 2019 
    ##  7     7  3.85 2019 
    ##  8     8  2.39 2019 
    ##  9     9  0.16 2019 
    ## 10    10  5.45 2019 
    ## 11    11  1.86 2019 
    ## 12    12  3.57 2019

``` r
precipitation_18
```

    ## # A tibble: 12 × 3
    ##    Month Total year 
    ##    <dbl> <dbl> <chr>
    ##  1     1  0.94 2018 
    ##  2     2  4.8  2018 
    ##  3     3  2.69 2018 
    ##  4     4  4.69 2018 
    ##  5     5  9.27 2018 
    ##  6     6  4.77 2018 
    ##  7     7 10.2  2018 
    ##  8     8  6.45 2018 
    ##  9     9 10.5  2018 
    ## 10    10  2.12 2018 
    ## 11    11  7.82 2018 
    ## 12    12  6.11 2018

``` r
prec_data = merge(precipitation_19, precipitation_18, all.x = TRUE, all.y = TRUE) # combine precipitation datasets
prec_data
```

    ##    Month Total year
    ## 1      1  0.94 2018
    ## 2      1  3.10 2019
    ## 3      2  3.64 2019
    ## 4      2  4.80 2018
    ## 5      3  2.69 2018
    ## 6      3  4.47 2019
    ## 7      4  1.46 2019
    ## 8      4  4.69 2018
    ## 9      5  3.58 2019
    ## 10     5  9.27 2018
    ## 11     6  0.42 2019
    ## 12     6  4.77 2018
    ## 13     7  3.85 2019
    ## 14     7 10.20 2018
    ## 15     8  2.39 2019
    ## 16     8  6.45 2018
    ## 17     9  0.16 2019
    ## 18     9 10.47 2018
    ## 19    10  2.12 2018
    ## 20    10  5.45 2019
    ## 21    11  1.86 2019
    ## 22    11  7.82 2018
    ## 23    12  3.57 2019
    ## 24    12  6.11 2018

``` r
as.character(month.name) #convert month to a character variable
```

    ##  [1] "January"   "February"  "March"     "April"     "May"       "June"     
    ##  [7] "July"      "August"    "September" "October"   "November"  "December"

The number of observation in Trash Wheel Collection Totals data is on
453 dumpster.The data include 13 key variables, they are “month”,
“year”, “date”, “weight\_tons”, “volume\_cubic\_yards”,
“plastic\_bottles”, “polystyrene”, “cigarette\_butts”, “glass\_bottles”,
“grocery\_bags”, “chip\_bags”, “sports\_balls”, “homes\_powered”.

The number of total observation in combine 2019 and 2018 precipitation
datasets is 24. The data include 3 variables. They are month total and
year. The total precipitation in 2018 is 70.33 and total precipitation
in 2019 is 33.95. The median number of sports balls in a dumpster in
2019 is 3.335.

# Problem 2
