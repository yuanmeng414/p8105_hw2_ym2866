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
trash_data = read_excel("/Users/yuanmeng/Desktop/Fall 2021/P8105 Data Science/DS_Hw/p8105_hw2_ym2866/Trash-Wheel-Collection-Totals-7-2020-2.xlsx", range = "A2:N535") #Read Excel file and omit non-data entries
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
precipitation_19 = read_excel("/Users/yuanmeng/Desktop/Fall 2021/P8105 Data Science/DS_Hw/p8105_hw2_ym2866/Trash-Wheel-Collection-Totals-7-2020-2.xlsx", sheet = 6, range = "A2:B14") #omit rows without precipitation data
precipitation_18 = read_excel("/Users/yuanmeng/Desktop/Fall 2021/P8105 Data Science/DS_Hw/p8105_hw2_ym2866/Trash-Wheel-Collection-Totals-7-2020-2.xlsx", sheet = 7, range = "A2:B14") #omit rows without precipitation data
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
prec_data = bind_rows(precipitation_19, precipitation_18) # combine precipitation datasets
prec_data
```

    ## # A tibble: 24 × 3
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
    ## # … with 14 more rows

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

``` r
pols_month = read_csv("./fivethirtyeight_datasets/pols-month.csv")
pols_month = separate(pols_month,1,c("year","month","day"),sep="-") # break up the variable into integer variables
pols_month
```

    ## # A tibble: 822 × 11
    ##    year  month day   prez_gop gov_gop sen_gop rep_gop prez_dem gov_dem sen_dem
    ##    <chr> <chr> <chr>    <dbl>   <dbl>   <dbl>   <dbl>    <dbl>   <dbl>   <dbl>
    ##  1 1947  01    15           0      23      51     253        1      23      45
    ##  2 1947  02    15           0      23      51     253        1      23      45
    ##  3 1947  03    15           0      23      51     253        1      23      45
    ##  4 1947  04    15           0      23      51     253        1      23      45
    ##  5 1947  05    15           0      23      51     253        1      23      45
    ##  6 1947  06    15           0      23      51     253        1      23      45
    ##  7 1947  07    15           0      23      51     253        1      23      45
    ##  8 1947  08    15           0      23      51     253        1      23      45
    ##  9 1947  09    15           0      23      51     253        1      23      45
    ## 10 1947  10    15           0      23      51     253        1      23      45
    ## # … with 812 more rows, and 1 more variable: rep_dem <dbl>

``` r
pols_month %>% 
  mutate(month = month.name[as.numeric(month)]) #replace month number with month name
```

    ## # A tibble: 822 × 11
    ##    year  month     day   prez_gop gov_gop sen_gop rep_gop prez_dem gov_dem sen_dem
    ##    <chr> <chr>     <chr>    <dbl>   <dbl>   <dbl>   <dbl>    <dbl>   <dbl>   <dbl>
    ##  1 1947  January   15           0      23      51     253        1      23      45
    ##  2 1947  February  15           0      23      51     253        1      23      45
    ##  3 1947  March     15           0      23      51     253        1      23      45
    ##  4 1947  April     15           0      23      51     253        1      23      45
    ##  5 1947  May       15           0      23      51     253        1      23      45
    ##  6 1947  June      15           0      23      51     253        1      23      45
    ##  7 1947  July      15           0      23      51     253        1      23      45
    ##  8 1947  August    15           0      23      51     253        1      23      45
    ##  9 1947  September 15           0      23      51     253        1      23      45
    ## 10 1947  October   15           0      23      51     253        1      23      45
    ## # … with 812 more rows, and 1 more variable: rep_dem <dbl>

``` r
pols_month = mutate(pols_month, president = ifelse(prez_gop == 1, "gop","dem"))
pols_month = select(pols_month, -c(prez_dem, prez_gop, day)) #remove prez_dem and prez_gop; and remove the day variable.
```

``` r
snp_data = read_csv("./fivethirtyeight_datasets/snp.csv")
```

    ## Rows: 787 Columns: 2

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
snp_data = separate(snp_data,1,c("month","day","year"),sep="/") #break up the variable into integer variables year, month, and day;
snp_data = snp_data %>% 
  mutate(month = month.name[as.numeric(month)]) #replace month number with month name
snp_data = snp_data [, c("year", "month", "day","close")] #organize let year and month are the leading columns.
snp_data
```

    ## # A tibble: 787 × 4
    ##    year  month    day   close
    ##    <chr> <chr>    <chr> <dbl>
    ##  1 15    July     1     2080.
    ##  2 15    June     1     2063.
    ##  3 15    May      1     2107.
    ##  4 15    April    1     2086.
    ##  5 15    March    2     2068.
    ##  6 15    February 2     2104.
    ##  7 15    January  2     1995.
    ##  8 14    December 1     2059.
    ##  9 14    November 3     2068.
    ## 10 14    October  1     2018.
    ## # … with 777 more rows

``` r
unemployment_data = read_csv("./fivethirtyeight_datasets/unemployment.csv")
unemployment_data = unemployment_data %>%
  pivot_longer(!Year, names_to = "Month", values_to = "unemployment_rate")#switching from “wide” to “long” format
unemployment_data
```

    ## # A tibble: 816 × 3
    ##     Year Month unemployment_rate
    ##    <dbl> <chr>             <dbl>
    ##  1  1948 Jan                 3.4
    ##  2  1948 Feb                 3.8
    ##  3  1948 Mar                 4  
    ##  4  1948 Apr                 3.9
    ##  5  1948 May                 3.5
    ##  6  1948 Jun                 3.6
    ##  7  1948 Jul                 3.6
    ##  8  1948 Aug                 3.9
    ##  9  1948 Sep                 3.8
    ## 10  1948 Oct                 3.7
    ## # … with 806 more rows

``` r
join_pols_snp = full_join(pols_month,snp_data)#merging snp into pols
```

    ## Joining, by = c("year", "month")

``` r
join_unemployment = bind_rows(join_pols_snp,unemployment_data) #merging unemployment into the result
join_pols_snp
```

    ## # A tibble: 1,609 × 11
    ##    year  month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president day  
    ##    <chr> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>     <chr>
    ##  1 1947  01         23      51     253      23      45     198 dem       <NA> 
    ##  2 1947  02         23      51     253      23      45     198 dem       <NA> 
    ##  3 1947  03         23      51     253      23      45     198 dem       <NA> 
    ##  4 1947  04         23      51     253      23      45     198 dem       <NA> 
    ##  5 1947  05         23      51     253      23      45     198 dem       <NA> 
    ##  6 1947  06         23      51     253      23      45     198 dem       <NA> 
    ##  7 1947  07         23      51     253      23      45     198 dem       <NA> 
    ##  8 1947  08         23      51     253      23      45     198 dem       <NA> 
    ##  9 1947  09         23      51     253      23      45     198 dem       <NA> 
    ## 10 1947  10         23      51     253      23      45     198 dem       <NA> 
    ## # … with 1,599 more rows, and 1 more variable: close <dbl>

``` r
join_unemployment
```

    ## # A tibble: 2,425 × 14
    ##    year  month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president day  
    ##    <chr> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>     <chr>
    ##  1 1947  01         23      51     253      23      45     198 dem       <NA> 
    ##  2 1947  02         23      51     253      23      45     198 dem       <NA> 
    ##  3 1947  03         23      51     253      23      45     198 dem       <NA> 
    ##  4 1947  04         23      51     253      23      45     198 dem       <NA> 
    ##  5 1947  05         23      51     253      23      45     198 dem       <NA> 
    ##  6 1947  06         23      51     253      23      45     198 dem       <NA> 
    ##  7 1947  07         23      51     253      23      45     198 dem       <NA> 
    ##  8 1947  08         23      51     253      23      45     198 dem       <NA> 
    ##  9 1947  09         23      51     253      23      45     198 dem       <NA> 
    ## 10 1947  10         23      51     253      23      45     198 dem       <NA> 
    ## # … with 2,415 more rows, and 4 more variables: close <dbl>, Year <dbl>,
    ## #   Month <chr>, unemployment_rate <dbl>

The pols\_month dataset have 11 variables and 1609 observations.The key
variables are year,
month,president,gov\_gop,sen\_gop,rep\_gop,gov\_dem,sen\_dem,rep\_dem.
The snp dataset have 4 variables and 787 observations. The key variables
are year,month,day,close. The unemployment dataset have 3 variables and
816 observations. The key variables are year month and unemployment
rate. The dataset merge snp, pols and unemployment have 14 variables and
533 observation. The key variables are variables from pols\_month,
snp\_data and unemployment\_data.

\#Problem3
