HW02
================
Yuxin Huang
2/26/2020

``` r
library(blscrapeR)
```

    ## Warning: package 'blscrapeR' was built under R version 3.5.2

``` r
library(data.table)
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.2

    ## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
    ## ✓ tibble  2.1.3     ✓ dplyr   0.8.4
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## Warning: package 'ggplot2' was built under R version 3.5.2

    ## Warning: package 'tibble' was built under R version 3.5.2

    ## Warning: package 'tidyr' was built under R version 3.5.2

    ## Warning: package 'purrr' was built under R version 3.5.2

    ## Warning: package 'dplyr' was built under R version 3.5.2

    ## Warning: package 'stringr' was built under R version 3.5.2

    ## Warning: package 'forcats' was built under R version 3.5.2

    ## ── Conflicts ────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::between()   masks data.table::between()
    ## x dplyr::filter()    masks stats::filter()
    ## x dplyr::first()     masks data.table::first()
    ## x dplyr::lag()       masks stats::lag()
    ## x dplyr::last()      masks data.table::last()
    ## x purrr::transpose() masks data.table::transpose()

``` r
library(modelr)
```

    ## Warning: package 'modelr' was built under R version 3.5.2

``` r
df <- get_bls_county()
WIunemployment = df %>% filter(fips_state == 55)
bridges = read_csv("https://www.fhwa.dot.gov/bridge/nbi/2018/delimited/WI18.txt")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   STRUCTURE_NUMBER_008 = col_character(),
    ##   ROUTE_NUMBER_005D = col_character(),
    ##   HIGHWAY_DISTRICT_002 = col_character(),
    ##   COUNTY_CODE_003 = col_character(),
    ##   FEATURES_DESC_006A = col_character(),
    ##   CRITICAL_FACILITY_006B = col_logical(),
    ##   FACILITY_CARRIED_007 = col_character(),
    ##   LOCATION_009 = col_character(),
    ##   LRS_INV_ROUTE_013A = col_character(),
    ##   LAT_016 = col_character(),
    ##   LONG_017 = col_character(),
    ##   MAINTENANCE_021 = col_character(),
    ##   OWNER_022 = col_character(),
    ##   FUNCTIONAL_CLASS_026 = col_character(),
    ##   DESIGN_LOAD_031 = col_character(),
    ##   RAILINGS_036A = col_character(),
    ##   TRANSITIONS_036B = col_character(),
    ##   APPR_RAIL_036C = col_character(),
    ##   APPR_RAIL_END_036D = col_character(),
    ##   NAVIGATION_038 = col_character()
    ##   # ... with 41 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: 3 parsing failures.
    ##   row                     col               expected actual                                                          file
    ##  5739 OTHR_STATE_STRUC_NO_099 no trailing characters   B010 'https://www.fhwa.dot.gov/bridge/nbi/2018/delimited/WI18.txt'
    ## 11175 OPR_RATING_METH_063     a double                 F    'https://www.fhwa.dot.gov/bridge/nbi/2018/delimited/WI18.txt'
    ## 11175 INV_RATING_METH_065     a double                 F    'https://www.fhwa.dot.gov/bridge/nbi/2018/delimited/WI18.txt'

``` r
newdata = bridges %>% 
  group_by(COUNTY_CODE_003) %>% 
  summarise(meanIMP = mean(as.numeric(TOTAL_IMP_COST_096), na.rm = T),
            meanADT = mean(as.numeric(ADT_029), na.rm = T)) %>%
  left_join(WIunemployment,by = c("COUNTY_CODE_003" = "fips_county"))


#data = fread("WA18.txt")
#colnames(data)[1] = "fips_state"
#df$fips_state = as.integer(df$fips_state)
#data = data %>% group_by(fips_state) 
```

Predict the number of unemployed

``` r
#newdata = newdata %>% left_join(data, by = "fips_state")

unem_linear = lm(unemployed ~ meanIMP + meanADT, data = newdata)
summary(unem_linear)
```

    ## 
    ## Call:
    ## lm(formula = unemployed ~ meanIMP + meanADT, data = newdata)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3834.1  -408.8   135.4   402.4  5268.6 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -666.56661  188.16532  -3.542 0.000716 ***
    ## meanIMP        7.40675    2.10964   3.511 0.000792 ***
    ## meanADT        0.36195    0.04111   8.805 6.75e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1050 on 69 degrees of freedom
    ## Multiple R-squared:  0.7771, Adjusted R-squared:  0.7707 
    ## F-statistic: 120.3 on 2 and 69 DF,  p-value: < 2.2e-16

``` r
newdata = newdata %>% add_predictions(unem_linear, var = "pre_unemployed")
```

Predict the unemployment rate

``` r
rateModel = lm(unemployed_rate ~ meanIMP + meanADT, data = newdata)
summary(rateModel)
```

    ## 
    ## Call:
    ## lm(formula = unemployed_rate ~ meanIMP + meanADT, data = newdata)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.5818 -0.7402 -0.0866  0.6121  2.5369 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.106e+00  1.721e-01  23.860  < 2e-16 ***
    ## meanIMP      5.073e-03  1.929e-03   2.629   0.0105 *  
    ## meanADT     -1.803e-04  3.759e-05  -4.796 9.01e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9603 on 69 degrees of freedom
    ## Multiple R-squared:  0.2553, Adjusted R-squared:  0.2337 
    ## F-statistic: 11.83 on 2 and 69 DF,  p-value: 3.826e-05

``` r
newdata = newdata %>% add_predictions(rateModel, var = "pre_rate")
```

Additional predictors using the unemployed number and rate from the
previous month

``` r
df2 <- get_bls_county("November 2019")
WINov = df2 %>% filter(fips_state == 55) %>% select(unemployed, unemployed_rate) 
colnames(WINov)[1] = "Nov_unem"
colnames(WINov)[2] = "Nov_rate"
newdata["Nov_unem"] = WINov[1]
newdata["Nov_rate"] = WINov[2]

m3 = lm(unemployed_rate ~ meanIMP + meanADT + Nov_unem + Nov_rate, data = newdata)
summary(m3)
```

    ## 
    ## Call:
    ## lm(formula = unemployed_rate ~ meanIMP + meanADT + Nov_unem + 
    ##     Nov_rate, data = newdata)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.67036 -0.26647 -0.04738  0.29654  1.05902 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  8.580e-01  2.432e-01   3.528 0.000762 ***
    ## meanIMP      1.242e-03  1.053e-03   1.180 0.242114    
    ## meanADT     -7.742e-05  2.792e-05  -2.773 0.007186 ** 
    ## Nov_unem    -3.859e-06  5.135e-05  -0.075 0.940313    
    ## Nov_rate     9.917e-01  6.731e-02  14.733  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4722 on 67 degrees of freedom
    ## Multiple R-squared:  0.8252, Adjusted R-squared:  0.8148 
    ## F-statistic: 79.07 on 4 and 67 DF,  p-value: < 2.2e-16

``` r
newdata = newdata %>% add_predictions(m3, var = "new_pre_rate")

m4 = lm(unemployed ~ meanIMP + meanADT + Nov_unem + Nov_rate, data = newdata)
summary(m4)
```

    ## 
    ## Call:
    ## lm(formula = unemployed ~ meanIMP + meanADT + Nov_unem + Nov_rate, 
    ##     data = newdata)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -206.24  -57.57  -13.62   47.84  358.10 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 251.363940  50.740287   4.954 5.21e-06 ***
    ## meanIMP       0.270076   0.219593   1.230   0.2230    
    ## meanADT       0.005792   0.005824   0.994   0.3236    
    ## Nov_unem      0.943071   0.010711  88.043  < 2e-16 ***
    ## Nov_rate    -36.603295  14.041661  -2.607   0.0113 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 98.5 on 67 degrees of freedom
    ## Multiple R-squared:  0.9981, Adjusted R-squared:  0.998 
    ## F-statistic:  8779 on 4 and 67 DF,  p-value: < 2.2e-16

``` r
newdata = newdata %>% add_predictions(m4, var = "new_pre_num")
#newdata = left_join(newdata, WINov)

newdata
```

    ## # A tibble: 72 x 18
    ##    COUNTY_CODE_003 meanIMP meanADT area_code fips_state area_title
    ##    <chr>             <dbl>   <dbl> <chr>     <chr>      <chr>     
    ##  1 001                7.46   1174. CN550010… 55         Adams Cou…
    ##  2 003               11.9     742. CN550030… 55         Ashland C…
    ##  3 005               23.2    1533. CN550050… 55         Barron Co…
    ##  4 007               64.9    1510. CN550070… 55         Bayfield …
    ##  5 009              174.    13442. CN550090… 55         Brown Cou…
    ##  6 011               31.7    1015. CN550110… 55         Buffalo C…
    ##  7 013               16.9     828. CN550130… 55         Burnett C…
    ##  8 015               55.8    2765. CN550150… 55         Calumet C…
    ##  9 017              112.     2681. CN550170… 55         Chippewa …
    ## 10 019               63.1    1006. CN550190… 55         Clark Cou…
    ## # … with 62 more rows, and 12 more variables: period <date>,
    ## #   labor_force <dbl>, employed <dbl>, unemployed <dbl>,
    ## #   unemployed_rate <dbl>, fips <chr>, pre_unemployed <dbl>,
    ## #   pre_rate <dbl>, Nov_unem <dbl>, Nov_rate <dbl>, new_pre_rate <dbl>,
    ## #   new_pre_num <dbl>
