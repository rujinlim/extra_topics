stat_learning
================
rl3411
2023-12-07

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3          ✔ readr     2.1.4     
    ## ✔ forcats   1.0.0          ✔ stringr   1.5.0     
    ## ✔ ggplot2   3.4.3          ✔ tibble    3.2.1.9006
    ## ✔ lubridate 1.9.2          ✔ tidyr     1.3.0     
    ## ✔ purrr     1.0.2          
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(glmnet)
```

    ## Loading required package: Matrix
    ## 
    ## Attaching package: 'Matrix'
    ## 
    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack
    ## 
    ## Loaded glmnet 4.1-8

``` r
set.seed(11)
```

## Lasso

Useful when we have many covariates

``` r
bwt_df = 
  read_csv("data/birthweight.csv") |> 
  janitor::clean_names() |>
  mutate(
    babysex = as.factor(babysex),
    babysex = fct_recode(babysex, "male" = "1", "female" = "2"),
    frace = as.factor(frace),
    frace = fct_recode(
      frace, "white" = "1", "black" = "2", "asian" = "3", 
      "puerto rican" = "4", "other" = "8"),
    malform = as.logical(malform),
    mrace = as.factor(mrace),
    mrace = fct_recode(
      mrace, "white" = "1", "black" = "2", "asian" = "3", 
      "puerto rican" = "4")) |> 
  sample_n(200)
```

    ## Rows: 4342 Columns: 20
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (20): babysex, bhead, blength, bwt, delwt, fincome, frace, gaweeks, malf...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Get predictors and outcome

``` r
# set the design matrix

x = model.matrix(bwt ~ ., bwt_df)[, -1]
y = bwt_df |> pull(bwt)
```

Start fitting lasso.

``` r
lambda = 10^(seq(3,-2, -0.1))

lasso_fit = 
  glmnet(x, y, lambda = lambda) # if we don't specify lambda, glmnet will do it for you (or try to)

lasso_cv = 
  cv.glmnet(x, y, lambda = lambda)

lambda_opt = lasso_cv$lambda.min
```

Let’s look at lasso results

``` r
lasso_fit |> 
  broom::tidy() |> 
  filter(step == 42)
```

    ## # A tibble: 20 × 5
    ##    term               step  estimate lambda dev.ratio
    ##    <chr>             <dbl>     <dbl>  <dbl>     <dbl>
    ##  1 (Intercept)          42 -4152.    0.0794     0.635
    ##  2 babysexfemale        42    77.3   0.0794     0.635
    ##  3 bhead                42    84.3   0.0794     0.635
    ##  4 blength              42    72.2   0.0794     0.635
    ##  5 fincome              42     0.498 0.0794     0.635
    ##  6 fraceblack           42   157.    0.0794     0.635
    ##  7 fraceasian           42   -40.2   0.0794     0.635
    ##  8 fracepuerto rican    42   113.    0.0794     0.635
    ##  9 gaweeks              42    25.3   0.0794     0.635
    ## 10 malformTRUE          42   542.    0.0794     0.635
    ## 11 menarche             42   -38.3   0.0794     0.635
    ## 12 mheight              42     5.70  0.0794     0.635
    ## 13 momage               42    -0.962 0.0794     0.635
    ## 14 mraceblack           42  -284.    0.0794     0.635
    ## 15 mraceasian           42    -0.989 0.0794     0.635
    ## 16 mracepuerto rican    42  -299.    0.0794     0.635
    ## 17 ppbmi                42     2.93  0.0794     0.635
    ## 18 ppwt                 42    -1.12  0.0794     0.635
    ## 19 smoken               42    -4.45  0.0794     0.635
    ## 20 wtgain               42     2.96  0.0794     0.635

Create visualizations for regression

``` r
lasso_fit |> 
  broom::tidy() |> 
  select(term, lambda, estimate) |> 
  complete(term, lambda, fill = list(estimate = 0)) |> 
  filter(term != "(Intercept)")  |> 
  ggplot(aes(x = log(lambda,10), y = estimate, color = term, group = term)) +
  geom_path()
```

![](stat_learning_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Show the CV results

``` r
lasso_cv |> 
  broom::tidy() |> 
  ggplot(aes(x = log(lambda, 10), estimate)) +
  geom_point()
```

![](stat_learning_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
