
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mildsvm

<!-- badges: start -->

[![R-CMD-check](https://github.com/skent259/mildsvm/workflows/R-CMD-check/badge.svg)](https://github.com/skent259/mildsvm/actions)
[![Codecov test
coverage](https://codecov.io/gh/skent259/mildsvm/branch/master/graph/badge.svg)](https://app.codecov.io/gh/skent259/mildsvm?branch=master)
<!-- badges: end -->

Weakly supervised (WS), multiple instance (MI) data lives in numerous
interesting applications such as drug discovery, object detection, and
tumor prediction on whole slide images. The `mildsvm` package provides
an easy way to learn from this data by training Support Vector Machine
(SVM)-based classifiers. It also contains helpful functions for building
and printing multiple instance data frames.

The `mildsvm` package implements methods that cover a variety of data
types, including:

-   ordinal and binary labels
-   weakly supervised and traditional supervised structures
-   vector-based and distributional-instance rows of data

A full table of functions with references is available
[below](#methods-implemented). We highlight two methods based on recent
research:

-   `omisvm()` runs a novel OMI-SVM approach for ordinal, multiple
    instance (weakly supervised) data using the work of Kent and Yu
    (2022+)
-   `mismm()` run the MISMM approach for binary, weakly supervised data
    where the instances can be thought of as a matrix of draws from a
    distribution. This non-convex SVM approach is formalized and applied
    to breast cancer diagnosis based on morphological features of the
    tumor microenvironment in [Kent and Yu
    (2022)](https://arxiv.org/abs/2206.14704).

## Usage

A typical MI data frame (a `mi_df`) with ordinal labels might look like
this, with multiple rows of information for each of the `bag_name`s
involved and a label that matches each bag:

``` r
library(mildsvm)
data("ordmvnorm")

print(ordmvnorm)
#> # An MI data frame: 1,000 × 7 with 200 bags
#> # and instance labels: 1, 1, 2, 1, 1, ...
#>    bag_label bag_name    V1     V2      V3       V4     V5
#>  *     <int>    <int> <dbl>  <dbl>   <dbl>    <dbl>  <dbl>
#>  1         2        1 1.55  -0.977  1.33   -0.659   -0.694
#>  2         2        1 0.980 -2.10  -0.618   2.15    -0.718
#>  3         2        1 6.16  -0.275  2.07   -0.624    0.444
#>  4         2        1 2.90  -2.15  -0.0407 -0.0629   1.38 
#>  5         2        1 2.62  -1.70   1.35   -1.66     1.23 
#>  6         4        2 3.39  -0.927  1.95    0.216   -0.164
#>  7         4        2 3.05  -0.930  1.34   -0.457    0.362
#>  8         4        2 6.63  -4.57   4.66   -0.00729  1.03 
#>  9         4        2 4.38  -0.714  2.32    0.0996   0.379
#> 10         4        2 2.43  -4.28   1.08    0.283   -1.14 
#> # … with 990 more rows
# dplyr::distinct(ordmvnorm, bag_label, bag_name)
```

The `mildsvm` package uses the familiar formula and predict methods that
R uses will be familiar with. To indicate that MI data is involved, we
specify the unique bag label and bag name with
`mi(bag_label, bag_name) ~ predictors`:

``` r
fit <- omisvm(mi(bag_label, bag_name) ~ V1 + V2 + V3,
              data = ordmvnorm, 
              weights = NULL)
print(fit)
#> An misvm object called with omisvm.formula 
#>  
#> Parameters: 
#>   method: qp-heuristic 
#>   kernel: linear  
#>   cost: 1 
#>   h: 1 
#>   s: 4 
#>   scale: TRUE 
#>   weights: FALSE 
#>  
#> Model info: 
#>   Levels of `y`: chr [1:5] "1" "2" "3" "4" "5"
#>   Features: chr [1:3] "V1" "V2" "V3"
#>   Number of iterations: 4
predict(fit, new_data = ordmvnorm)
#> # A tibble: 1,000 × 1
#>    .pred_class
#>    <fct>      
#>  1 2          
#>  2 2          
#>  3 2          
#>  4 2          
#>  5 2          
#>  6 4          
#>  7 4          
#>  8 4          
#>  9 4          
#> 10 4          
#> # … with 990 more rows
```

Or, if the data frame has the `mi_df` class, we can directly pass it to
the function and all features will be included:

``` r
fit2 <- omisvm(ordmvnorm)
#> Warning: Weights are not currently implemented for `omisvm()` when `kernel ==
#> 'linear'`.
print(fit2)
#> An misvm object called with omisvm.mi_df 
#>  
#> Parameters: 
#>   method: qp-heuristic 
#>   kernel: linear  
#>   cost: 1 
#>   h: 1 
#>   s: 4 
#>   scale: TRUE 
#>   weights: FALSE 
#>  
#> Model info: 
#>   Levels of `y`: chr [1:5] "1" "2" "3" "4" "5"
#>   Features: chr [1:5] "V1" "V2" "V3" "V4" "V5"
#>   Number of iterations: 3
```

## Installation

mildsvm is not currently on CRAN.
<!-- You can install the released version of mildsvm from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->
<!-- install.packages("mildsvm") -->
<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("skent259/mildsvm")
```

## Additional Usage

`mildsvm` also works well MI data with distributional instances. There
is a 3-level structure with *bags*, *instances*, and *samples*. As in
MIL, *instances* are contained within *bags* (where we only observe the
bag label). However, for MILD, each instance represents a distribution,
and the *samples* are drawn from this distribution.

You can generate MILD data with `generate_mild_df()`:

``` r
# Normal(mean=0, sd=1) vs Normal(mean=3, sd=1)
set.seed(4)
mild_df <- generate_mild_df(
  ncov = 1, nimp_pos = 1, nimp_neg = 1, 
  positive_dist = "mvnormal", positive_mean = 3,
  negative_dist = "mvnormal", negative_mean = 0, 
  nbag = 4,
  ninst = 2, 
  nsample = 2
)
print(mild_df)
#> # An MILD data frame: 16 × 4 with 4 bags, 8 instances
#> # and instance labels: 0, 0, 0, 0, 0, ...
#>    bag_label bag_name instance_name      X1
#>        <dbl> <chr>    <chr>           <dbl>
#>  1         0 bag1     bag1inst1      1.51  
#>  2         0 bag1     bag1inst1     -0.463 
#>  3         0 bag1     bag1inst2      1.79  
#>  4         0 bag1     bag1inst2      1.67  
#>  5         0 bag2     bag2inst1      0.299 
#>  6         0 bag2     bag2inst1      0.666 
#>  7         0 bag2     bag2inst2      0.0118
#>  8         0 bag2     bag2inst2      0.146 
#>  9         1 bag3     bag3inst1      0.546 
#> 10         1 bag3     bag3inst1      0.473 
#> 11         1 bag3     bag3inst2      1.94  
#> 12         1 bag3     bag3inst2      1.25  
#> 13         1 bag4     bag4inst1      1.11  
#> 14         1 bag4     bag4inst1      0.768 
#> 15         1 bag4     bag4inst2      0.111 
#> 16         1 bag4     bag4inst2     -0.290
```

You can train a MISVM classifier using `mismm()` on the MILD data with
the `mild()` formula specification:

``` r
fit3 <- mismm(mild(bag_label, bag_name, instance_name) ~ X1, data = mild_df, cost = 100)

# summarize predictions at the bag layer
mild_df %>% 
  dplyr::bind_cols(predict(fit3, mild_df, type = "raw")) %>% 
  dplyr::bind_cols(predict(fit3, mild_df, type = "class")) %>% 
  dplyr::distinct(bag_label, bag_name, .pred, .pred_class)
#> # A tibble: 4 × 4
#>   bag_label bag_name  .pred .pred_class
#>       <dbl> <chr>     <dbl> <fct>      
#> 1         0 bag1     -1.18  0          
#> 2         0 bag2      0.482 1          
#> 3         1 bag3      1.00  1          
#> 4         1 bag4      1.00  1
```

If you summarize a MILD data set (for example, by taking the mean of
each covariate), you can recover a MIL data set. Use
`summarize_samples()` for this:

``` r
mil_df <- summarize_samples(mild_df, .fns = list(mean = mean)) 
print(mil_df)
#> # A tibble: 8 × 4
#>   bag_label bag_name instance_name    mean
#>       <dbl> <chr>    <chr>           <dbl>
#> 1         0 bag1     bag1inst1      0.522 
#> 2         0 bag1     bag1inst2      1.73  
#> 3         0 bag2     bag2inst1      0.483 
#> 4         0 bag2     bag2inst2      0.0791
#> 5         1 bag3     bag3inst1      0.510 
#> 6         1 bag3     bag3inst2      1.59  
#> 7         1 bag4     bag4inst1      0.941 
#> 8         1 bag4     bag4inst2     -0.0896
```

You can train an MI-SVM classifier using `misvm()` on MIL data with the
helper function `mi()`:

``` r
fit4 <- misvm(mi(bag_label, bag_name) ~ mean, data = mil_df, cost = 100)

print(fit4)
#> An misvm object called with misvm.formula 
#>  
#> Parameters: 
#>   method: heuristic 
#>   kernel: linear  
#>   cost: 100 
#>   scale: TRUE 
#>   weights: ('0' = 0.5, '1' = 1) 
#>  
#> Model info: 
#>   Features: chr "mean"
#>   Number of iterations: 2
```

### Methods implemented

| Function        | Method           | Outcome/label | Data type             | Extra libraries | Reference    |
|-----------------|------------------|---------------|-----------------------|-----------------|--------------|
| `omisvm()`      | `"qp-heuristic"` | ordinal       | MI                    | gurobi          | \[1\]        |
| `mismm()`       | `"heuristic"`    | binary        | distributional MI     | —               | \[2\]        |
| `mismm()`       | `"mip"`          | binary        | distributional MI     | gurobi          | \[2\]        |
| `mismm()`       | `"qp-heuristic"` | binary        | distributional MI     | gurobi          | \[2\]        |
| `misvm()`       | `"heuristic"`    | binary        | MI                    | —               | \[3\]        |
| `misvm()`       | `"mip"`          | binary        | MI                    | gurobi          | \[3\], \[2\] |
| `misvm()`       | `"qp-heuristic"` | binary        | MI                    | gurobi          | \[3\]        |
| `mior()`        | `"qp-heuristic"` | ordinal       | MI                    | gurobi          | \[4\]        |
| `misvm_orova()` | `"heuristic"`    | ordinal       | MI                    | —               | \[3\], \[1\] |
| `misvm_orova()` | `"mip"`          | ordinal       | MI                    | gurobi          | \[3\], \[1\] |
| `misvm_orova()` | `"qp-heuristic"` | ordinal       | MI                    | gurobi          | \[3\], \[1\] |
| `svor_exc()`    | `"smo"`          | ordinal       | vector                | —               | \[5\]        |
| `smm()`         | —                | binary        | distributional vector | —               | \[6\]        |

#### Table acronyms

-   MI: multiple instance
-   SVM: support vector machine
-   SMM: support measure machine
-   OR: ordinal regression
-   OVA: one-vs-all
-   MIP: mixed integer programming
-   QP: quadratic programming
-   SVOR: support vector ordinal regression
-   EXC: explicit constraints
-   SMO: sequential minimal optimization

### References

\[1\] Kent, S., & Yu, M. (2022+). Ordinal multiple instance support
vector machines. *In prep.*

\[2\] [Kent, S., & Yu, M. (2022)](https://arxiv.org/abs/2206.14704).
Non-convex SVM for cancer diagnosis based on morphologic features of
tumor microenvironment. *arXiv preprint arXiv:2206.14704.*

\[3\] Andrews, S., Tsochantaridis, I., & Hofmann, T. (2002). Support
vector machines for multiple-instance learning. *Advances in neural
information processing systems, 15.*

\[4\] Xiao, Y., Liu, B., & Hao, Z. (2017). Multiple-instance ordinal
regression. *IEEE Transactions on Neural Networks and Learning Systems*,
*29*(9), 4398-4413.

\[5\] Chu, W., & Keerthi, S. S. (2007). Support vector ordinal
regression. *Neural computation*, *19*(3), 792-815.

\[6\] Muandet, K., Fukumizu, K., Dinuzzo, F., & Schölkopf, B. (2012).
Learning from distributions via support measure machines. *Advances in
neural information processing systems*, *25*.

<!-- Links that are re-used -->
<!-- TODO: create a vignette and link -->
