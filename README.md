
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mildsvm

<!-- badges: start -->

[![R-CMD-check](https://github.com/skent259/mildsvm/workflows/R-CMD-check/badge.svg)](https://github.com/skent259/mildsvm/actions)
<!-- badges: end -->

mildsvm contains popular methods for training classifiers on
multiple-instance learning (MIL) data. A core feature of MIL data is
that instances are grouped into bags, and only the bag label is observed
(in contrast to supervised learning where all instances will have an
instance label).

In particular, mildsvm implements

-   `misvm()`: The MI-SVM approach from [Andrews et
    al. (2003)](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.86.8281&rep=rep1&type=pdf)
    “Support Vector Machines for Multiple-Instance Learning”
-   `mismm()`: Novel research code for training a MI-SVM classifier.
-   `smm()`: Support Measure Machines (for supervised data) based on
    [Muandet et
    al. (2012)](https://papers.nips.cc/paper/2012/file/9bf31c7ff062936a96d3c8bd1f8f2ff3-Paper.pdf)
    “Learning from Distributions via Support Measure Machines”

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

## Usage

mildsvm works well for two types of data: MIL and MILD.

MILD data comes from a 3-level structure with *bags*, *instances*, and
*samples*. As in MIL, *instances* are contained within *bags* (where we
only observe the bag label). However, for MILD, each instance represents
a distribution, and the *samples* are drawn from this distribution.

You can generate MILD data with `generate_mild_df()`:

``` r
# Normal(mean=0, sd=1) vs Normal(mean=3, sd=1)
library(mildsvm)
set.seed(4)
(mild_df <- generate_mild_df(
  ncov = 1, nimp_pos = 1, nimp_neg = 1, 
  positive_dist = "mvnormal", positive_mean = 3,
  negative_dist = "mvnormal", negative_mean = 0, 
  nbag = 4,
  ninst = 2, 
  nsample = 2
))
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

If you summarize a MILD data set (for example, by taking the mean of
each covariate), you can recover a MIL data set. Use
`summarize_samples()` for this:

``` r
( mil_df <- summarize_samples(mild_df, .fns = list(mean = mean)) )
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
fit1 <- misvm(mi(bag_label, bag_name) ~ mean, data = mil_df, cost = 100)

# predict
library(dplyr, quietly = TRUE)
mil_df %>% 
  dplyr::bind_cols(predict(fit1, mil_df, type = "raw")) %>% 
  dplyr::bind_cols(predict(fit1, mil_df, type = "class")) %>% 
  dplyr::distinct(bag_label, bag_name, .pred, .pred_class)
#> # A tibble: 4 × 4
#>   bag_label bag_name  .pred .pred_class
#>       <dbl> <chr>     <dbl> <fct>      
#> 1         0 bag1      1.24  1          
#> 2         0 bag2     -1.00  0          
#> 3         1 bag3      1.00  1          
#> 4         1 bag4     -0.175 0
```

Similarly, you can train a MILD-SVM classifier using `mismm()` on the
MILD data:

``` r
fit2 <- mismm(mild(bag_label, bag_name, instance_name) ~ X1, data = mild_df, cost = 100)

# predict
mild_df %>% 
  dplyr::bind_cols(predict(fit2, mild_df, type = "raw")) %>% 
  dplyr::bind_cols(predict(fit2, mild_df, type = "class")) %>% 
  dplyr::distinct(bag_label, bag_name, .pred, .pred_class)
#> # A tibble: 4 × 4
#>   bag_label bag_name  .pred .pred_class
#>       <dbl> <chr>     <dbl> <fct>      
#> 1         0 bag1     -1.18  0          
#> 2         0 bag2      0.482 1          
#> 3         1 bag3      1.00  1          
#> 4         1 bag4      1.00  1
```

<!-- TODO: create a vignette and link -->
