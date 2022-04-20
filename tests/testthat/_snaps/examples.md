# `mildsvm()` example works

    Code
      set.seed(8)
      mil_data <- generate_mild_df(nbag = 15, nsample = 20, positive_prob = 0.15,
        sd_of_mean = rep(0.1, 3))
      mdl1 <- mildsvm(mil_data)
      mdl2 <- mildsvm(mild(bag_label, bag_name, instance_name) ~ X1 + X2 + X3, data = mil_data)
      if (require(gurobi)) {
        mdl3 <- mildsvm(mil_data, method = "mip", control = list(nystrom_args = list(
          m = 10, r = 10)))
        predict(mdl3, mil_data)
      }
    Message <packageStartupMessage>
      Loading required package: gurobi
      Loading required package: slam
    Output
      # A tibble: 1,200 x 1
         .pred_class
         <fct>      
       1 0          
       2 0          
       3 0          
       4 0          
       5 0          
       6 0          
       7 0          
       8 0          
       9 0          
      10 0          
      # ... with 1,190 more rows
    Code
      predict(mdl1, new_data = mil_data, type = "raw", layer = "bag")
    Output
      # A tibble: 1,200 x 1
          .pred
          <dbl>
       1 -0.289
       2 -0.289
       3 -0.289
       4 -0.289
       5 -0.289
       6 -0.289
       7 -0.289
       8 -0.289
       9 -0.289
      10 -0.289
      # ... with 1,190 more rows
    Code
      mil_data %>% bind_cols(predict(mdl2, mil_data, type = "class")) %>% bind_cols(
        predict(mdl2, mil_data, type = "raw")) %>% distinct(bag_name, bag_label,
        .pred_class, .pred)
    Output
         bag_label bag_name .pred_class       .pred
      1          0     bag1           0 -0.11956070
      2          1     bag2           1  0.21090014
      3          0     bag3           0 -0.09390996
      4          1     bag4           1  0.09450872
      5          0     bag5           0 -0.09219624
      6          0     bag6           0 -0.13385229
      7          1     bag7           1  0.15464759
      8          0     bag8           0 -0.04083047
      9          1     bag9           1  0.16941904
      10         1    bag10           1  0.25006646
      11         1    bag11           1  0.13690982
      12         0    bag12           0 -0.05835921
      13         1    bag13           1  0.17325203
      14         1    bag14           1  0.14325177
      15         1    bag15           1  0.32877489

# `predict.mildsvm()` examples work

    Code
      mil_data <- generate_mild_df(nbag = 15, nsample = 20, positive_prob = 0.15,
        sd_of_mean = rep(0.1, 3))
      mdl1 <- mildsvm(mil_data, control = list(sigma = 1 / 5))
      mil_data %>% bind_cols(predict(mdl1, mil_data, type = "class")) %>% bind_cols(
        predict(mdl1, mil_data, type = "raw")) %>% distinct(bag_name, bag_label,
        .pred_class, .pred)
    Output
         bag_label bag_name .pred_class       .pred
      1          0     bag1           0 -0.37740255
      2          1     bag2           1  0.28344679
      3          0     bag3           0 -0.33199064
      4          1     bag4           1  0.13194961
      5          0     bag5           0 -0.33495785
      6          0     bag6           0 -0.24794687
      7          1     bag7           1  0.26145917
      8          0     bag8           0 -0.06040958
      9          1     bag9           1  0.37934673
      10         1    bag10           1  0.39238521
      11         1    bag11           1  0.30107408
      12         0    bag12           0 -0.28196329
      13         1    bag13           1  0.32638769
      14         1    bag14           1  0.22273935
      15         1    bag15           1  0.45906821
    Code
      mil_data %>% bind_cols(predict(mdl1, mil_data, type = "class", layer = "instance")) %>%
        bind_cols(predict(mdl1, mil_data, type = "raw", layer = "instance")) %>%
        distinct(bag_name, instance_name, bag_label, .pred_class, .pred)
    Output
         bag_label bag_name instance_name .pred_class       .pred
      1          0     bag1     bag1inst1           0 -0.38021259
      2          0     bag1     bag1inst2           0 -0.42078066
      3          0     bag1     bag1inst3           0 -0.37740255
      4          0     bag1     bag1inst4           0 -0.42800397
      5          1     bag2     bag2inst1           0 -0.32139888
      6          1     bag2     bag2inst2           0 -0.36278924
      7          1     bag2     bag2inst3           0 -0.25074180
      8          1     bag2     bag2inst4           1  0.28344679
      9          0     bag3     bag3inst1           0 -0.33199064
      10         0     bag3     bag3inst2           0 -0.39525382
      11         0     bag3     bag3inst3           0 -0.34569461
      12         0     bag3     bag3inst4           0 -0.33304887
      13         1     bag4     bag4inst1           0 -0.31900998
      14         1     bag4     bag4inst2           0 -0.24545506
      15         1     bag4     bag4inst3           0 -0.22103767
      16         1     bag4     bag4inst4           1  0.13194961
      17         0     bag5     bag5inst1           0 -0.45906816
      18         0     bag5     bag5inst2           0 -0.44501101
      19         0     bag5     bag5inst3           0 -0.33495785
      20         0     bag5     bag5inst4           0 -0.40857446
      21         0     bag6     bag6inst1           0 -0.39161068
      22         0     bag6     bag6inst2           0 -0.30728351
      23         0     bag6     bag6inst3           0 -0.24794687
      24         0     bag6     bag6inst4           0 -0.40984793
      25         1     bag7     bag7inst1           0 -0.22847389
      26         1     bag7     bag7inst2           1  0.26145917
      27         1     bag7     bag7inst3           0 -0.31135055
      28         1     bag7     bag7inst4           0 -0.26689426
      29         0     bag8     bag8inst1           0 -0.06040958
      30         0     bag8     bag8inst2           0 -0.31502077
      31         0     bag8     bag8inst3           0 -0.43158096
      32         0     bag8     bag8inst4           0 -0.32769429
      33         1     bag9     bag9inst1           1  0.37934673
      34         1     bag9     bag9inst2           0 -0.25656688
      35         1     bag9     bag9inst3           0 -0.15597718
      36         1     bag9     bag9inst4           0 -0.31327836
      37         1    bag10    bag10inst1           1  0.39238521
      38         1    bag10    bag10inst2           1  0.37351991
      39         1    bag10    bag10inst3           0 -0.22796150
      40         1    bag10    bag10inst4           0 -0.30594715
      41         1    bag11    bag11inst1           1  0.30107408
      42         1    bag11    bag11inst2           0 -0.25283830
      43         1    bag11    bag11inst3           0 -0.31290676
      44         1    bag11    bag11inst4           1  0.10095951
      45         0    bag12    bag12inst1           0 -0.39928055
      46         0    bag12    bag12inst2           0 -0.34958706
      47         0    bag12    bag12inst3           0 -0.28196329
      48         0    bag12    bag12inst4           0 -0.35179512
      49         1    bag13    bag13inst1           1  0.32638769
      50         1    bag13    bag13inst2           0 -0.25396081
      51         1    bag13    bag13inst3           0 -0.41066831
      52         1    bag13    bag13inst4           0 -0.36702806
      53         1    bag14    bag14inst1           0 -0.27036333
      54         1    bag14    bag14inst2           0 -0.33179443
      55         1    bag14    bag14inst3           1  0.22273935
      56         1    bag14    bag14inst4           0 -0.34223518
      57         1    bag15    bag15inst1           0 -0.30562584
      58         1    bag15    bag15inst2           0 -0.23537016
      59         1    bag15    bag15inst3           0 -0.21896721
      60         1    bag15    bag15inst4           1  0.45906821

# `misvm()` examples work

    Code
      set.seed(8)
      mil_data <- generate_mild_df(nbag = 20, positive_prob = 0.15, sd_of_mean = rep(
        0.1, 3))
      df <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))
      mdl1 <- misvm(x = df[, 4:123], y = df$bag_label, bags = df$bag_name, method = "heuristic")
      mdl2 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df)
      if (require(gurobi)) {
        mdl3 <- misvm(x = df[, 4:123], y = df$bag_label, bags = df$bag_name, method = "mip")
      }
      predict(mdl1, new_data = df, type = "raw", layer = "bag")
    Output
      # A tibble: 80 x 1
         .pred
         <dbl>
       1 -1.04
       2 -1.04
       3 -1.04
       4 -1.04
       5  1.00
       6  1.00
       7  1.00
       8  1.00
       9 -1.00
      10 -1.00
      # ... with 70 more rows
    Code
      df %>% bind_cols(predict(mdl2, df, type = "class")) %>% bind_cols(predict(mdl2,
        df, type = "raw")) %>% distinct(bag_name, bag_label, .pred_class, .pred)
    Output
         bag_label bag_name .pred_class       .pred
      1          0     bag1           0 -0.11805071
      2          1     bag2           1  1.01732791
      3          0     bag3           0 -0.24540426
      4          1     bag4           1  1.00046917
      5          0     bag5           1  0.15460188
      6          0     bag6           1  0.87469487
      7          1     bag7           1  0.16754553
      8          0     bag8           1  1.00811386
      9          1     bag9           1  0.99998275
      10         1    bag10           1  2.67168111
      11         1    bag11           1  0.29471379
      12         0    bag12           1  1.52487131
      13         1    bag13           1  2.15326561
      14         1    bag14           1  0.99956477
      15         1    bag15           0 -0.38940230
      16         1    bag16           1  0.67654218
      17         0    bag17           1  0.39241276
      18         1    bag18           0 -0.11878006
      19         0    bag19           1  0.06554383
      20         1    bag20           1  0.85951804

# `cv_misvm()` examples work

    Code
      set.seed(8)
      mil_data <- generate_mild_df(nbag = 20, positive_prob = 0.15, dist = rep(
        "mvnormal", 3), mean = list(rep(1, 10), rep(2, 10)), sd_of_mean = rep(0.1, 3))
      df <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))
      cost_seq <- 2^seq(-5, 7, length.out = 3)
      mdl1 <- cv_misvm(x = df[, 4:123], y = df$bag_label, bags = df$bag_name,
      cost_seq = cost_seq, n_fold = 3, method = "heuristic")
      mdl2 <- cv_misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df,
      cost_seq = cost_seq, n_fold = 3)
      if (require(gurobi)) {
        mdl3 <- cv_misvm(x = df[, 4:123], y = df$bag_label, bags = df$bag_name,
        cost_seq = cost_seq, n_fold = 3, method = "mip")
      }
      predict(mdl1, new_data = df, type = "raw", layer = "bag")
    Output
      # A tibble: 80 x 1
         .pred
         <dbl>
       1 -1.00
       2 -1.00
       3 -1.00
       4 -1.00
       5  1.04
       6  1.04
       7  1.04
       8  1.04
       9 -1.13
      10 -1.13
      # ... with 70 more rows
    Code
      df %>% bind_cols(predict(mdl2, df, type = "class")) %>% bind_cols(predict(mdl2,
        df, type = "raw")) %>% distinct(bag_name, bag_label, .pred_class, .pred)
    Output
         bag_label bag_name .pred_class      .pred
      1          0     bag1           0 -0.5932349
      2          1     bag2           1  0.7493612
      3          0     bag3           0 -0.9387030
      4          1     bag4           1  1.2126533
      5          0     bag5           0 -0.8094506
      6          0     bag6           0 -0.8083522
      7          1     bag7           1  0.6587946
      8          0     bag8           0 -0.9032079
      9          1     bag9           1  0.5855234
      10         1    bag10           1  1.2019300
      11         1    bag11           1  1.2689043
      12         0    bag12           0 -0.8143970
      13         1    bag13           1  0.8591738
      14         1    bag14           1  1.0000000
      15         1    bag15           1  1.1078369
      16         1    bag16           1  1.2117319
      17         0    bag17           0 -0.6022075
      18         1    bag18           1  0.9355648
      19         0    bag19           0 -0.7314129
      20         1    bag20           1  1.0393764

# `smm()` examples work

    Code
      set.seed(8)
      n_instances <- 10
      n_samples <- 20
      y <- rep(c(1, -1), each = n_samples * n_instances / 2)
      instances <- as.character(rep(1:n_instances, each = n_samples))
      x <- data.frame(x1 = rnorm(length(y), mean = 1 * (y == 1)), x2 = rnorm(length(y),
      mean = 2 * (y == 1)), x3 = rnorm(length(y), mean = 3 * (y == 1)))
      df <- data.frame(instance_name = instances, y = y, x)
      mdl <- smm(x, y, instances)
      mdl2 <- smm(y ~ ., data = df)
      df %>% dplyr::bind_cols(predict(mdl, type = "raw", new_data = x, new_instances = instances)) %>%
        dplyr::bind_cols(predict(mdl, type = "class", new_data = x, new_instances = instances)) %>%
        dplyr::distinct(instance_name, y, .pred, .pred_class)
    Output
         instance_name  y      .pred .pred_class
      1              1  1  1.0000000           1
      2              2  1  0.9038444           1
      3              3  1  1.1047533           1
      4              4  1  0.9112317           1
      5              5  1  0.8965109           1
      6              6 -1 -1.1437942          -1
      7              7 -1 -1.0000000          -1
      8              8 -1 -1.0679171          -1
      9              9 -1 -1.1645562          -1
      10            10 -1 -1.2293499          -1

# `generate_mild_df()` examples work

    Code
      set.seed(8)
      mild_data <- generate_mild_df(nbag = 7, ninst = 3, nsample = 20, ncov = 2,
        nimp_pos = 1, dist = rep("mvnormal", 3), mean = list(rep(5, 1), rep(15, 2), 0))
      library(dplyr)
      distinct(mild_data, bag_label, bag_name, instance_name)
    Output
         bag_label bag_name instance_name
      1          0     bag1     bag1inst1
      2          0     bag1     bag1inst2
      3          0     bag1     bag1inst3
      4          0     bag2     bag2inst1
      5          0     bag2     bag2inst2
      6          0     bag2     bag2inst3
      7          1     bag3     bag3inst1
      8          1     bag3     bag3inst2
      9          1     bag3     bag3inst3
      10         0     bag4     bag4inst1
      11         0     bag4     bag4inst2
      12         0     bag4     bag4inst3
      13         0     bag5     bag5inst1
      14         0     bag5     bag5inst2
      15         0     bag5     bag5inst3
      16         1     bag6     bag6inst1
      17         1     bag6     bag6inst2
      18         1     bag6     bag6inst3
      19         0     bag7     bag7inst1
      20         0     bag7     bag7inst2
      21         0     bag7     bag7inst3
    Code
      split(mild_data[, 4:5], mild_data$instance_name) %>% sapply(colMeans) %>% round(
        2) %>% t()
    Output
                   X1    X2
      bag1inst1 14.95 14.63
      bag1inst2 15.53 15.42
      bag1inst3 14.27 16.08
      bag2inst1 15.29 15.01
      bag2inst2 15.78 14.87
      bag2inst3 14.53 15.71
      bag3inst1 14.79 14.83
      bag3inst2  5.36 -0.69
      bag3inst3 15.13 15.78
      bag4inst1 15.32 14.46
      bag4inst2 15.31 15.53
      bag4inst3 15.93 14.55
      bag5inst1 15.25 14.25
      bag5inst2 14.98 16.03
      bag5inst3 15.48 14.95
      bag6inst1  4.83  0.26
      bag6inst2 16.23 16.06
      bag6inst3 15.31 14.56
      bag7inst1 14.74 14.90
      bag7inst2 14.61 15.03
      bag7inst3 15.17 15.20

