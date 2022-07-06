# mismm() works for data-frame-like inputs

    Code
      bag_preds
    Output
      # A tibble: 8 x 4
      # Groups:   bag_name [8]
        bag_label bag_name .pred_class  .pred
            <dbl> <chr>    <fct>        <dbl>
      1         0 bag1     1            13.6 
      2         0 bag2     1           164.  
      3         0 bag3     1            23.7 
      4         1 bag4     1            47.1 
      5         0 bag5     0           -33.2 
      6         0 bag6     0           -31.4 
      7         0 bag7     1             8.95
      8         1 bag8     1           643.  

---

    Code
      bag_preds
    Output
      # A tibble: 8 x 4
      # Groups:   bag_name [8]
        bag_label bag_name .pred_class .pred
            <dbl> <chr>    <fct>       <dbl>
      1         0 bag1     1           0.826
      2         0 bag2     1           0.780
      3         0 bag3     1           0.705
      4         1 bag4     1           1.00 
      5         0 bag5     1           0.885
      6         0 bag6     1           0.763
      7         0 bag7     1           0.706
      8         1 bag8     1           0.809

---

    Code
      bag_preds
    Output
      # A tibble: 8 x 4
      # Groups:   bag_name [8]
        bag_label bag_name .pred_class  .pred
            <dbl> <chr>    <fct>        <dbl>
      1         0 bag1     0           -0.731
      2         0 bag2     0           -0.821
      3         0 bag3     0           -0.768
      4         1 bag4     0           -0.617
      5         0 bag5     0           -0.809
      6         0 bag6     0           -0.779
      7         0 bag7     0           -0.800
      8         1 bag8     0           -0.638

# `mismm()` value returns make sense

    Code
      models <- list(`mildata-heur` = mismm(df, method = "heuristic"), `mildata-mip` = mismm(
        df, method = "mip", control = list(nystrom_args = list(m = 10))),
      `mildata-qp` = mismm(df, method = "qp-heuristic"), xy = mismm(x = as.data.frame(
        df[, 4:6]), y = df$bag_label, bags = df$bag_name, instances = df$
      instance_name), formula = mismm(mild(bag_label, bag_name, instance_name) ~ .,
      data = df), `no-scale-heur` = mismm(df, method = "heuristic", control = list(
        scale = FALSE)), `no-scale-mip` = mismm(df, method = "mip", control = list(
        scale = FALSE, nystrom_args = list(m = 10))), `no-scale-qp` = mismm(df,
        method = "qp-heuristic", control = list(scale = FALSE)), `no-weights` = mismm(
        df, method = "heuristic", weights = FALSE)) %>% suppressWarnings() %>%
        suppressMessages()
      print(lapply(models, names))
    Output
      $`mildata-heur`
       [1] "ksvm_fit"        "call_type"       "x"               "features"       
       [5] "levels"          "cost"            "sigma"           "weights"        
       [9] "kernel"          "kernel_param"    "repr_inst"       "n_step"         
      [13] "useful_inst_idx" "inst_order"      "x_scale"         "bag_name"       
      [17] "instance_name"  
      
      $`mildata-mip`
       [1] "gurobi_fit"    "kfm_fit"       "call_type"     "features"     
       [5] "levels"        "cost"          "sigma"         "weights"      
       [9] "kernel"        "kernel_param"  "x_scale"       "bag_name"     
      [13] "instance_name"
      
      $`mildata-qp`
       [1] "gurobi_fit"    "call_type"     "x"             "features"     
       [5] "levels"        "cost"          "sigma"         "weights"      
       [9] "kernel"        "kernel_param"  "repr_inst"     "n_step"       
      [13] "x_scale"       "bag_name"      "instance_name"
      
      $xy
       [1] "ksvm_fit"        "call_type"       "x"               "features"       
       [5] "levels"          "cost"            "sigma"           "weights"        
       [9] "kernel"          "kernel_param"    "repr_inst"       "n_step"         
      [13] "useful_inst_idx" "inst_order"      "x_scale"        
      
      $formula
       [1] "ksvm_fit"        "call_type"       "x"               "features"       
       [5] "levels"          "cost"            "sigma"           "weights"        
       [9] "kernel"          "kernel_param"    "repr_inst"       "n_step"         
      [13] "useful_inst_idx" "inst_order"      "x_scale"         "formula"        
      [17] "bag_name"        "instance_name"  
      
      $`no-scale-heur`
       [1] "ksvm_fit"        "call_type"       "x"               "features"       
       [5] "levels"          "cost"            "sigma"           "weights"        
       [9] "kernel"          "kernel_param"    "repr_inst"       "n_step"         
      [13] "useful_inst_idx" "inst_order"      "bag_name"        "instance_name"  
      
      $`no-scale-mip`
       [1] "gurobi_fit"    "kfm_fit"       "call_type"     "features"     
       [5] "levels"        "cost"          "sigma"         "weights"      
       [9] "kernel"        "kernel_param"  "bag_name"      "instance_name"
      
      $`no-scale-qp`
       [1] "gurobi_fit"    "call_type"     "x"             "features"     
       [5] "levels"        "cost"          "sigma"         "weights"      
       [9] "kernel"        "kernel_param"  "repr_inst"     "n_step"       
      [13] "bag_name"      "instance_name"
      
      $`no-weights`
       [1] "ksvm_fit"        "call_type"       "x"               "features"       
       [5] "levels"          "cost"            "sigma"           "kernel"         
       [9] "kernel_param"    "repr_inst"       "n_step"          "useful_inst_idx"
      [13] "inst_order"      "x_scale"         "bag_name"        "instance_name"  
      
    Code
      print(models)
    Output
      $`mildata-heur`
      An mismm object called with mismm.mild_df 
       
      Parameters: 
        method: heuristic 
        kernel: kme w/ radial  (sigma = 0.3333333) 
        cost: 1 
        scale: TRUE 
        weights: ('0' = 0.166666666666667, '1' = 1) 
       
      Model info: 
        Features: chr [1:3] "X1" "X2" "X3"
        Number of iterations: 2 
      
      
      $`mildata-mip`
      An mismm object called with mismm.mild_df 
       
      Parameters: 
        method: mip 
        kernel: kme w/ radial  (sigma = 0.3333333) 
        cost: 1 
        scale: TRUE 
        weights: ('0' = 0.166666666666667, '1' = 1) 
       
      Model info: 
        Features: chr [1:3] "X1" "X2" "X3"
        Gap to optimality: 0 
      
      
      $`mildata-qp`
      An mismm object called with mismm.mild_df 
       
      Parameters: 
        method: qp-heuristic 
        kernel: kme w/ radial  (sigma = 0.3333333) 
        cost: 1 
        scale: TRUE 
        weights: ('0' = 0.166666666666667, '1' = 1) 
       
      Model info: 
        Features: chr [1:3] "X1" "X2" "X3"
        Number of iterations: 1 
      
      
      $xy
      An mismm object called with mismm.default 
       
      Parameters: 
        method: heuristic 
        kernel: kme w/ radial  (sigma = 0.3333333) 
        cost: 1 
        scale: TRUE 
        weights: ('0' = 0.166666666666667, '1' = 1) 
       
      Model info: 
        Features: chr [1:3] "X1" "X2" "X3"
        Number of iterations: 2 
      
      
      $formula
      An mismm object called with mismm.formula 
       
      Parameters: 
        method: heuristic 
        kernel: kme w/ radial  (sigma = 0.3333333) 
        cost: 1 
        scale: TRUE 
        weights: ('0' = 0.166666666666667, '1' = 1) 
       
      Model info: 
        Features: chr [1:3] "X1" "X2" "X3"
        Number of iterations: 2 
      
      
      $`no-scale-heur`
      An mismm object called with mismm.mild_df 
       
      Parameters: 
        method: heuristic 
        kernel: kme w/ radial  (sigma = 0.3333333) 
        cost: 1 
        scale: FALSE 
        weights: ('0' = 0.166666666666667, '1' = 1) 
       
      Model info: 
        Features: chr [1:3] "X1" "X2" "X3"
        Number of iterations: 2 
      
      
      $`no-scale-mip`
      An mismm object called with mismm.mild_df 
       
      Parameters: 
        method: mip 
        kernel: kme w/ radial  (sigma = 0.3333333) 
        cost: 1 
        scale: FALSE 
        weights: ('0' = 0.166666666666667, '1' = 1) 
       
      Model info: 
        Features: chr [1:3] "X1" "X2" "X3"
        Gap to optimality: 0 
      
      
      $`no-scale-qp`
      An mismm object called with mismm.mild_df 
       
      Parameters: 
        method: qp-heuristic 
        kernel: kme w/ radial  (sigma = 0.3333333) 
        cost: 1 
        scale: FALSE 
        weights: ('0' = 0.166666666666667, '1' = 1) 
       
      Model info: 
        Features: chr [1:3] "X1" "X2" "X3"
        Number of iterations: 1 
      
      
      $`no-weights`
      An mismm object called with mismm.mild_df 
       
      Parameters: 
        method: heuristic 
        kernel: kme w/ radial  (sigma = 0.3333333) 
        cost: 1 
        scale: TRUE 
        weights: FALSE 
       
      Model info: 
        Features: chr [1:3] "X1" "X2" "X3"
        Number of iterations: 2 
      
      

