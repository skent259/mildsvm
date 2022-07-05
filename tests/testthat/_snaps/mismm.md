# mismm() works for data-frame-like inputs

    Code
      bag_preds
    Output
      # A tibble: 10 x 4
      # Groups:   bag_name [10]
         bag_label bag_name .pred_class  .pred
             <dbl> <chr>    <fct>        <dbl>
       1         0 bag1     1            1096.
       2         0 bag2     1             588.
       3         1 bag3     1             913.
       4         0 bag4     1             209.
       5         0 bag5     0           -1195.
       6         1 bag6     1           10652.
       7         0 bag7     1             575.
       8         0 bag8     1             302.
       9         1 bag9     1           10031.
      10         1 bag10    1             816.

---

    Code
      bag_preds
    Output
      # A tibble: 10 x 4
      # Groups:   bag_name [10]
         bag_label bag_name .pred_class  .pred
             <dbl> <chr>    <fct>        <dbl>
       1         0 bag1     0           -0.886
       2         0 bag2     0           -0.830
       3         1 bag3     1            1.01 
       4         0 bag4     0           -0.895
       5         0 bag5     0           -1.10 
       6         1 bag6     1            1.00 
       7         0 bag7     0           -0.918
       8         0 bag8     0           -0.934
       9         1 bag9     1            0.971
      10         1 bag10    1            0.885

---

    Code
      bag_preds
    Output
      # A tibble: 10 x 4
      # Groups:   bag_name [10]
         bag_label bag_name .pred_class  .pred
             <dbl> <chr>    <fct>        <dbl>
       1         0 bag1     0           -0.829
       2         0 bag2     0           -0.721
       3         1 bag3     0           -0.494
       4         0 bag4     0           -0.880
       5         0 bag5     0           -0.892
       6         1 bag6     0           -0.575
       7         0 bag7     0           -0.878
       8         0 bag8     0           -0.805
       9         1 bag9     0           -0.581
      10         1 bag10    0           -0.552

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
        weights: ('0' = 0.222222222222222, '1' = 1) 
       
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
        weights: ('0' = 0.222222222222222, '1' = 1) 
       
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
        weights: ('0' = 0.222222222222222, '1' = 1) 
       
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
        weights: ('0' = 0.222222222222222, '1' = 1) 
       
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
        weights: ('0' = 0.222222222222222, '1' = 1) 
       
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
        weights: ('0' = 0.222222222222222, '1' = 1) 
       
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
        weights: ('0' = 0.222222222222222, '1' = 1) 
       
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
        weights: ('0' = 0.222222222222222, '1' = 1) 
       
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
      
      

