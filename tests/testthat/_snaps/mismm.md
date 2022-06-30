# `mismm()` value returns make sense

    Code
      models <- list(`mildata-heur` = mismm(mil_data, method = "heuristic"),
      `mildata-mip` = mismm(mil_data, method = "mip", control = list(nystrom_args = list(
        m = 10))), `mildata-qp` = mismm(mil_data, method = "qp-heuristic"), xy = mismm(
        x = as.data.frame(mil_data[, 4:13]), y = mil_data$bag_label, bags = mil_data$
          bag_name, instances = mil_data$instance_name), formula = mismm(mild(
        bag_label, bag_name, instance_name) ~ ., data = mil_data), `no-scale-heur` = mismm(
        mil_data, method = "heuristic", control = list(scale = FALSE)),
      `no-scale-mip` = mismm(mil_data, method = "mip", control = list(scale = FALSE,
        nystrom_args = list(m = 10))), `no-scale-qp` = mismm(mil_data, method = "qp-heuristic",
        control = list(scale = FALSE)), `no-weights` = mismm(mil_data, method = "heuristic",
        weights = FALSE)) %>% suppressWarnings() %>% suppressMessages()
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
        kernel: kme w/ radial  (sigma = 0.1) 
        cost: 1 
        scale: TRUE 
        weights: ('0' = 0.222222222222222, '1' = 1) 
       
      Model info: 
        Features: chr [1:10] "X1" "X2" "X3" "X4" "X5" "X6" "X7" "X8" "X9" ...
        Number of iterations: 2 
      
      
      $`mildata-mip`
      An mismm object called with mismm.mild_df 
       
      Parameters: 
        method: mip 
        kernel: kme w/ radial  (sigma = 0.1) 
        cost: 1 
        scale: TRUE 
        weights: ('0' = 0.222222222222222, '1' = 1) 
       
      Model info: 
        Features: chr [1:10] "X1" "X2" "X3" "X4" "X5" "X6" "X7" "X8" "X9" ...
        Gap to optimality: 0 
      
      
      $`mildata-qp`
      An mismm object called with mismm.mild_df 
       
      Parameters: 
        method: qp-heuristic 
        kernel: kme w/ radial  (sigma = 0.1) 
        cost: 1 
        scale: TRUE 
        weights: ('0' = 0.222222222222222, '1' = 1) 
       
      Model info: 
        Features: chr [1:10] "X1" "X2" "X3" "X4" "X5" "X6" "X7" "X8" "X9" ...
        Number of iterations: 1 
      
      
      $xy
      An mismm object called with mismm.default 
       
      Parameters: 
        method: heuristic 
        kernel: kme w/ radial  (sigma = 0.1) 
        cost: 1 
        scale: TRUE 
        weights: ('0' = 0.222222222222222, '1' = 1) 
       
      Model info: 
        Features: chr [1:10] "X1" "X2" "X3" "X4" "X5" "X6" "X7" "X8" "X9" ...
        Number of iterations: 2 
      
      
      $formula
      An mismm object called with mismm.formula 
       
      Parameters: 
        method: heuristic 
        kernel: kme w/ radial  (sigma = 0.1) 
        cost: 1 
        scale: TRUE 
        weights: ('0' = 0.222222222222222, '1' = 1) 
       
      Model info: 
        Features: chr [1:10] "X1" "X2" "X3" "X4" "X5" "X6" "X7" "X8" "X9" ...
        Number of iterations: 2 
      
      
      $`no-scale-heur`
      An mismm object called with mismm.mild_df 
       
      Parameters: 
        method: heuristic 
        kernel: kme w/ radial  (sigma = 0.1) 
        cost: 1 
        scale: FALSE 
        weights: ('0' = 0.222222222222222, '1' = 1) 
       
      Model info: 
        Features: chr [1:10] "X1" "X2" "X3" "X4" "X5" "X6" "X7" "X8" "X9" ...
        Number of iterations: 3 
      
      
      $`no-scale-mip`
      An mismm object called with mismm.mild_df 
       
      Parameters: 
        method: mip 
        kernel: kme w/ radial  (sigma = 0.1) 
        cost: 1 
        scale: FALSE 
        weights: ('0' = 0.222222222222222, '1' = 1) 
       
      Model info: 
        Features: chr [1:10] "X1" "X2" "X3" "X4" "X5" "X6" "X7" "X8" "X9" ...
        Gap to optimality: 0 
      
      
      $`no-scale-qp`
      An mismm object called with mismm.mild_df 
       
      Parameters: 
        method: qp-heuristic 
        kernel: kme w/ radial  (sigma = 0.1) 
        cost: 1 
        scale: FALSE 
        weights: ('0' = 0.222222222222222, '1' = 1) 
       
      Model info: 
        Features: chr [1:10] "X1" "X2" "X3" "X4" "X5" "X6" "X7" "X8" "X9" ...
        Number of iterations: 2 
      
      
      $`no-weights`
      An mismm object called with mismm.mild_df 
       
      Parameters: 
        method: heuristic 
        kernel: kme w/ radial  (sigma = 0.1) 
        cost: 1 
        scale: TRUE 
        weights: FALSE 
       
      Model info: 
        Features: chr [1:10] "X1" "X2" "X3" "X4" "X5" "X6" "X7" "X8" "X9" ...
        Number of iterations: 2 
      
      

