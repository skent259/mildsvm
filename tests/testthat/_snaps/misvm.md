# `misvm()` value returns make sense

    Code
      models <- list(`xy-heur` = run_misvm(method = "heuristic"), `xy-mip` = run_misvm(
        method = "mip"), `xy-qp` = run_misvm(method = "qp-heuristic"), formula = misvm(
        mi(bag_label, bag_name) ~ X1_mean + X2_mean, method = "heuristic", data = df1),
      mi_df = misvm(as_mi_df(df1, instance_label = NULL)), mildata = misvm(mil_data),
      `no-scale-heur` = run_misvm(method = "heuristic", control = list(scale = FALSE)),
      `no-scale-mip` = run_misvm(method = "mip", control = list(scale = FALSE)),
      `no-scale-qp` = run_misvm(method = "qp-heuristic", control = list(scale = FALSE)),
      kfm_fit = misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, data = df1,
      method = "mip", control = list(kernel = "radial")), `no-weights-heur` = run_misvm(
        method = "heuristic", weights = FALSE), `no-weights-mildata` = misvm(mil_data)) %>%
        suppressWarnings() %>% suppressMessages()
      print(lapply(models, names))
    Output
      $`xy-heur`
       [1] "svm_fit"   "call_type" "x"         "features"  "levels"    "cost"     
       [7] "weights"   "kernel"    "repr_inst" "n_step"    "x_scale"  
      
      $`xy-mip`
      [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
      [6] "weights"    "kernel"     "x_scale"   
      
      $`xy-qp`
       [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
       [6] "weights"    "kernel"     "repr_inst"  "n_step"     "x_scale"   
      
      $formula
       [1] "svm_fit"   "call_type" "x"         "features"  "levels"    "cost"     
       [7] "weights"   "kernel"    "repr_inst" "n_step"    "x_scale"   "formula"  
      [13] "bag_name" 
      
      $mi_df
       [1] "svm_fit"   "call_type" "x"         "features"  "levels"    "cost"     
       [7] "weights"   "kernel"    "repr_inst" "n_step"    "x_scale"   "bag_name" 
      
      $mildata
       [1] "svm_fit"       "call_type"     "x"             "features"     
       [5] "levels"        "cost"          "weights"       "kernel"       
       [9] "repr_inst"     "n_step"        "x_scale"       "bag_name"     
      [13] "instance_name" "summary_fns"   "summary_cor"  
      
      $`no-scale-heur`
      [1] "svm_fit"   "call_type" "features"  "levels"    "cost"      "weights"  
      [7] "kernel"    "repr_inst" "n_step"   
      
      $`no-scale-mip`
      [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
      [6] "weights"    "kernel"    
      
      $`no-scale-qp`
      [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
      [6] "weights"    "kernel"     "repr_inst"  "n_step"    
      
      $kfm_fit
       [1] "gurobi_fit"   "kfm_fit"      "call_type"    "features"     "levels"      
       [6] "cost"         "weights"      "kernel"       "kernel_param" "x_scale"     
      [11] "formula"      "bag_name"    
      
      $`no-weights-heur`
       [1] "svm_fit"   "call_type" "x"         "features"  "levels"    "cost"     
       [7] "kernel"    "repr_inst" "n_step"    "x_scale"  
      
      $`no-weights-mildata`
       [1] "svm_fit"       "call_type"     "x"             "features"     
       [5] "levels"        "cost"          "weights"       "kernel"       
       [9] "repr_inst"     "n_step"        "x_scale"       "bag_name"     
      [13] "instance_name" "summary_fns"   "summary_cor"  
      
    Code
      print(models)
    Output
      $`xy-heur`
      An misvm object called with misvm.default 
       
      Parameters: 
        method: heuristic 
        kernel: linear  
        cost: 1 
        scale: TRUE 
        weights: ('0' = 0.375, '1' = 1) 
       
      Model info: 
        Features: chr [1:120] "X1_0.05" "X1_0.15" "X1_0.25" "X1_0.35" ...
        Number of iterations: 2 
      
      
      $`xy-mip`
      An misvm object called with misvm.default 
       
      Parameters: 
        method: mip 
        kernel: linear  
        cost: 1 
        scale: TRUE 
        weights: ('0' = 0.375, '1' = 1) 
       
      Model info: 
        Features: chr [1:120] "X1_0.05" "X1_0.15" "X1_0.25" "X1_0.35" ...
        Gap to optimality: 0 
      
      
      $`xy-qp`
      An misvm object called with misvm.default 
       
      Parameters: 
        method: qp-heuristic 
        kernel: linear  
        cost: 1 
        scale: TRUE 
        weights: ('0' = 0.375, '1' = 1) 
       
      Model info: 
        Features: chr [1:120] "X1_0.05" "X1_0.15" "X1_0.25" "X1_0.35" ...
        Number of iterations: 2 
      
      
      $formula
      An misvm object called with misvm.formula 
       
      Parameters: 
        method: heuristic 
        kernel: linear  
        cost: 1 
        scale: TRUE 
        weights: ('0' = 0.375, '1' = 1) 
       
      Model info: 
        Features: chr [1:2] "X1_mean" "X2_mean"
        Number of iterations: 2 
      
      
      $mi_df
      An misvm object called with misvm.mi_df 
       
      Parameters: 
        method: heuristic 
        kernel: linear  
        cost: 1 
        scale: TRUE 
        weights: ('0' = 0.375, '1' = 1) 
       
      Model info: 
        Features: chr [1:120] "X1_0.05" "X1_0.15" "X1_0.25" "X1_0.35" ...
        Number of iterations: 2 
      
      
      $mildata
      An misvm object called with misvm.mild_df 
       
      Parameters: 
        method: heuristic 
        kernel: linear  
        cost: 1 
        scale: TRUE 
        weights: ('0' = 0.375, '1' = 1) 
       
      Model info: 
        Features: chr [1:20] "X1_mean" "X2_mean" "X3_mean" "X4_mean" "X5_mean" ...
        Number of iterations: 2 
      
      
      $`no-scale-heur`
      An misvm object called with misvm.default 
       
      Parameters: 
        method: heuristic 
        kernel: linear  
        cost: 1 
        scale: FALSE 
        weights: ('0' = 0.375, '1' = 1) 
       
      Model info: 
        Features: chr [1:120] "X1_0.05" "X1_0.15" "X1_0.25" "X1_0.35" ...
        Number of iterations: 3 
      
      
      $`no-scale-mip`
      An misvm object called with misvm.default 
       
      Parameters: 
        method: mip 
        kernel: linear  
        cost: 1 
        scale: FALSE 
        weights: ('0' = 0.375, '1' = 1) 
       
      Model info: 
        Features: chr [1:120] "X1_0.05" "X1_0.15" "X1_0.25" "X1_0.35" ...
        Gap to optimality: 0 
      
      
      $`no-scale-qp`
      An misvm object called with misvm.default 
       
      Parameters: 
        method: qp-heuristic 
        kernel: linear  
        cost: 1 
        scale: FALSE 
        weights: ('0' = 0.375, '1' = 1) 
       
      Model info: 
        Features: chr [1:120] "X1_0.05" "X1_0.15" "X1_0.25" "X1_0.35" ...
        Number of iterations: 3 
      
      
      $kfm_fit
      An misvm object called with misvm.formula 
       
      Parameters: 
        method: mip 
        kernel: radial  (sigma = 0.5) 
        cost: 1 
        scale: TRUE 
        weights: ('0' = 0.375, '1' = 1) 
       
      Model info: 
        Features: chr [1:2] "X1_mean" "X2_mean"
        Gap to optimality: 0 
      
      
      $`no-weights-heur`
      An misvm object called with misvm.default 
       
      Parameters: 
        method: heuristic 
        kernel: linear  
        cost: 1 
        scale: TRUE 
        weights: FALSE 
       
      Model info: 
        Features: chr [1:120] "X1_0.05" "X1_0.15" "X1_0.25" "X1_0.35" ...
        Number of iterations: 2 
      
      
      $`no-weights-mildata`
      An misvm object called with misvm.mild_df 
       
      Parameters: 
        method: heuristic 
        kernel: linear  
        cost: 1 
        scale: TRUE 
        weights: ('0' = 0.375, '1' = 1) 
       
      Model info: 
        Features: chr [1:20] "X1_mean" "X2_mean" "X3_mean" "X4_mean" "X5_mean" ...
        Number of iterations: 2 
      
      

# Ordering of data doesn't change `misvm()` results

    Code
      with(df1_test, {
        pred <- predict(mdl2, df1_test, type = "raw")$.pred
        pROC::auc(classify_bags(bag_label, bag_name), classify_bags(pred, bag_name))
      })
    Message <simpleMessage>
      Setting levels: control = 0, case = 1
      Setting direction: controls < cases
    Output
      Area under the curve: 1

---

    Code
      with(df1_test, {
        pred <- predict(mdl2, df1_test, type = "raw")$.pred
        pROC::auc(classify_bags(bag_label, bag_name), classify_bags(pred, bag_name))
      })
    Message <simpleMessage>
      Setting levels: control = 0, case = 1
      Setting direction: controls < cases
    Output
      Area under the curve: 1

---

    Code
      with(mil_data_test, {
        pred <- predict(mdl2, mil_data_test, type = "raw")$.pred
        pROC::auc(classify_bags(bag_label, bag_name), classify_bags(pred, bag_name))
      })
    Message <simpleMessage>
      Setting levels: control = 0, case = 1
      Setting direction: controls < cases
    Output
      Area under the curve: 1

