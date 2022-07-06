# `mior()` has reasonable performance

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.6775
    Code
      print(mzoe)
    Output
      [1] 0.8466667
    Code
      print(mae)
    Output
      [1] 1.373333
    Code
      print(table(true, pred))
    Output
          pred
      true  1  3
         1 18 30
         2 24 24
         3 49  5

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.5973
    Code
      print(mzoe)
    Output
      [1] 0.76
    Code
      print(mae)
    Output
      [1] 1.173333
    Code
      print(table(true, pred))
    Output
          pred
      true  1  3
         1 22 30
         2 21 31
         3 32 14

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.6057
    Code
      print(mzoe)
    Output
      [1] 0.7
    Code
      print(mae)
    Output
      [1] 0.7733333
    Code
      print(table(true, pred))
    Output
          pred
      true  1  2
         1 22 26
         2 25 23
         3 11 43

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.5268
    Code
      print(mzoe)
    Output
      [1] 0.68
    Code
      print(mae)
    Output
      [1] 0.7733333
    Code
      print(table(true, pred))
    Output
          pred
      true  1  2
         1 20 32
         2 24 28
         3 14 32

# `mior()` works for data-frame-like inputs

    Code
      predict(mdl2, new_data = df1, type = "class", layer = "bag")
    Output
      # A tibble: 450 x 1
         .pred_class
         <fct>      
       1 1          
       2 1          
       3 2          
       4 1          
       5 2          
       6 1          
       7 2          
       8 1          
       9 1          
      10 1          
      # ... with 440 more rows
    Code
      predict(mdl2, new_data = df1, type = "class", layer = "instance")
    Output
      # A tibble: 450 x 1
         .pred_class
         <fct>      
       1 1          
       2 1          
       3 1          
       4 1          
       5 1          
       6 1          
       7 1          
       8 1          
       9 1          
      10 1          
      # ... with 440 more rows
    Code
      predict(mdl2, new_data = df1, type = "raw", layer = "bag")
    Output
      # A tibble: 450 x 1
          .pred
          <dbl>
       1  1.84 
       2  0.455
       3 -1.56 
       4  0.566
       5 -2.61 
       6  0.622
       7 -1.13 
       8  2.78 
       9  0.403
      10  2.48 
      # ... with 440 more rows
    Code
      predict(mdl2, new_data = df1, type = "raw", layer = "instance")
    Output
      # A tibble: 450 x 1
          .pred
          <dbl>
       1  1.84 
       2 10.9  
       3  3.44 
       4  0.566
       5  2.38 
       6  5.63 
       7  2.85 
       8  2.78 
       9  0.681
      10  2.48 
      # ... with 440 more rows

# `mior()` value returns make sense

    Code
      models <- list(xy = mior(x = df1[, 4:6], y = df1$bag_label, bags = df1$bag_name,
      method = "qp-heuristic", weights = NULL), formula = mior(mi(bag_label, bag_name) ~
        V1 + V2, method = "qp-heuristic", data = df1, weights = NULL), mi_df = mior(
        as_mi_df(df1, instance_label = NULL)), `no-scale` = mior(x = df1[, 4:6], y = df1$
        bag_label, bags = df1$bag_name, method = "qp-heuristic", weights = NULL,
      control = list(scale = FALSE))) %>% suppressWarnings() %>% suppressMessages()
      print(lapply(models, names))
    Output
      $xy
       [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
       [6] "cost_eta"   "kernel"     "repr_inst"  "n_step"     "x_scale"   
      
      $formula
       [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
       [6] "cost_eta"   "kernel"     "repr_inst"  "n_step"     "x_scale"   
      [11] "formula"    "bag_name"  
      
      $mi_df
       [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
       [6] "cost_eta"   "kernel"     "repr_inst"  "n_step"     "x_scale"   
      [11] "bag_name"  
      
      $`no-scale`
      [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
      [6] "cost_eta"   "kernel"     "repr_inst"  "n_step"    
      
    Code
      print(models)
    Output
      $xy
      An mior object called with mior.default 
       
      Parameters: 
        method: qp-heuristic 
        kernel: linear  
        cost: 1 
        cost_eta: 1 
        scale: TRUE 
        weights: FALSE 
       
      Model info: 
        Levels of `y`: chr [1:3] "1" "2" "3"
        Features: chr [1:3] "V1" "V2" "V3"
        Number of iterations: 4 
        Gap to optimality: 
      
      
      $formula
      An mior object called with mior.formula 
       
      Parameters: 
        method: qp-heuristic 
        kernel: linear  
        cost: 1 
        cost_eta: 1 
        scale: TRUE 
        weights: FALSE 
       
      Model info: 
        Levels of `y`: chr [1:3] "1" "2" "3"
        Features: chr [1:2] "V1" "V2"
        Number of iterations: 5 
        Gap to optimality: 
      
      
      $mi_df
      An mior object called with mior.mi_df 
       
      Parameters: 
        method: qp-heuristic 
        kernel: linear  
        cost: 1 
        cost_eta: 1 
        scale: TRUE 
        weights: FALSE 
       
      Model info: 
        Levels of `y`: chr [1:3] "1" "2" "3"
        Features: chr [1:4] "repr" "V1" "V2" "V3"
        Number of iterations: 4 
        Gap to optimality: 
      
      
      $`no-scale`
      An mior object called with mior.default 
       
      Parameters: 
        method: qp-heuristic 
        kernel: linear  
        cost: 1 
        cost_eta: 1 
        scale: FALSE 
        weights: FALSE 
       
      Model info: 
        Levels of `y`: chr [1:3] "1" "2" "3"
        Features: chr [1:3] "V1" "V2" "V3"
        Number of iterations: 4 
        Gap to optimality: 
      
      

