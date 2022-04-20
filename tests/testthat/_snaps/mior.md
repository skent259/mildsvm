# mior() internal functions work on simple examples

    Code
      (mzoe <- mean(y[1:300] != y_pred))
    Output
      [1] 0.8866667
    Code
      (mae <- mean(abs(y[1:300] - y_pred)))
    Output
      [1] 1.476667
    Code
      table(y_pred, y[1:300])
    Output
            
      y_pred   1   2   3
           1  23  12 100
           2   0  11   0
           3  77  77   0
    Code
      pROC::multiclass.roc(response = y[1:300], predictor = y_pred)
    Message <simpleMessage>
      Setting direction: controls < cases
      Setting direction: controls > cases
      Setting direction: controls > cases
    Output
      
      Call:
      multiclass.roc.default(response = y[1:300], predictor = y_pred)
      
      Data: y_pred with 3 levels of y[1:300]: 1, 2, 3.
      Multi-class area under the curve: 0.7792

# mior() works for data-frame-like inputs

    Code
      predict(mdl2, new_data = df1, type = "class", layer = "bag")
    Output
      # A tibble: 450 x 1
         .pred_class
         <fct>      
       1 2          
       2 -1         
       3 2          
       4 2          
       5 -1         
       6 -1         
       7 -1         
       8 -1         
       9 2          
      10 2          
      # ... with 440 more rows
    Code
      predict(mdl2, new_data = df1, type = "class", layer = "instance")
    Output
      # A tibble: 450 x 1
         .pred_class
         <fct>      
       1 2          
       2 2          
       3 2          
       4 2          
       5 2          
       6 -1         
       7 -1         
       8 2          
       9 2          
      10 2          
      # ... with 440 more rows
    Code
      predict(mdl2, new_data = df1, type = "raw", layer = "bag")
    Output
      # A tibble: 450 x 1
          .pred
          <dbl>
       1  0.836
       2 -0.226
       3  0.431
       4  1.13 
       5 -1.18 
       6 -0.408
       7 -0.263
       8 -0.722
       9  2.25 
      10  1.32 
      # ... with 440 more rows
    Code
      predict(mdl2, new_data = df1, type = "raw", layer = "instance")
    Output
      # A tibble: 450 x 1
          .pred
          <dbl>
       1  0.836
       2  0.772
       3  8.71 
       4  3.23 
       5  4.09 
       6 -0.408
       7 -0.575
       8  2.58 
       9  4.68 
      10  2.54 
      # ... with 440 more rows

# `mior()` value returns make sense

    Code
      models <- list(xy = mior(x = df1[, 4:6], y = df1$bag_label, bags = df1$bag_name,
      method = "qp-heuristic", weights = NULL), formula = mior(mi(bag_label, bag_name) ~
        V1 + V2, method = "qp-heuristic", data = df1, weights = NULL), `no-scale` = mior(
        x = df1[, 4:6], y = df1$bag_label, bags = df1$bag_name, method = "qp-heuristic",
        weights = NULL, control = list(scale = FALSE))) %>% suppressWarnings() %>%
        suppressMessages()
      print(lapply(models, names))
    Output
      $xy
      [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
      [6] "cost_eta"   "repr_inst"  "n_step"     "x_scale"   
      
      $formula
       [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
       [6] "cost_eta"   "repr_inst"  "n_step"     "x_scale"    "formula"   
      [11] "bag_name"  
      
      $`no-scale`
      [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
      [6] "cost_eta"   "repr_inst"  "n_step"    
      

