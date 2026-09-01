# svor_exc() internal functions work on simple examples

    Code
      table(y, y_pred)
    Output
         y_pred
      y     1   2   3   4   5
        1 103   0   0   0   0
        2  13   8   1   0   0
        3   1   2   8   1   0
        4   0   0   1   7   1
        5   0   0   0   2   2
    Code
      pROC::multiclass.roc(response = y, predictor = f) %>% suppressMessages()
    Output
      
      Call:
      multiclass.roc.default(response = y, predictor = f)
      
      Data: f with 5 levels of y: 1, 2, 3, 4, 5.
      Multi-class area under the curve: 0.9833
    Code
      mzoe <- mean(y != y_pred)
      mae <- mean(y - y_pred)
      mzoe
    Output
      [1] 0.1466667
    Code
      mae
    Output
      [1] 0.1133333

# svor_exc() has reasonable performance

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.9509
    Code
      print(mzoe)
    Output
      [1] 0.1066667
    Code
      print(mae)
    Output
      [1] 0.1066667

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.9491
    Code
      print(mzoe)
    Output
      [1] 0.1235294
    Code
      print(mae)
    Output
      [1] 0.1235294

# `svor_exc()` value returns make sense

    Code
      models <- list(xy = svor_exc(x = df1[, 2:6], y = df1$y, weights = NULL),
      formula = svor_exc(y ~ V1 + V2, data = df1, weights = NULL), mi_df = svor_exc(
        as_mi_df(df2, bag_label = "y", instance_label = NULL)), `no-scale` = svor_exc(
        x = df1[, 2:6], y = df1$y, weights = NULL, control = list(scale = FALSE))) %>%
        suppressWarnings() %>% suppressMessages()
      print(lapply(models, names))
    Output
      $xy
      [1] "smo_fit"   "call_type" "x"         "features"  "levels"    "cost"     
      [7] "kernel"    "n_step"    "x_scale"  
      
      $formula
       [1] "smo_fit"   "call_type" "x"         "features"  "levels"    "cost"     
       [7] "kernel"    "n_step"    "x_scale"   "formula"  
      
      $mi_df
       [1] "smo_fit"   "call_type" "x"         "features"  "levels"    "cost"     
       [7] "kernel"    "n_step"    "x_scale"   "bag_name" 
      
      $`no-scale`
      [1] "smo_fit"   "call_type" "x"         "features"  "levels"    "cost"     
      [7] "kernel"    "n_step"   
      
    Code
      print(models)
    Output
      $xy
      An svor_exc object called with svor_exc.default 
       
      Parameters: 
        method: smo 
        kernel: linear  
        cost: 1 
        scale: TRUE 
        weights: FALSE 
       
      Model info: 
        Levels of `y`: chr [1:5] "1" "2" "3" "4" "5"
        Features: chr [1:5] "V1" "V2" "V3" "V4" "V5"
        Number of iterations: 500 
      
      
      $formula
      An svor_exc object called with svor_exc.formula 
       
      Parameters: 
        method: smo 
        kernel: linear  
        cost: 1 
        scale: TRUE 
        weights: FALSE 
       
      Model info: 
        Levels of `y`: chr [1:5] "1" "2" "3" "4" "5"
        Features: chr [1:2] "V1" "V2"
        Number of iterations: 146 
      
      
      $mi_df
      An svor_exc object called with svor_exc.mi_df 
       
      Parameters: 
        method: smo 
        kernel: linear  
        cost: 1 
        scale: TRUE 
        weights: FALSE 
       
      Model info: 
        Levels of `y`: chr [1:5] "1" "2" "3" "4" "5"
        Features: chr [1:5] "V1" "V2" "V3" "V4" "V5"
        Number of iterations: 500 
      
      
      $`no-scale`
      An svor_exc object called with svor_exc.default 
       
      Parameters: 
        method: smo 
        kernel: linear  
        cost: 1 
        scale: FALSE 
        weights: FALSE 
       
      Model info: 
        Levels of `y`: chr [1:5] "1" "2" "3" "4" "5"
        Features: chr [1:5] "V1" "V2" "V3" "V4" "V5"
        Number of iterations: 500 
      
      

