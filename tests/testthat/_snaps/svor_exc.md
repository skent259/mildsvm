# svor_exc() internal functions work on simple examples

    Code
      table(y, y_pred)
    Output
         y_pred
      y     1   2   3   4   5
        1 168   7   3   0   0
        2  33  22  28   0   0
        3   3   8  38   8   1
        4   0   0  15   7  13
        5   0   0   2   7  37
    Code
      pROC::multiclass.roc(response = y, predictor = f) %>% suppressMessages()
    Output
      
      Call:
      multiclass.roc.default(response = y, predictor = f)
      
      Data: f with 5 levels of y: 1, 2, 3, 4, 5.
      Multi-class area under the curve: 0.9323
    Code
      mzoe <- mean(y != y_pred)
      mae <- mean(y - y_pred)
      mzoe
    Output
      [1] 0.32
    Code
      mae
    Output
      [1] 0.0225

# svor_exc() has reasonable performance

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.958
    Code
      print(mzoe)
    Output
      [1] 0.14
    Code
      print(mae)
    Output
      [1] 0.14

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.9527
    Code
      print(mzoe)
    Output
      [1] 0.156
    Code
      print(mae)
    Output
      [1] 0.156

# `svor_exc()` value returns make sense

    Code
      models <- list(xy = svor_exc(x = df1[, 2:6], y = df1$y, weights = NULL),
      formula = svor_exc(y ~ V1 + V2, data = df1, weights = NULL), `no-scale` = svor_exc(
        x = df1[, 2:6], y = df1$y, weights = NULL, control = list(scale = FALSE))) %>%
        suppressWarnings() %>% suppressMessages()
      print(lapply(models, names))
    Output
      $xy
      [1] "smo_fit"   "call_type" "x"         "features"  "levels"    "cost"     
      [7] "n_step"    "x_scale"  
      
      $formula
      [1] "smo_fit"   "call_type" "x"         "features"  "levels"    "cost"     
      [7] "n_step"    "x_scale"   "formula"  
      
      $`no-scale`
      [1] "smo_fit"   "call_type" "x"         "features"  "levels"    "cost"     
      [7] "n_step"   
      

