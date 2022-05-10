# svor_exc() internal functions work on simple examples

    Code
      table(y, y_pred)
    Output
         y_pred
      y    1  2  3  5
        1 11  4  0  0
        2  9 28  3  0
        3  0  6 34  0
        4  0  0 15 20
        5  0  0  0 20
    Code
      pROC::multiclass.roc(response = y, predictor = f) %>% suppressMessages()
    Output
      
      Call:
      multiclass.roc.default(response = y, predictor = f)
      
      Data: f with 5 levels of y: 1, 2, 3, 4, 5.
      Multi-class area under the curve: 0.9671
    Code
      mzoe <- mean(y != y_pred)
      mae <- mean(y - y_pred)
      mzoe
    Output
      [1] 0.38
    Code
      mae
    Output
      [1] 0.02

# svor_exc() has reasonable performance

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.9394
    Code
      print(mzoe)
    Output
      [1] 0.1933333
    Code
      print(mae)
    Output
      [1] 0.1933333

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.9498
    Code
      print(mzoe)
    Output
      [1] 0.2070588
    Code
      print(mae)
    Output
      [1] 0.2070588

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
      

