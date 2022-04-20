# Re-ordering data doesn't reduce performance

    Code
      auc1 <- with(mil_df_test, pROC::auc(response = classify_bags(bag_label,
        bag_name), predictor = classify_bags(pred1$.pred, bag_name)))
    Message <simpleMessage>
      Setting levels: control = 0, case = 1
      Setting direction: controls < cases
    Code
      auc2 <- with(mil_df_test, pROC::auc(response = classify_bags(bag_label,
        bag_name), predictor = classify_bags(pred2$.pred, bag_name)))
    Message <simpleMessage>
      Setting levels: control = 0, case = 1
      Setting direction: controls < cases
    Code
      auc1
    Output
      Area under the curve: 1
    Code
      auc2
    Output
      Area under the curve: 1
    Code
      eps <- 0.05

# `smm()` value returns make sense

    Code
      models <- list(xy = smm(x, y, instances), formula = smm(y ~ x1 + x2 + x3, data = df),
      mildata = smm(mil_df), `no-scale-xy` = smm(x, y, instances, control = list(
        scale = FALSE)), `no-scale-mildata` = smm(mil_df, control = list(scale = FALSE)),
      `no-weights-xy` = smm(x, y, instances, weights = FALSE), `no-weights-mildata` = smm(
        mil_df, weights = FALSE)) %>% suppressWarnings() %>% suppressMessages()
      print(lapply(models, names))
    Output
      $xy
      [1] "ksvm_fit"  "call_type" "x"         "features"  "levels"    "cost"     
      [7] "sigma"     "weights"   "x_scale"  
      
      $formula
       [1] "ksvm_fit"      "call_type"     "x"             "features"     
       [5] "levels"        "cost"          "sigma"         "weights"      
       [9] "x_scale"       "formula"       "instance_name"
      
      $mildata
       [1] "ksvm_fit"      "call_type"     "x"             "features"     
       [5] "levels"        "cost"          "sigma"         "weights"      
       [9] "x_scale"       "bag_name"      "instance_name"
      
      $`no-scale-xy`
      [1] "ksvm_fit"  "call_type" "x"         "features"  "levels"    "cost"     
      [7] "sigma"     "weights"  
      
      $`no-scale-mildata`
       [1] "ksvm_fit"      "call_type"     "x"             "features"     
       [5] "levels"        "cost"          "sigma"         "weights"      
       [9] "bag_name"      "instance_name"
      
      $`no-weights-xy`
      [1] "ksvm_fit"  "call_type" "x"         "features"  "levels"    "cost"     
      [7] "sigma"     "x_scale"  
      
      $`no-weights-mildata`
       [1] "ksvm_fit"      "call_type"     "x"             "features"     
       [5] "levels"        "cost"          "sigma"         "x_scale"      
       [9] "bag_name"      "instance_name"
      

