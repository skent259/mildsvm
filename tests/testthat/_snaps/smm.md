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
       [1] "ksvm_fit"     "call_type"    "x"            "features"     "levels"      
       [6] "cost"         "sigma"        "weights"      "kernel"       "kernel_param"
      [11] "x_scale"     
      
      $formula
       [1] "ksvm_fit"      "call_type"     "x"             "features"     
       [5] "levels"        "cost"          "sigma"         "weights"      
       [9] "kernel"        "kernel_param"  "x_scale"       "formula"      
      [13] "instance_name"
      
      $mildata
       [1] "ksvm_fit"      "call_type"     "x"             "features"     
       [5] "levels"        "cost"          "sigma"         "weights"      
       [9] "kernel"        "kernel_param"  "x_scale"       "bag_name"     
      [13] "instance_name"
      
      $`no-scale-xy`
       [1] "ksvm_fit"     "call_type"    "x"            "features"     "levels"      
       [6] "cost"         "sigma"        "weights"      "kernel"       "kernel_param"
      
      $`no-scale-mildata`
       [1] "ksvm_fit"      "call_type"     "x"             "features"     
       [5] "levels"        "cost"          "sigma"         "weights"      
       [9] "kernel"        "kernel_param"  "bag_name"      "instance_name"
      
      $`no-weights-xy`
       [1] "ksvm_fit"     "call_type"    "x"            "features"     "levels"      
       [6] "cost"         "sigma"        "kernel"       "kernel_param" "x_scale"     
      
      $`no-weights-mildata`
       [1] "ksvm_fit"      "call_type"     "x"             "features"     
       [5] "levels"        "cost"          "sigma"         "kernel"       
       [9] "kernel_param"  "x_scale"       "bag_name"      "instance_name"
      
    Code
      print(models)
    Output
      $xy
      A smm object called with smm.default 
       
      Parameters: 
        kernel: kme w/ radial  (sigma = 0.3333333) 
        cost: 1 
        scale: TRUE 
        weights: ('-1' = 1, '1' = 1) 
       
      Model info: 
        Features: chr [1:3] "x1" "x2" "x3"
      
      
      $formula
      A smm object called with smm.formula 
       
      Parameters: 
        kernel: kme w/ radial  (sigma = 0.3333333) 
        cost: 1 
        scale: TRUE 
        weights: ('-1' = 1, '1' = 1) 
       
      Model info: 
        Features: chr [1:3] "x1" "x2" "x3"
      
      
      $mildata
      A smm object called with smm.mild_df 
       
      Parameters: 
        kernel: kme w/ radial  (sigma = 0.1) 
        cost: 1 
        scale: TRUE 
        weights: ('0' = 1.5, '1' = 1) 
       
      Model info: 
        Features: chr [1:10] "X1" "X2" "X3" "X4" "X5" "X6" "X7" "X8" "X9" ...
      
      
      $`no-scale-xy`
      A smm object called with smm.default 
       
      Parameters: 
        kernel: kme w/ radial  (sigma = 0.3333333) 
        cost: 1 
        scale: FALSE 
        weights: ('-1' = 1, '1' = 1) 
       
      Model info: 
        Features: chr [1:3] "x1" "x2" "x3"
      
      
      $`no-scale-mildata`
      A smm object called with smm.mild_df 
       
      Parameters: 
        kernel: kme w/ radial  (sigma = 0.1) 
        cost: 1 
        scale: FALSE 
        weights: ('0' = 1.5, '1' = 1) 
       
      Model info: 
        Features: chr [1:10] "X1" "X2" "X3" "X4" "X5" "X6" "X7" "X8" "X9" ...
      
      
      $`no-weights-xy`
      A smm object called with smm.default 
       
      Parameters: 
        kernel: kme w/ radial  (sigma = 0.3333333) 
        cost: 1 
        scale: TRUE 
        weights: FALSE 
       
      Model info: 
        Features: chr [1:3] "x1" "x2" "x3"
      
      
      $`no-weights-mildata`
      A smm object called with smm.mild_df 
       
      Parameters: 
        kernel: kme w/ radial  (sigma = 0.1) 
        cost: 1 
        scale: TRUE 
        weights: FALSE 
       
      Model info: 
        Features: chr [1:10] "X1" "X2" "X3" "X4" "X5" "X6" "X7" "X8" "X9" ...
      
      

