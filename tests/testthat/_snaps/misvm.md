# `misvm()` value returns make sense

    Code
      models <- list(`xy-heur` = misvm(x = df1[, 3:122], y = df1$bag_label, bags = df1$
        bag_name, method = "heuristic"), `xy-mip` = misvm(x = df1[, 3:122], y = df1$
        bag_label, bags = df1$bag_name, method = "mip"), `xy-qp` = misvm(x = df1[, 3:
        122], y = df1$bag_label, bags = df1$bag_name, method = "qp-heuristic"),
      formula = misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, method = "heuristic",
      data = df1), mi_df = misvm(as_mi_df(df1, instance_label = NULL)), mildata = misvm(
        mil_data), `no-scale-heur` = misvm(x = df1[, 3:122], y = df1$bag_label, bags = df1$
        bag_name, method = "heuristic", control = list(scale = FALSE)),
      `no-scale-mip` = misvm(x = df1[, 3:122], y = df1$bag_label, bags = df1$bag_name,
      method = "mip", control = list(scale = FALSE)), `no-scale-qp` = misvm(x = df1[,
        3:122], y = df1$bag_label, bags = df1$bag_name, method = "qp-heuristic",
      control = list(scale = FALSE)), kfm_fit = misvm(mi(bag_label, bag_name) ~
        X1_mean + X2_mean, data = df1, method = "mip", control = list(kernel = "radial")),
      `no-weights-heur` = misvm(x = df1[, 3:122], y = df1$bag_label, bags = df1$
        bag_name, method = "heuristic", weights = FALSE), `no-weights-mildata` = misvm(
        mil_data)) %>% suppressWarnings() %>% suppressMessages()
      print(lapply(models, names))
    Output
      $`xy-heur`
       [1] "svm_fit"   "call_type" "x"         "features"  "levels"    "cost"     
       [7] "weights"   "repr_inst" "n_step"    "x_scale"  
      
      $`xy-mip`
      [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
      [6] "weights"    "x_scale"   
      
      $`xy-qp`
      [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
      [6] "weights"    "repr_inst"  "n_step"     "x_scale"   
      
      $formula
       [1] "svm_fit"   "call_type" "x"         "features"  "levels"    "cost"     
       [7] "weights"   "repr_inst" "n_step"    "x_scale"   "formula"   "bag_name" 
      
      $mi_df
       [1] "svm_fit"   "call_type" "x"         "features"  "levels"    "cost"     
       [7] "weights"   "repr_inst" "n_step"    "x_scale"   "bag_name" 
      
      $mildata
       [1] "svm_fit"       "call_type"     "x"             "features"     
       [5] "levels"        "cost"          "weights"       "repr_inst"    
       [9] "n_step"        "x_scale"       "bag_name"      "instance_name"
      [13] "summary_fns"   "summary_cor"  
      
      $`no-scale-heur`
      [1] "svm_fit"   "call_type" "features"  "levels"    "cost"      "weights"  
      [7] "repr_inst" "n_step"   
      
      $`no-scale-mip`
      [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
      [6] "weights"   
      
      $`no-scale-qp`
      [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
      [6] "weights"    "repr_inst"  "n_step"    
      
      $kfm_fit
       [1] "gurobi_fit" "kfm_fit"    "call_type"  "features"   "levels"    
       [6] "cost"       "weights"    "x_scale"    "formula"    "bag_name"  
      
      $`no-weights-heur`
      [1] "svm_fit"   "call_type" "x"         "features"  "levels"    "cost"     
      [7] "repr_inst" "n_step"    "x_scale"  
      
      $`no-weights-mildata`
       [1] "svm_fit"       "call_type"     "x"             "features"     
       [5] "levels"        "cost"          "weights"       "repr_inst"    
       [9] "n_step"        "x_scale"       "bag_name"      "instance_name"
      [13] "summary_fns"   "summary_cor"  
      

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

