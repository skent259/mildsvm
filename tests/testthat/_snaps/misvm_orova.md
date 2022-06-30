# misvm_orova() has reasonable performance

    Code
      print(mzoe)
    Output
      [1] 0.24
    Code
      print(mae)
    Output
      [1] 0.24
    Code
      print(table(bag_resp, bag_pred))
    Output
              bag_pred
      bag_resp  1  2  3  4  5
             1  2  8  0  0  0
             2  0 34  0  0  0
             3  0  3 11  9  0
             4  0  0  2 17  1
             5  0  0  0  1 12

---

    Code
      print(mzoe)
    Output
      [1] 0.36
    Code
      print(mae)
    Output
      [1] 0.36
    Code
      print(table(bag_resp, bag_pred))
    Output
              bag_pred
      bag_resp  1  2  3  4  5
             1  2 15  0  0  0
             2  0 29  2  0  0
             3  0  2 11 12  0
             4  0  0  2 11  1
             5  0  0  0  2 11

# `misvm_orova()` value returns make sense

    Code
      models <- list(heur = misvm_orova(x = df2[, 3:7], y = df2$bag_label, bags = df2$
        bag_name, method = "heuristic"), qp = misvm_orova(x = df2[, 3:7], y = df2$
        bag_label, bags = df2$bag_name, method = "qp-heuristic"), mip = misvm_orova(
        x = df2[, 3:7], y = df2$bag_label, bags = df2$bag_name, method = "mip"),
      formula = misvm_orova(mi(bag_label, bag_name) ~ V1 + V2, method = "qp-heuristic",
      data = df2), mi_df = misvm_orova(as_mi_df(df2, instance_label = NULL))) %>%
        suppressWarnings() %>% suppressMessages()
      print(lapply(models, names))
    Output
      $heur
      [1] "fits"      "call_type" "levels"    "features"  "kernel"   
      
      $qp
      [1] "fits"      "call_type" "levels"    "features"  "kernel"   
      
      $mip
      [1] "fits"      "call_type" "levels"    "features"  "kernel"   
      
      $formula
      [1] "fits"      "call_type" "levels"    "features"  "kernel"    "formula"  
      [7] "bag_name" 
      
      $mi_df
      [1] "fits"      "call_type" "levels"    "features"  "kernel"    "bag_name" 
      
    Code
      print(models)
    Output
      $heur
      An misvm_orova object called with misvm_orova.default 
       
      Parameters: 
        method: heuristic 
        kernel: linear  
        cost: 1 
        scale: TRUE 
        weights: TRUE 
       
      Model info: 
        Number of models: 5 
        Levels of `y`: chr [1:5] "1" "2" "3" "4" "5"
        Features: chr [1:5] "V1" "V2" "V3" "V4" "V5"
        Number of iterations: 3 2 3 2 3 
      
      
      $qp
      An misvm_orova object called with misvm_orova.default 
       
      Parameters: 
        method: qp-heuristic 
        kernel: linear  
        cost: 1 
        scale: TRUE 
        weights: TRUE 
       
      Model info: 
        Number of models: 5 
        Levels of `y`: chr [1:5] "1" "2" "3" "4" "5"
        Features: chr [1:5] "V1" "V2" "V3" "V4" "V5"
        Number of iterations: 3 2 2 1 0 
      
      
      $mip
      An misvm_orova object called with misvm_orova.default 
       
      Parameters: 
        method: mip 
        kernel: linear  
        cost: 1 
        scale: TRUE 
        weights: TRUE 
       
      Model info: 
        Number of models: 5 
        Levels of `y`: chr [1:5] "1" "2" "3" "4" "5"
        Features: chr [1:5] "V1" "V2" "V3" "V4" "V5"
        Gap to optimality: 0 0 0 0 0 
      
      
      $formula
      An misvm_orova object called with misvm_orova.formula 
       
      Parameters: 
        method: qp-heuristic 
        kernel: linear  
        cost: 1 
        scale: TRUE 
        weights: TRUE 
       
      Model info: 
        Number of models: 5 
        Levels of `y`: chr [1:5] "1" "2" "3" "4" "5"
        Features: chr [1:2] "V1" "V2"
        Number of iterations: 2 2 2 1 1 
      
      
      $mi_df
      An misvm_orova object called with misvm_orova.mi_df 
       
      Parameters: 
        method: heuristic 
        kernel: linear  
        cost: 1 
        scale: TRUE 
        weights: TRUE 
       
      Model info: 
        Number of models: 5 
        Levels of `y`: chr [1:5] "1" "2" "3" "4" "5"
        Features: chr [1:5] "V1" "V2" "V3" "V4" "V5"
        Number of iterations: 3 2 3 2 3 
      
      

