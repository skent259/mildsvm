# omisvm() has reasonable performance

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.9515
    Code
      print(mzoe)
    Output
      [1] 0.2
    Code
      print(mae)
    Output
      [1] 0.2
    Code
      print(table(true, pred))
    Output
          pred
      true  1  2  3  4  5
         1  9  1  0  0  0
         2  1 32  1  0  0
         3  0  7 12  4  0
         4  0  0  2 16  2
         5  0  0  0  2 11

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.9333
    Code
      print(mzoe)
    Output
      [1] 0.28
    Code
      print(mae)
    Output
      [1] 0.28
    Code
      print(table(true, pred))
    Output
          pred
      true  1  2  3  4  5
         1 11  6  0  0  0
         2  2 26  3  0  0
         3  0  9 14  2  0
         4  0  0  5  9  0
         5  0  0  0  1 12

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.8741
    Code
      print(mzoe)
    Output
      [1] 0.32
    Code
      print(mae)
    Output
      [1] 0.32
    Code
      print(table(true, pred))
    Output
          pred
      true  1  2  3  4
         1  6  4  0  0
         2  0 33  1  0
         3  0  4  9 10
         4  0  0  0 20
         5  0  0  0 13

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.8392
    Code
      print(mzoe)
    Output
      [1] 0.48
    Code
      print(mae)
    Output
      [1] 0.5
    Code
      print(table(true, pred))
    Output
          pred
      true  1  2  3  4
         1  4 13  0  0
         2  1 27  1  2
         3  0  8  8  9
         4  0  0  1 13
         5  0  0  0 13

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.8753
    Code
      print(mzoe)
    Output
      [1] 0.32
    Code
      print(mae)
    Output
      [1] 0.32
    Code
      print(table(true, pred))
    Output
          pred
      true  1  2  3  4
         1  6  4  0  0
         2  0 33  1  0
         3  0  5  9  9
         4  0  0  0 20
         5  0  0  0 13

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.8426
    Code
      print(mzoe)
    Output
      [1] 0.48
    Code
      print(mae)
    Output
      [1] 0.5
    Code
      print(table(true, pred))
    Output
          pred
      true  1  2  3  4
         1  4 13  0  0
         2  2 26  1  2
         3  0  9  9  7
         4  0  0  1 13
         5  0  0  0 13

# `omisvm()` value returns make sense

    Code
      models <- list(xy = .run_omisvm(df1, weights = NULL), formula = omisvm(mi(
        bag_label, bag_name) ~ V1 + V2, data = df1, weights = NULL), mi_df = omisvm(
        as_mi_df(df1, instance_label = NULL)), `no-scale` = .run_omisvm(df1, weights = NULL,
        control = list(scale = FALSE))) %>% suppressWarnings() %>% suppressMessages()
      print(lapply(models, names))
    Output
      $xy
       [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
       [6] "h"          "s"          "kernel"     "repr_inst"  "x_scale"   
      
      $formula
       [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
       [6] "h"          "s"          "kernel"     "repr_inst"  "x_scale"   
      [11] "formula"    "bag_name"  
      
      $mi_df
       [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
       [6] "h"          "s"          "kernel"     "repr_inst"  "x_scale"   
      [11] "bag_name"  
      
      $`no-scale`
      [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
      [6] "h"          "s"          "kernel"     "repr_inst" 
      
    Code
      print(models)
    Output
      $xy
      An misvm object called with misvm.default 
       
      Parameters: 
        method: qp-heuristic 
        kernel: linear  
        cost: 1 
        h: 1 
        s: 2 
        scale: TRUE 
        weights: FALSE 
       
      Model info: 
        Levels of `y`: chr [1:3] "1" "2" "3"
        Features: chr [1:5] "V1" "V2" "V3" "V4" "V5"
        Number of iterations: 2 
      
      
      $formula
      An misvm object called with omisvm.formula 
       
      Parameters: 
        method: qp-heuristic 
        kernel: linear  
        cost: 1 
        h: 1 
        s: 2 
        scale: TRUE 
        weights: FALSE 
       
      Model info: 
        Levels of `y`: chr [1:3] "1" "2" "3"
        Features: chr [1:2] "V1" "V2"
        Number of iterations: 1 
      
      
      $mi_df
      An misvm object called with omisvm.mi_df 
       
      Parameters: 
        method: qp-heuristic 
        kernel: linear  
        cost: 1 
        h: 1 
        s: 2 
        scale: TRUE 
        weights: FALSE 
       
      Model info: 
        Levels of `y`: chr [1:3] "1" "2" "3"
        Features: chr [1:5] "V1" "V2" "V3" "V4" "V5"
        Number of iterations: 2 
      
      
      $`no-scale`
      An misvm object called with misvm.default 
       
      Parameters: 
        method: qp-heuristic 
        kernel: linear  
        cost: 1 
        h: 1 
        s: 2 
        scale: FALSE 
        weights: FALSE 
       
      Model info: 
        Levels of `y`: chr [1:3] "1" "2" "3"
        Features: chr [1:5] "V1" "V2" "V3" "V4" "V5"
        Number of iterations: 3 
      
      

# Ordering of data doesn't change `omisvm()` results

    Code
      with(df1_test, suppressWarnings({
        pred <- predict(mdl2, df1_test, type = "raw")$.pred
        pROC::auc(classify_bags(bag_label, bag_name), classify_bags(pred, bag_name))
      }))
    Message <simpleMessage>
      Setting levels: control = 1, case = 2
      Setting direction: controls < cases
    Output
      Area under the curve: 0.9335

