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
      Multi-class area under the curve: 0.5
    Code
      print(mzoe)
    Output
      [1] 0.9
    Code
      print(mae)
    Output
      [1] 1.92
    Code
      print(table(true, pred))
    Output
          pred
      true  1
         1 10
         2 34
         3 23
         4 20
         5 13

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.5
    Code
      print(mzoe)
    Output
      [1] 0.83
    Code
      print(mae)
    Output
      [1] 1.75
    Code
      print(table(true, pred))
    Output
          pred
      true  1
         1 17
         2 31
         3 25
         4 14
         5 13

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.5448
    Code
      print(mzoe)
    Output
      [1] 0.83
    Code
      print(mae)
    Output
      [1] 1.57
    Code
      print(table(true, pred))
    Output
          pred
      true  1  2
         1  1  9
         2 18 16
         3 14  9
         4  9 11
         5  5  8

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.5848
    Code
      print(mzoe)
    Output
      [1] 0.8
    Code
      print(mae)
    Output
      [1] 1.46
    Code
      print(table(true, pred))
    Output
          pred
      true  1  2
         1  7 10
         2 18 13
         3 14 11
         4  9  5
         5  3 10

# `omisvm()` value returns make sense

    Code
      models <- list(xy = omisvm(x = df1[, 3:7], y = df1$bag_label, bags = df1$
        bag_name, method = "qp-heuristic", weights = NULL), formula = omisvm(mi(
        bag_label, bag_name) ~ V1 + V2, method = "qp-heuristic", data = df1, weights = NULL),
      `no-scale` = omisvm(x = df1[, 3:7], y = df1$bag_label, bags = df1$bag_name,
      method = "qp-heuristic", weights = NULL, control = list(scale = FALSE))) %>%
        suppressWarnings() %>% suppressMessages()
      print(lapply(models, names))
    Output
      $xy
      [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
      [6] "h"          "repr_inst"  "x_scale"   
      
      $formula
       [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
       [6] "h"          "repr_inst"  "x_scale"    "formula"    "bag_name"  
      
      $`no-scale`
      [1] "gurobi_fit" "call_type"  "features"   "levels"     "cost"      
      [6] "h"          "repr_inst" 
      

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
      Area under the curve: 0.9403

---

    Code
      with(df1_test, suppressWarnings({
        pred <- predict(mdl2, df1_test, type = "raw")$.pred
        pROC::auc(classify_bags(bag_label, bag_name), classify_bags(pred, bag_name))
      }))
    Message <simpleMessage>
      Setting levels: control = 1, case = 2
      Setting direction: controls > cases
    Output
      Area under the curve: 0.5694

