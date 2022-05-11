# omisvm() has reasonable performance

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.9826
    Code
      print(mzoe)
    Output
      [1] 0.06
    Code
      print(mae)
    Output
      [1] 0.06
    Code
      print(table(bag_resp, bag_pred))
    Output
              bag_pred
      bag_resp  1  2  3  4  5
             1  9  1  0  0  0
             2  1 33  0  0  0
             3  0  0 22  1  0
             4  0  0  0 18  2
             5  0  0  0  1 12

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.9856
    Code
      print(mzoe)
    Output
      [1] 0.05
    Code
      print(mae)
    Output
      [1] 0.05
    Code
      print(table(bag_resp, bag_pred))
    Output
              bag_pred
      bag_resp  1  2  3  4  5
             1 16  1  0  0  0
             2  0 31  0  0  0
             3  0  0 23  2  0
             4  0  0  0 13  1
             5  0  0  0  1 12

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.8788
    Code
      print(mzoe)
    Output
      [1] 0.84
    Code
      print(mae)
    Output
      [1] 1.01
    Code
      print(table(bag_resp, bag_pred))
    Output
              bag_pred
      bag_resp  1  2  3
             1 10  0  0
             2 30  4  0
             3  3 18  2
             4  0  1 19
             5  0  0 13

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.8712
    Code
      print(mzoe)
    Output
      [1] 0.78
    Code
      print(mae)
    Output
      [1] 0.92
    Code
      print(table(bag_resp, bag_pred))
    Output
              bag_pred
      bag_resp  1  2  3
             1 14  3  0
             2 26  5  0
             3  0 22  3
             4  0  1 13
             5  0  0 13

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.8948
    Code
      print(mzoe)
    Output
      [1] 0.78
    Code
      print(mae)
    Output
      [1] 0.9
    Code
      print(table(bag_resp, bag_pred))
    Output
              bag_pred
      bag_resp  1  2  3  4
             1 10  0  0  0
             2 27  7  0  0
             3  0 20  3  0
             4  0  2 16  2
             5  0  0 10  3

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.8812
    Code
      print(mzoe)
    Output
      [1] 0.72
    Code
      print(mae)
    Output
      [1] 0.8
    Code
      print(table(bag_resp, bag_pred))
    Output
              bag_pred
      bag_resp  1  2  3  4
             1 14  3  0  0
             2 25  6  0  0
             3  0 19  6  0
             4  0  0 12  2
             5  0  0  8  5

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
      Area under the curve: 0.9938

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
      Area under the curve: 0.8215

