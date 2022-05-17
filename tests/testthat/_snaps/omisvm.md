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
      print(table(true, pred))
    Output
          pred
      true  1  2  3  4  5
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
      print(table(true, pred))
    Output
          pred
      true  1  2  3  4  5
         1 16  1  0  0  0
         2  0 31  0  0  0
         3  0  0 23  2  0
         4  0  0  0 13  1
         5  0  0  0  1 12

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.8871
    Code
      print(mzoe)
    Output
      [1] 0.83
    Code
      print(mae)
    Output
      [1] 0.99
    Code
      print(table(true, pred))
    Output
          pred
      true  1  2  3
         1 10  0  0
         2 28  6  0
         3  2 20  1
         4  0  1 19
         5  0  0 13

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.8805
    Code
      print(mzoe)
    Output
      [1] 0.73
    Code
      print(mae)
    Output
      [1] 0.9
    Code
      print(table(true, pred))
    Output
          pred
      true  1  2  3
         1 17  0  0
         2 23  8  0
         3  3 20  2
         4  0  1 13
         5  0  0 13

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.7303
    Code
      print(mzoe)
    Output
      [1] 0.69
    Code
      print(mae)
    Output
      [1] 1.05
    Code
      print(table(true, pred))
    Output
          pred
      true  1  2  3
         1  5  5  0
         2  8 26  0
         3  0 23  0
         4  0 18  2
         5  0  5  8

---

    Code
      print(roc$auc)
    Output
      Multi-class area under the curve: 0.7535
    Code
      print(mzoe)
    Output
      [1] 0.64
    Code
      print(mae)
    Output
      [1] 0.89
    Code
      print(table(true, pred))
    Output
          pred
      true  1  2  3
         1  8  9  0
         2  4 27  0
         3  0 24  1
         4  0  8  6
         5  0  4  9

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

