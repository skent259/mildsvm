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

