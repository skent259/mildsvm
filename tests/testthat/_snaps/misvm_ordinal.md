# misvm_ordinal() internal functions work on simple examples

    Code
      pROC::multiclass.roc(response = classify_bags(y, bags), predictor = classify_bags(
        f, bags)) %>% suppressMessages()
    Output
      
      Call:
      multiclass.roc.default(response = classify_bags(y, bags), predictor = classify_bags(f,     bags))
      
      Data: classify_bags(f, bags) with 5 levels of classify_bags(y, bags): 1, 2, 3, 4, 5.
      Multi-class area under the curve: 0.9982
    Code
      mzoe <- mean(classify_bags(y, bags) != classify_bags(y_pred, bags))
      mae <- mean(abs(classify_bags(y, bags) - classify_bags(y_pred, bags)))
      mzoe
    Output
      [1] 0.06
    Code
      mae
    Output
      [1] 0.06
    Code
      outer(as.vector(f), res$gurobi_fit$b, `+`)[1:10, ]
    Output
                  [,1]      [,2]       [,3]      [,4]
       [1,] -0.1757371 -2.453356 -4.8078936 -6.979969
       [2,] -1.0602319 -3.337851 -5.6923884 -7.864464
       [3,]  0.6259899 -1.651629 -4.0061666 -6.178242
       [4,]  0.8622232 -1.415396 -3.7699333 -5.942009
       [5,]  0.9196849 -1.357934 -3.7124716 -5.884547
       [6,]  4.2715276  1.993909 -0.3606289 -2.532705
       [7,]  4.3343577  2.056739 -0.2977988 -2.469875
       [8,]  4.1057509  1.828132 -0.5264056 -2.698481
       [9,]  5.6321565  3.354538  1.0000000 -1.172076
      [10,]  4.2658762  1.988257 -0.3662803 -2.538356

# misvm_ordinal() has reasonable performance

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

# `misvm_ordinal()` value returns make sense

    Code
      models <- list(xy = misvm_ordinal(x = df1[, 3:7], y = df1$bag_label, bags = df1$
        bag_name, method = "qp-heuristic", weights = NULL), formula = misvm_ordinal(
        mi(bag_label, bag_name) ~ V1 + V2, method = "qp-heuristic", data = df1,
        weights = NULL), `no-scale` = misvm_ordinal(x = df1[, 3:7], y = df1$bag_label,
      bags = df1$bag_name, method = "qp-heuristic", weights = NULL, control = list(
        scale = FALSE))) %>% suppressWarnings() %>% suppressMessages()
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
      

# Ordering of data doesn't change `misvm_ordinal()` results

    Code
      with(df1_test, suppressWarnings({
        pred <- predict(mdl2, df1_test, type = "raw")$.pred
        pROC::auc(classify_bags(bag_label, bag_name), classify_bags(pred, bag_name))
      }))
    Message <simpleMessage>
      Setting levels: control = 1, case = 2
      Setting direction: controls < cases
    Output
      Area under the curve: 1

