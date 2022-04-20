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
      data = df2)) %>% suppressWarnings() %>% suppressMessages()
      print(lapply(models, names))
    Output
      $heur
      [1] "fits"      "call_type" "levels"    "features" 
      
      $qp
      [1] "fits"      "call_type" "levels"    "features" 
      
      $mip
      [1] "fits"      "call_type" "levels"    "features" 
      
      $formula
      [1] "fits"      "call_type" "levels"    "features"  "formula"   "bag_name" 
      

