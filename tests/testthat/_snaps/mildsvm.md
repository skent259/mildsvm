# `mildsvm()` value returns make sense

    Code
      models <- list(`mildata-heur` = mildsvm(mil_data, method = "heuristic"),
      `mildata-mip` = mildsvm(mil_data, method = "mip", control = list(nystrom_args = list(
        m = 10))), `mildata-qp` = mildsvm(mil_data, method = "qp-heuristic"), xy = mildsvm(
        x = as.data.frame(mil_data[, 4:13]), y = mil_data$bag_label, bags = mil_data$
          bag_name, instances = mil_data$instance_name), formula = mildsvm(mild(
        bag_label, bag_name, instance_name) ~ ., data = mil_data), `no-scale-heur` = mildsvm(
        mil_data, method = "heuristic", control = list(scale = FALSE)),
      `no-scale-mip` = mildsvm(mil_data, method = "mip", control = list(scale = FALSE,
        nystrom_args = list(m = 10))), `no-scale-qp` = mildsvm(mil_data, method = "qp-heuristic",
        control = list(scale = FALSE)), `no-weights` = mildsvm(mil_data, method = "heuristic",
        weights = FALSE)) %>% suppressWarnings() %>% suppressMessages()
      print(lapply(models, names))
    Output
      $`mildata-heur`
       [1] "ksvm_fit"        "call_type"       "x"               "features"       
       [5] "levels"          "cost"            "sigma"           "weights"        
       [9] "repr_inst"       "n_step"          "useful_inst_idx" "inst_order"     
      [13] "x_scale"         "bag_name"        "instance_name"  
      
      $`mildata-mip`
       [1] "gurobi_fit"    "kfm_fit"       "call_type"     "features"     
       [5] "levels"        "cost"          "sigma"         "weights"      
       [9] "x_scale"       "bag_name"      "instance_name"
      
      $`mildata-qp`
       [1] "gurobi_fit"    "call_type"     "x"             "features"     
       [5] "levels"        "cost"          "sigma"         "weights"      
       [9] "repr_inst"     "n_step"        "x_scale"       "bag_name"     
      [13] "instance_name"
      
      $xy
       [1] "ksvm_fit"        "call_type"       "x"               "features"       
       [5] "levels"          "cost"            "sigma"           "weights"        
       [9] "repr_inst"       "n_step"          "useful_inst_idx" "inst_order"     
      [13] "x_scale"        
      
      $formula
       [1] "ksvm_fit"        "call_type"       "x"               "features"       
       [5] "levels"          "cost"            "sigma"           "weights"        
       [9] "repr_inst"       "n_step"          "useful_inst_idx" "inst_order"     
      [13] "x_scale"         "formula"         "bag_name"        "instance_name"  
      
      $`no-scale-heur`
       [1] "ksvm_fit"        "call_type"       "x"               "features"       
       [5] "levels"          "cost"            "sigma"           "weights"        
       [9] "repr_inst"       "n_step"          "useful_inst_idx" "inst_order"     
      [13] "bag_name"        "instance_name"  
      
      $`no-scale-mip`
       [1] "gurobi_fit"    "kfm_fit"       "call_type"     "features"     
       [5] "levels"        "cost"          "sigma"         "weights"      
       [9] "bag_name"      "instance_name"
      
      $`no-scale-qp`
       [1] "gurobi_fit"    "call_type"     "x"             "features"     
       [5] "levels"        "cost"          "sigma"         "weights"      
       [9] "repr_inst"     "n_step"        "bag_name"      "instance_name"
      
      $`no-weights`
       [1] "ksvm_fit"        "call_type"       "x"               "features"       
       [5] "levels"          "cost"            "sigma"           "repr_inst"      
       [9] "n_step"          "useful_inst_idx" "inst_order"      "x_scale"        
      [13] "bag_name"        "instance_name"  
      

