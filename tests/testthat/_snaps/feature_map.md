# Nystrom feature map prints correctly

    Code
      kfms <- list(default = kfm_nystrom(df), supplied_sample = kfm_nystrom(df,
        sampling = 1:7), stratified_sample = kfm_nystrom(df2, sampling = "stratified"),
      `low m` = kfm_nystrom(df, m = 5), `low r` = kfm_nystrom(df, r = 5), sigma = kfm_nystrom(
        df, sigma = 0.05)) %>% suppressWarnings() %>% suppressMessages()
      print(kfms)
    Output
      $default
      A Nystrom kernel feature map object 
       
      Parameters: 
        m: 7 
        r: 7 
        kernel: radial  (sigma = NULL) 
        sampling: random 
      
      
      $supplied_sample
      A Nystrom kernel feature map object 
       
      Parameters: 
        m: 7 
        r: 7 
        kernel: radial  (sigma = NULL) 
        sampling: user supplied sampling 
      
      
      $stratified_sample
      A Nystrom kernel feature map object 
       
      Parameters: 
        m: 196 
        r: 196 
        kernel: radial  (sigma = NULL) 
        sampling: user supplied sampling 
      
      
      $`low m`
      A Nystrom kernel feature map object 
       
      Parameters: 
        m: 5 
        r: 5 
        kernel: radial  (sigma = NULL) 
        sampling: random 
      
      
      $`low r`
      A Nystrom kernel feature map object 
       
      Parameters: 
        m: 7 
        r: 5 
        kernel: radial  (sigma = NULL) 
        sampling: random 
      
      
      $sigma
      A Nystrom kernel feature map object 
       
      Parameters: 
        m: 7 
        r: 7 
        kernel: radial  (sigma = 0.05) 
        sampling: random 
      
      

