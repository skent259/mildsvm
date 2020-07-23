

##' Create a new smm object 
##'
##' Create a new smm object
##' @param x A list
##' @return An smm object
##' @examples
##' 
##' new_smm(list())
##' @export 
##' @author Yifei Liu
new_smm <- function(x = list()){
    stopifnot(is.list(x))
    structure(
        x,
        class = c("smm")
    )
}


##' default method for SMM function.
##'
##' default method for SMM function.
##' @param df A data.frame where the first two columns being `instance_label` and `instance_name`, or a MilData object. `df` can be NULL if kernel_mild is a matrix and y is not NULL.
##' @param kernel_mild The kernel to be used, current only supports "rbf"; or a kernal matrix.
##' @param cost The cost for SMM, larger cost gives larger tolerance to errors.
##' @param class.weights A named vector where the names should be the same as the unique values of the label. For the use of imbalance data. Will be passed to the `ksvm` function in `kernlab` package. Notice that a class weight 2:1 will gives different results from 1:0.5 because they will be directly multiplied to the cost.
##' @param sigma The parameter for the rbf kernel.
##' @param y The instance label. If omitted, will use that from df.
##' @return An `smm` object.
##' @examples
##' x = data.frame("instance_label" = factor(c(1, 1, 0)),
##'                 "instance_name" = c("bag_1_inst_1", "bag_1_inst_2", "bag_2_inst_1"),
##'                "X1" = c(-0.4, 0.5, 2))
##' mdl <- SMM(df = x, cost = 10)
##' @importFrom kernlab ksvm
##' @export 
##' @author Yifei Liu
SMM.default <- function(df, kernel_mild = "rbf", cost = 1, class.weights = NULL, sigma = 0.05, y = NULL){
    ## the data structure for the input data.frame df is
    ## instance_label | instance_name | feature_1 | ...

    if(is.null(y)) y <- unlist( df %>% dplyr::group_by(instance_name) %>% dplyr::summarise( label = instance_label[1] ) %>% dplyr::select(label))

    if(is.matrix(kernel_mild)){
        K_matrix <- kernel_mild
    }else{
        K_matrix <- kme(df = df %>% dplyr::select(- instance_label), sigma = sigma)
    }
    res <- kernlab::ksvm(x = K_matrix, y = y, kernel = "matrix", C = cost, class.weights = class.weights)
    return(new_smm(list("ksvm_res" = res, "sigma" = sigma, "traindata" = df, "cost" = cost)))
}

##' MilData method for SMM function
##'
##' MilData method for SMM function
##' @param df A data.frame where the first two columns being `instance_label` and `instance_name`, or a MilData object. `df` can be NULL if kernel_mild is a matrix and y is not NULL.
##' @param kernel_mild The kernel to be used, current only supports "rbf"; or a kernal matrix.
##' @param cost The cost for SMM, larger cost gives larger tolerance to errors.
##' @param class.weights A named vector where the names should be the same as the unique values of the label. For the use of imbalance data. Will be passed to the `ksvm` function in `kernlab` package. Notice that a class weight 2:1 will gives different results from 1:0.5 because they will be directly multiplied to the cost.
##' @param sigma The parameter for the rbf kernel.
##' @param y The instance label. If omitted, will use that from df.
##' @return An `smm` object.
##' @examples
##' 
##' x = data.frame("bag_LABEL" = factor(c(1, 1, 0)),
##'                "bag_name" = c(rep("bag_1", 2), "bag_2"),
##'                "instance_name" = c("bag_1_inst_1", "bag_1_inst_2", "bag_2_inst_1"),
##'                "X1" = c(-0.4, 0.5, 2),
##'                "instance_label" = c(0, 1, 0)
##' )
##' mdl <- SMM(df = MilData(x), cost = 10)
##' @export 
##' @author Yifei Liu
SMM.MilData <- function(df, kernel_mild = "rbf", cost = 1, class.weights = NULL, sigma = 0.05, y = NULL){
    colnames(df)[which(colnames(df) == "bag_label")] = "instance_label"
    df$bag_name = NULL
    SMM.default(df, kernel_mild, cost, class.weights, sigma, y)
}
##' Function to carry out support measure machines (SMM) algorithm.
##'
##' Function to carry out support measure machines algorithm which is appropriate for multiple instance learning. The algorithm basically calculates the kernal matrix of differernt empirical measures using kernel mean embedding.
##' @param df A data.frame where the first two columns being `instance_label` and `instance_name`, or a MilData object. `df` can be NULL if kernel_mild is a matrix and y is not NULL.
##' @param kernel_mild The kernel to be used, current only supports "rbf"; or a kernal matrix.
##' @param cost The cost for SMM, larger cost gives larger tolerance to errors.
##' @param class.weights A named vector where the names should be the same as the unique values of the label. For the use of imbalance data. Will be passed to the `ksvm` function in `kernlab` package. Notice that a class weight 2:1 will gives different results from 1:0.5 because they will be directly multiplied to the cost.
##' @param sigma The parameter for the rbf kernel.
##' @param y The instance label. If omitted, will use that from df.
##' @return An `smm` object.
##' @examples
##' 
##' x = data.frame("bag_LABEL" = factor(c(1, 1, 0)),
##'                "bag_name" = c(rep("bag_1", 2), "bag_2"),
##'                "instance_name" = c("bag_1_inst_1", "bag_1_inst_2", "bag_2_inst_1"),
##'                "X1" = c(-0.4, 0.5, 2),
##'                "instance_label" = c(0, 1, 0)
##' )
##' mdl <- SMM(df = MilData(x), cost = 10)
##' @export 
##' @author Yifei Liu
SMM <- function(df, kernel_mild = "rbf", cost = 1, class.weights = NULL, sigma = 0.05, y = NULL){
    UseMethod("SMM")
}

##' Prediction function for smm objects (the return from SMM())
##'
##' This function gives the prediction of an smm object in the scale of score. A higher score indicates the observation is more likely to be of class `1`.
##' @param object An smm object.
##' @param ... `newdata`, new data whose first column should be `instance_name`. `traindata`, training data used to train res. The first column should be `instance_name`. `kernel_mild`, currently only support "rbf"
##' @return A vector of length of the instances in newdata.
##' @examples
##' x = data.frame("bag_LABEL" = factor(c(1, 1, 0)),
##'                "bag_name" = c(rep("bag_1", 2), "bag_2"),
##'                "instance_name" = c("bag_1_inst_1", "bag_1_inst_2", "bag_2_inst_1"),
##'                "X1" = c(-0.4, 0.5, 2),
##'                "instance_label" = c(0, 1, 0)
##' )
##' mdl <- SMM(df = MilData(x), cost = 10, sigma = 0.05)
##' x_inst <- data.frame("instance_name" = c("bag_1_inst_1", "bag_1_inst_2", "bag_2_inst_1"),
##'                "X1" = c(-0.4, 0.5, 2)
##'                )
##' predictions <- predict(object = mdl, kernel_mild = "rbf", newdata = x_inst, traindata = x_inst)
##' @export
##' @importFrom stats predict
##' @author Yifei Liu
predict.smm <- function(object, ...){## kernel_mild, newdata, traindata){
    ## the data structure for the input data.frames are
    ## instance_name | feature_1 | ...

    ## sigma can be read from object
    args = list(...)
    kernel_mild = args$kernel_mild
    newdata = args$newdata
    traindata = args$traindata
    
    sigma = object$sigma
    ksvm_res = object$ksvm_res
    
    beta_0 <- - ksvm_res@b
    if(is.matrix(kernel_mild)){
        kernel_matrix <- kernel_mild[ksvm_res@alphaindex[[1]], ]
        return( ( t(ksvm_res@coef[[1]]) %*% kernel_matrix+ beta_0 )[1, ] )
    }else{
        if(is.null(kernel_mild)) kernel_mild <- "rbf"
        unique_instance_name_train <- unique(traindata$instance_name)[ksvm_res@alphaindex[[1]]]
        kernel_matrix <- kme(df = traindata[traindata$instance_name %in% unique_instance_name_train, ], df2 = newdata, sigma = sigma)
        return( ( t(ksvm_res@coef[[1]]) %*% kernel_matrix + beta_0 )[1, ] )
    }
}
