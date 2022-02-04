# RMSE ----
RMSE <- function(a,b){
    result <- sqrt((1/length(a))*(sum((a - b)^2)))
    return(result)
}
a <- c(1,3,4,5)
b <- c(2,2,3,1)
RMSE(a,b)

# set.dummy ----
# y_name: dependent variable name, ex: "price_unit"
# X: independent variables
# data: include dependent and independent variables
set.dummy <- function(y_name, X, data){
    ## set dummy variable
    
    ### 先做lm，要使用和lm相同的dummy排序: coef_names
    
    model_lm <- lm(as.formula(paste0(y_name,"~.")), data = data)
    model_lm_sum <- summary(model_lm)
    coef_lm <- model_lm_sum$coefficients[,1]
    coef_names <- names(coef_lm)
    
    
    ### factor column no. setting
    x_factor <- NULL; no_level <- c()
    for(i in 1:ncol(X)){
        # print(i)
        # print(class(X[,i]))
        if(class(X[,i]) == "factor"){
            x_factor <- c(x_factor,i) # factor col
            X[,i] <- droplevels(X[,i])
        } 
        no_level[i] <- nlevels(X[,i]) # 為了標記dummyvar中是原本相同變數的
    }
    no_level[which(no_level == 0)] <- 2 # 0是數值，後面會要-1所以這裡先set 2
    no_level <- no_level - 1
    # dummyvar_index <- rep(1:length(no_level), no_level)  # 不同變數就用不同數字代表
    dummyvar_index <- rep(0, sum(no_level)) # 只用"factor"代表類別變數
    for(m in 1:ncol(X)){
        if(class(X[,m]) == "factor"){
            dummyvar_index[(sum(no_level[1:m-1]) + 1):(sum(no_level[1:m-1]) + no_level[m])] <- "factor"
        }
    }
    
    ### 遇到X當中沒有類別變數的話，就直接return X
    if(is.null(x_factor) == TRUE){ # 沒有類別變數
        dummyvar <- X
    }else{ # 有類別變數
        
        x_numeric <- seq(1,ncol(X))[-x_factor] # numeric col
        
        ### set dummy (和lm相同base line; lm當中用的是1st level作為base line)
        first_level <- c(); levels_factorvar <- list()
        for(i in 1:length(x_factor)){
            levels_factorvar[[i]] <- levels(X[,x_factor[i]])
            j <- x_factor[i]
            first_level[i] <- paste0(colnames(X)[j],"_",levels(X[,j])[1])
        }
        
        dummyvar <- fastDummies::dummy_cols(X, 
                                            select_columns = colnames(X)[x_factor])
        dummyvar <- dummyvar[, !colnames(dummyvar) %in% first_level] # delete base level
        dummyvar <- dummyvar[, !colnames(dummyvar) %in% colnames(X)[x_factor]] # delete原本column
        colnames(dummyvar)[1] <- "(Intercept)"
        
        cleannames_dummyvar <- stringr::str_replace_all(colnames(dummyvar), "_", "")
        cleannames_coef <- stringr::str_replace_all(coef_names, "_", "") # coef_names 是來自lm的排序
        r <- sapply(cleannames_coef, function(x) which(cleannames_dummyvar %in% x))
        dummyvar <- dummyvar[,r]
        
    }
    sum_dummyvar <- apply(dummyvar, 2, sum)
    sum_dummyvar[-which(dummyvar_index == "factor")] <- 0
    
    output <- list(dummyvar = dummyvar,
                   dummyvar_index = dummyvar_index)
    return(output)
}


# compute.datamatrix ----
# X: independent variables (transform to dummy if categorical var include)
# k_can:
# bf (g): basis functions
library(matrixcalc)
# compute.datamatrix <- function(X, k_can, bf){
# 
#     p <- ncol(X)
#     phi <- c(); phi_test <- c()
#     for(i in 1:p){
#         if(k_can[i]>0){
#             bf_slct <- matrix(bf[, 1:k_can[i]], ncol = k_can[i])
#             X_colmt <- matrix(rep(X[,i], times = k_can[i]), ncol = k_can[i])
#             phi <- cbind(phi, matrixcalc::hadamard.prod(bf_slct, X_colmt))
#         }
#     }
#     return(phi)
# }


compute.datamatrix <- function(X, k_can, mrts_knot, mrts_x){

    if(max(k_can) <= 2){ # autoFRK restrict k > dimension (which is 2 in this case)
        g <- mrts(knot = mrts_knot, k = 3, x = mrts_x)
        g_train <- matrix(g[,1:max(k_can)], ncol = max(k_can))
    }else{
        g_train <- mrts(knot = mrts_knot, k = max(k_can), x = mrts_x)
    }


    p <- ncol(X)
    phi <- c(); phi_test <- c()
    for(i in 1:p){
        if(k_can[i]>0){
            bf_slct <- matrix(g_train[, 1:k_can[i]], ncol = k_can[i])
            X_colmt <- matrix(rep(X[,i], times = k_can[i]), ncol = k_can[i])
            phi <- cbind(phi, matrixcalc::hadamard.prod(bf_slct, X_colmt))
        }
    }
    return(phi)
}





# data_for_CV ----
# separate data into training set and validation set for cross validation
library(sp)
data_separation_all <- function(data, seed, fold){
    set.seed(seed)
    n = nrow(data)
    n_unit = floor(nrow(data)/fold)
    
    # randomly permute rows
    data_random <- data[sample(n),]
    
    # length of the list is number of fold
    output <- list()
    for (j in 1:fold){
        
        if(j != fold){
            I <- ((j-1)*n_unit+1):(j*n_unit)
        }else{
            I <- ((j-1)*n_unit+1) : (n)
        }
        
        validation <- SpatialPointsDataFrame(coords = data_random[I,  c("X", "Y")], 
                                             data = data_random[I,  c("price_unit","build_area","age","floor_th")])
        training   <- SpatialPointsDataFrame(coords = data_random[-I, c("X", "Y")], 
                                             data = data_random[-I, c("price_unit","build_area","age","floor_th")])
        
        # each element in the list contains both training set and validation set
        output[[j]] <- list(training = training, validation = validation)
    }
    
    return(output) 
}

# log-normal prediciton ----
# calculate predictions (Y_hat and Z_hat) from linear model: Z=log(Y)~X
# Y: original data; Z: log-transformation
lognormal.pred <- function(model, data_matrix_test, Y_train){
    
    coef_est <- model$coefficients
    Y_train_hat <- model$fitted.values
    
    var_est <- sum((Y_train_hat - Y_train)^2)/(nrow(data_matrix_test) - ncol(data_matrix_test))
    
    double_inv_H <- solve(t(data_matrix_test) %*% data_matrix_test)
    b_term_train <- diag(data_matrix_test %*% double_inv_H %*% t(data_matrix_test))
    
    # Z_hat <- as.numeric(data_matrix_test %*% coef_est) - 0.5*var_est*b_term_train
    Z_hat <- as.numeric(data_matrix_test %*% coef_est)
    Y_hat <- exp(as.numeric(data_matrix_test %*% coef_est) - 0.5*var_est*b_term_train + 0.5*var_est)
    
    return(list(pred_original = Y_hat,
                pred_logscale = Z_hat))
}


# cv.lm_self (adjust from cv.lm)----
cv.lm_self_lmfit <- function (phi_for_cv,
                              Y_for_cv,
                              k_can,
                              k = 5, 
                              log_normal = FALSE, 
                              max_cores = NULL){

    outlist = list()
    if(log_normal == TRUE){
        for(i in 1:k){
            
            fit = RcppEigen::fastLm(y = Y_for_cv[[i]][["train"]], X = phi_for_cv[[i]][["train"]])
            
            r = lognormal.pred(fit, data_matrix = phi_for_cv[[i]][["test"]], Y_for_cv[[i]][["train"]])
            mu <- r$pred_original
            
            mae = abs(exp(Y_for_cv[[i]][["test"]]) - mu)
            mse = mae^2
            outlist[[i]] = list(mae = mean(mae), mse = mean(mse), mse_sqrt = sqrt(mean(mse)))   
        }
    }else{
        for(i in 1:k){

            
            # fit = lm(y[!foldrows] ~ . - 1, as.data.frame(XX), x = TRUE)
            fit = RcppEigen::fastLm(y = Y_for_cv[[i]][["train"]], X = phi_for_cv[[i]][["train"]])

            XX = phi_for_cv[[i]][["test"]]
            mu = as.matrix(XX) %*% fit$coefficients
            
            mae = abs(Y_for_cv[[i]][["test"]] - mu)
            mse = mae^2
            outlist[[i]] = list(mae = mean(mae), mse = mean(mse), mse_sqrt = sqrt(mean(mse)))   
            # print(outlist[[i]]["mse_sqrt"])
        }
    }
    
    
    cv_results = outlist
    output = list()
    for (statistic in names(outlist[[1]])) {
        vec = sapply(cv_results, function(cv_result) {
            return(cv_result[[statistic]])
        })
        statistic_value = list(mean = mean(vec, na.rm = TRUE), 
                               sd = sd(vec, na.rm = TRUE))
        
        output[[statistic]] = statistic_value
    }
    # print(output$mse_sqrt$mean)
    return(output)
}


# obtain phi_train(current k_can) from phi_train_full(M_up) ----
# another way is to use compute.datamatrix(but time consuming)
subset_phi <- function(phi_train_full, M_up, k_can, p_var){
    
    if(sum(k_can==0)>0){
        subset.kcan_start <- c(0, cumsum(rep(M_up, p_var-1))) + 1
        subset.kcan_start <- subset.kcan_start[-which(k_can==0)]
        
        subset.kcan_end <- k_can + c(0,cumsum(rep(M_up, p_var-1)))
        subset.kcan_end <- subset.kcan_end[-which(k_can==0)]
    }else{
        subset.kcan_start <- c(0, cumsum(rep(M_up, p_var-1))) + 1
        
        subset.kcan_end <- k_can + c(0,cumsum(rep(M_up, p_var-1)))
    }
    subset.kcan <- unlist(lapply(paste(subset.kcan_start, subset.kcan_end, sep = ":"), function(x) eval(parse(text = x))))

    phi_train <- phi_train_full[, subset.kcan]
    return(phi_train)
}
M_up <- 5
k_can <- c(0,1,3)
p_var <- 3


# AIC and BIC ----
# build for fastLM since AIC and BIC can only apply on the output from lm
# aic.bic_self <- function(X.train, Y.train, Y.hat.train){
    
#     if(class(X.train) == "numeric" | class(X.train) == "integer"){
#         p = 1
#         n = length(X.train)
#     }else{
#         p = ncol(X.train)
#         n = nrow(X.train)
#     }
#     sig.hat <- sqrt(sum((Y.hat.train - Y.train)^2)/(n - p))
    
#     loglikelihood <- -length(Y.train)/2*log(2*pi*sig.hat^2) - sum((Y.train-Y.hat.train)^2)/(2*sig.hat^2)
#     aic.v <- -2*loglikelihood + 2*(p)
#     bic.v <- -2*loglikelihood + log(n)*(p)
#     output = c(aic = aic.v, bic = bic.v)
    
#     return(output)
# }
aic.bic <- function(lm_model){
    res <- lm_model$residuals
    n   <- length(res)
    ll <- 0.5 * (- n * (log(2 * pi) + 1 - log(n) + log(sum(res^2))))
    df <- length(lm_model$coefficients) + 1
    aic = -2 * ll + 2 * df
    bic = -2 * ll + log(n) * df
    
    output = c(aic = aic, bic = bic)
    return(output)
}