library(autoFRK)
library(lmvar)
library(foreach)
library(doParallel)

mrbs.lm <- function(k_can, 
                    M_up, 
                    p_var, 
                    data_space_train, 
                    phi_full_for_cv,
                    Y_for_cv, 
                    phi_full,
                    Y_train,
                    criterion_setting, 
                    no_intecept,
                    log_normal){

    phi_for_cv = list()
    for(i in 1:5){
        phi_for_cv[[i]] = list(train = subset_phi(phi_full_for_cv[[i]][["train"]], M_up, k_can, p_var),
                               test = subset_phi(phi_full_for_cv[[i]][["test"]], M_up, k_can, p_var))
    }
    
    if(substr(criterion_setting,1,2)=="cv"){
        
        cv_result <- cv.lm_self_lmfit(phi_for_cv = phi_for_cv, Y_for_cv = Y_for_cv, k_can = k_can, k = 5, log_normal = log_normal)

        result <- c(cv_rmse = cv_result$mse_sqrt$mean, 
                    cv_mse = cv_result$mse$mean, 
                    cv_mae = cv_result$mae$mean)
    }else{
        phi_train = subset_phi(phi_full, M_up, k_can, p_var)        
        model <- RcppEigen::fastLm(X = phi_train, y = Y_train)
        result <- c(aic = aic.bic(model)["aic"],
                    bic = aic.bic(model)["bic"]) 
        
    }   
    #print(result["cv_rmse"])
    return(result)
}


mrbs.select.one <- function(k_can, 
                            M_up, 
                            m, 
                            criterion_setting, 
                            data_space_train, 
                            p_var,
                            phi_full_for_cv,
                            Y_for_cv,
                            phi_full,
                            Y_train,
                            no_intecept,
                            log_normal){
    
    # 一個 SVC 對應到的 M 從 M_start 到 M_up 跑一次
#     M_start <- 0
    M_start <- 0
    M_seq <- rev(seq(M_start, M_up))

    
    #####
    # parallel 1
    # criterion_result_ls = foreach(j=1:length(M_seq)) %dopar% {
    #      mrbs.lm(k_can, M_up, p_var, data_space_train, X_train, Y_train, criterion_setting)
    # }
    
    # parallel 2
    no_cores = parallel::detectCores() - 1
    cl = parallel::makeCluster(no_cores)
    clusterExport(cl, "mrbs.lm")
    clusterExport(cl, "cv.lm_self_lmfit")
    clusterExport(cl, "aic.bic")
    clusterExport(cl, "subset_phi")
    clusterExport(cl, "compute.datamatrix")
    clusterExport(cl, "lognormal.pred")    
    criterion_result_ls <- parallel::parLapply(cl, 1:length(M_seq), function(j) {
        k_can[m] <- M_seq[j]
        outlist <- mrbs.lm(k_can = k_can,
                           M_up, p_var,
                           data_space_train,
                           phi_full_for_cv,
                           Y_for_cv,
                            phi_full,
                            Y_train,
                           criterion_setting,
                          no_intecept,
                          log_normal)
        return(outlist)
    })
    parallel::stopCluster(cl)
    
    # no parallel
#     criterion_result_ls <- list()
#     for (j in 1:length(M_seq)){
#         k_can[m] <- M_seq[j]
#         criterion_result_ls[[j]] <- mrbs.lm(k_can = k_can,
#                                             M_up, p_var,
#                                             data_space_train,
#                                             phi_full_for_cv,
#                                             Y_for_cv,
#                                             criterion_setting,
#                                             no_intecept,
#                                             log_normal)
#     }
    #####
    
    # record criteria values in data frame
    criterion_result_df <- matrix(unlist(criterion_result_ls), 
                                  ncol = length(criterion_result_ls[[1]]), 
                                  byrow = TRUE)
    
    if(substr(criterion_setting,1,2)=="cv"){
        colnames(criterion_result_df) = c("cv_rmse", "cv_mse", "cv_mae")
    }else{
        colnames(criterion_result_df) = c("aic", "bic")      
    }
    
    # select min criteria 和 M_seq 中的對應值（更新在 mrbs.multi.svc 做）
    k_min <- which.min(criterion_result_df[, criterion_setting])
    min_criterion_result_vt <- criterion_result_df[k_min, ]
    
    output <- list(M_hat = M_seq[k_min], # this M_hat here is a value
                   criterion_value = min_criterion_result_vt)
    
    return(output)
}




mrbs.multi.svc <- function(formula,
                           data_attrb_train,
                           data_space_train,
                           criterion_setting, 
                           M_up,
                           log_normal = FALSE){
    # obtain Y_train ----
    y_name <- all.vars(as.formula(formula))[1]
    y_ind <- which(colnames(data_attrb_train) == y_name)
    Y_train <- data_attrb_train[,y_ind]
    if(log_normal == TRUE){
        Y_train <- log(Y_train)
    }
    
    # 產生cv的切割 ----
    set.seed(1)
    k = 5
    size = trunc(length(Y_train)/k)
    remaining = seq.int(from = 1, to = length(Y_train))
    selected_obs = matrix(nrow = k, ncol = size)
    for (i in 1:k) {
        selected_obs[i, ] = sample(remaining, size)
        remaining = setdiff(remaining, selected_obs[i, ])
    }
    
    
    # compute X_train: w/ 1 or w/o 1 ----
    no_intecept <- grepl("- 1", formula[3], fixed = TRUE) || grepl("-1", formula[3], fixed = TRUE)
    print(no_intecept)
    if(no_intecept == TRUE){ # include -1, data matrix w/o vector 1
        formula_modelmatrix <- as.formula(formula)
        `%!in%` <- Negate(`%in%`)
        X_train <- data_attrb_train[, colnames(data_attrb_train) %!in% y_name]
    }else{ # data matrix w/ vector 1 
        formula_modelmatrix <- as.formula(paste0("~ 1 + ", substr(paste(formula,collapse=" "), 
                                                                  start = nchar(y_name) + 4, 
                                                                  stop = nchar(paste(formula,collapse=" ")))))
        X_train <- model.matrix(formula_modelmatrix, data_attrb_train)
    }
    X_train = as.matrix(X_train, ncol = ncol(data_attrb_train)-1, byrow = F)
    
    # parameters setting ----
    p_var <- ncol(X_train)
    
    # compute data matrix (basis functions x independent variables) phi_train_full ----
    phi_full_for_cv <- list()
    Y_for_cv <- list()
    for(i in 1:k){
        foldrows = is.element(1:length(Y_train), selected_obs[i, ])
        data_space_train_sub <- data_space_train[!foldrows,]
        phi_full <- compute.datamatrix(X = X_train, k_can = rep(M_up, p_var), mrts_knot = data_space_train_sub, mrts_x = data_space_train)
        phi_full_for_cv[[i]] <- list(train = phi_full[!foldrows, ],
                                     test = phi_full[foldrows, ])
        Y_for_cv[[i]] <- list(train = Y_train[!foldrows],
                              test = Y_train[foldrows])
    }

    # calculate dummy variable ----    
    # dummyvar_data  <- set.dummy(y_name, X_train, data_attrb_train)[[1]] # 尚未處理
    
    # output object  ----
    criterion_result_len <- data.frame(aic   = c(), bic   = c())
    k_can_all <- data.frame()
    
    # algorithm ----
#     k_can <- rep(M_up, p_var)
    k_can <- rep(1, p_var)
    k_previous <- rep(0, p_var)
    c <- 0
    print(c(c, k_can))
    while(sum(k_previous == k_can) != p_var){
        k_previous <- k_can
        
        for(m in 1:p_var){
            
            result  <- mrbs.select.one (k_can , 
                                        M_up, 
                                        m, 
                                        criterion_setting, 
                                        data_space_train, 
                                        p_var, 
                                        phi_full_for_cv,
                                        Y_for_cv,
                                        phi_full,
                                        Y_train,
                                        no_intecept,
                                        log_normal)
            # update
            criterion_result_len <- rbind(criterion_result_len, result$criterion_value)
            k_can[m] <-  result$M_hat
            c <- c + 1
            
            print(c(c, k_can))
            
        }
    }
    
    # compute final model using selected M_hat ----
    M_hat <- k_can
    # g_train <- mrts(knot = data_space_train, k = max(k_can), x = data_space_train)
    # phi_train <- compute.datamatrix(X_train, M_hat, bf = g_train)  
    phi_train <- compute.datamatrix(X = X_train, k_can = k_can, mrts_knot = data_space_train, mrts_x = data_space_train)
    model <- RcppEigen::fastLm(y = Y_train, X = phi_train)
    Y_hat = model$fitted.values
    
    # output ----
    if(log_normal == TRUE){
        r = lognormal.pred(model, data_matrix = phi_train, Y_train)
        Z_hat <- r$pred_logscale
        Y_hat <- r$pred_original
        
        output <- list(model = model, 
                       M_hat = M_hat, 
                       X_train = X_train,
                       Y_train = Y_train,
                       data_space_train = data_space_train,
                       formula = formula,
                       log_normal = log_normal,
                       Y_hat = Y_hat,
                       Z_hat = Z_hat)
    }else{
        output <- list(model = model, 
                       M_hat = M_hat, 
                       X_train = X_train,
                       Y_train = Y_train,
                       data_space_train = data_space_train,
                       formula = formula,
                       log_normal = log_normal,
                       Y_hat = Y_hat)
    }
    return(output)
    
}











