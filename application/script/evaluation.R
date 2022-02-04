# prediction ----
predict.mrbs.svc <- function(mrbs_svc_result, 
                             data_attrb_test, 
                             data_space_test,
                             log_normal){
    
    M_hat <- mrbs_svc_result$M_hat
    Y_train <- mrbs_svc_result$Y_train
    data_space_train <- as.matrix(mrbs_svc_result$data_space_train)
    formula <- mrbs_svc_result$formula
    model <- mrbs_svc_result$model
    log_normal <- mrbs_svc_result$log_normal
    X_train <- mrbs_svc_result$X_train
    
    y_name <- all.vars(as.formula(formula))[1]
    y_ind <- which(colnames(data_attrb_test) == y_name)
    Y_test <- data_attrb_test[,y_ind]
    if(log_normal == TRUE){
        Y_test <- log(Y_test)
    }
    
    coef_est <- model$coefficients
    Y_train_hat <- model$fitted.values
    
    sigma_est <- sqrt(sum((Y_train_hat - Y_train)^2)/(nrow(X_train) - ncol(X_train)))
    
    # compute data matrix (basis functions x independent variables)
    no_intecept <- grepl("- 1", formula[3], fixed = TRUE) || grepl("-1", formula[3], fixed = TRUE)
    if(no_intecept == TRUE){
        formula_modelmatrix <- as.formula(formula)
        `%!in%` <- Negate(`%in%`)
        X_test <- data_attrb_test[, colnames(data_attrb_test) %!in% y_name]
    }else{
        formula_modelmatrix <- as.formula(paste0("~ 1 + ", substr(paste(formula,collapse=" "), 
                                                                  start = nchar(y_name) + 4, 
                                                                  stop = nchar(paste(formula,collapse=" ")))))
        X_test <- model.matrix(formula_modelmatrix, data_attrb_test)
    }
    
    data_space_test <- as.matrix(data_space_test)
    # g_test  <- mrts(knot = data_space_train, 
    #                 k = max(M_hat), 
    #                 x = data_space_test)
    # phi_test <- compute.datamatrix(X_test, M_hat, g_test)
    phi_test <- compute.datamatrix(X_test, M_hat, data_space_train, data_space_test)
    

    
    if(log_normal == TRUE){
        r = lognormal.pred(model, data_matrix = phi_test, Y_train)
        Z_hat <- r$pred_logscale
        Y_hat <- r$pred_original
        
        output <- list(X_test = X_test,
                       data_space_test = data_space_test,
                       # g_test = g_test,
                       log_normal = log_normal,
                       Y_hat = Y_hat,
                       Z_hat = Z_hat,
                       sigma_est = sigma_est)
    }else{
        Y_hat <-  phi_test %*% coef_est
        output <- list(X_test = X_test,
                       data_space_test = data_space_test,
                       # g_test = g_test,
                       log_normal = log_normal,
                       Y_hat = Y_hat,
                       sigma_est = sigma_est)
    }
    return(output)
    
}



predict.mrbs.svc.simple <- function(M_hat, 
                                    formula, 
                                    data_attrb_train, 
                                    data_space_train, 
                                    data_attrb_valid, 
                                    data_space_valid,
                                    log_normal){
    
    # parameters setting ----
    formula <- as.formula(formula)
    y_name <- all.vars(formula)[1]
    y_ind <- which(colnames(data_attrb_train) == y_name)
    Y_train <- data_attrb_train[,y_ind]
    if(log_normal == TRUE){
        Y_train <- log(Y_train)
    }
    
    formula_modelmatrix = as.formula(paste0("~ 1 + ", substr(paste(formula,collapse=" "), start = nchar(y_name) + 4, stop = nchar(paste(formula,collapse=" ")))))
    data_space_mt = as.matrix(data_space_train)
    
    g_train <- mrts(knot = data_space_mt, k = max(M_hat), x = data_space_mt) # NOTE: 用於surface estimation
    X_train <- model.matrix(formula_modelmatrix, data_attrb_train)
    # dummyvar_data  <- set.dummy(y_name, X_train, data_attrb_train)[[1]]
    dummyvar_data <- X_train
    
    
    
    phi_train <- compute.datamatrix(dummyvar_data, M_hat, g_train)
    # model <- lm(Y_train ~ phi_train[,-1])
    model <- RcppEigen::fastLm(X = phi_train, y = Y_train)
    
    
    # output ----
    if(log_normal == TRUE){

        r = lognormal.pred(model, data_matrix = phi_train, Y_train)
        
        Z_hat <- r$pred_logscale
        Y_hat <- r$pred_original
        
        result <- list(model = model, 
                       M_hat = M_hat, 
                       X_train = X_train,
                       Y_train = Y_train,
                       data_space_train = data_space_train,
                       g_train = g_train,
                       formula = formula,
                       log_normal = log_normal,
                       Y_hat = Y_hat,
                       Z_hat = Z_hat)
    }else{
        Y_hat = model$fitted.values
        
        result <- list(model = model, 
                       M_hat = M_hat, 
                       X_train = X_train,
                       Y_train = Y_train,
                       data_space_train = data_space_train,
                       g_train = g_train,
                       formula = formula,
                       log_normal = log_normal,
                       Y_hat = Y_hat)
    }

    
    # predict ----
    prediction = predict.mrbs.svc(mrbs_svc_result = result, 
                                  data_attrb_valid, 
                                  data_space_valid,
                                  log_normal = log_normal)
    return(prediction)
}