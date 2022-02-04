# updated: 2020.09.09
# issues: 
# (1) alter mrbs.select.one and mrbs.multi to use only training data (exclude test data) 
# (2) add predict.mrts.multi for test data
# note: file = 3_DA_2018_bigtaipei_update.R use this function version


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
compute.datamatrix <- function(X, k_can, bf){
    p <- ncol(X)
    phi <- c(); phi_test <- c() 
    for(i in 1:p){
        if(k_can[i]>0){
            bf_slct <- matrix(bf[, 1:k_can[i]], ncol = k_can[i])
            X_colmt <- matrix(rep(X[,i], times = k_can[i]), ncol = k_can[i])
            phi <- cbind(phi, matrixcalc::hadamard.prod(bf_slct, X_colmt))
        }
    }
    return(phi)
}

# mrbs.select.one ----
mrbs.select.one <- function(k_can, M.up, m, criterion_setting, phi_train_full, p.,
                            g.train, dummyvar.train, Y.train){
    # 1. create object
    criterion_result_vt <- data.frame(aic  = numeric(), bic  = numeric())
    # 2. 跑回圈
    
    M.start <- 0
    # M.seq <- ceiling(seq(M.start, M.up, length.out = 100))
    M.seq <- seq(M.start, M.up)
    
    for(j in 1:length(M.seq)){
        
        k_can[m] <- M.seq[j]
        # compute phi
        # phi_train <- compute.datamatrix(dummyvar.train, k_can, bf = g.train)
        
        # compute phi (sunset version)
        if(sum(k_can==0)>0){
            subset.kcan_start <- c(0, cumsum(rep(M.up, p.-1))) + 1
            subset.kcan_start[which(k_can==0)] <- 0
        }else{
            subset.kcan_start <- c(0, cumsum(rep(M.up, p.-1))) + 1
        }
        subset.kcan_end <- k_can + c(0,cumsum(rep(M.up, p.-1)))
        subset.kcan <- unlist(lapply(paste(subset.kcan_start, subset.kcan_end, sep = ":"), function(x) eval(parse(text = x))))
        
        phi_train <- phi_train_full[, subset.kcan]
        
        # model result
        model <- lm(Y.train ~ phi_train - 1)
        
        ## criterion
        df <- data.frame(aic   = AIC(model),
                         bic   = BIC(model))
        criterion_result_vt <- rbind(criterion_result_vt, df)
        # print(j)
    }
    gc()
    # print(k_can)
    # 3. selection
    k_min <- which.min(criterion_result_vt[, criterion_setting])
    
    # 4. select result
    min_criterion_result_vt <- criterion_result_vt[k_min, ]
    # 5. return
    output <- list(M.hat = M.seq[k_min],
                   criterion_value = min_criterion_result_vt)
    # M.hat
    # min_criterion_result_vt
    # min_rmse_result_vt
}

# mrbs.multi.svc ----
mrbs.multi.svc <- function(formula,
                           data.train,
                           space.train,
                           criterion_setting, 
                           M.up){
    # prepare data
    space.train_mt <- as.matrix(space.train)
    g.train <- autoFRK::mrts(knot = space.train_mt, k = M.up, x = space.train_mt) # NOTE: 用於surface estimation
    formula <- as.formula(formula)
    y_name <- all.vars(formula)[1]
    y_ind <- which(colnames(data.train) == y_name)
    Y.train <- data.train[,y_ind]
    formula.modelmatrix <- as.formula(paste0("~ 1 + ", substr(paste(formula,collapse=" "), start = nchar(y_name) + 4, stop = nchar(paste(formula,collapse=" ")))))
    X.train <- model.matrix(formula.modelmatrix, data.train)
    dummyvar.train <- set.dummy(y_name, X.train, data.train)[[1]] # source(file = "source_set_dummy.R") # dummyvar
    dummyvar_index <- set.dummy(y_name, X.train, data.train)[[2]] # NOTE: 用於 function中判斷是否為類別變數的依據
    p. <- ncol(dummyvar.train) # p_X <- ncol(X) # 原本X有幾個變數(並非看dummy)
    
    
    
    ## output object 
    criterion_result_len <- data.frame(aic   = c(), bic   = c())
    k_can_all <- data.frame()
    
    ## full basis functions
    phi_train_full <- compute.datamatrix(dummyvar.train, rep(M.up, p.), bf = g.train)
    ## loop
    k_can <- rep(M.up, p.)
    k_previous <- rep(0, p.)
    while(sum(k_previous == k_can) != p.){
        k_previous <- k_can
        
        for(m in 1:p.){
            
            result  <- mrbs.select.one (k_can , M.up, m, criterion_setting, phi_train_full, p., 
                                        g.train, dummyvar.train, Y.train)
            # update
            criterion_result_len <- rbind(criterion_result_len, result$criterion_value)
            k_can[m] <-  result$M.hat
            print(k_can)
            
        }
    }
    
    
    # build final model
    phi.train <- compute.datamatrix(dummyvar.train, k_can, g.train)
    model <- lm(Y.train ~ phi.train[,-1]) 
    coef.est <- model$coefficients
    Y.hat.train <- as.numeric(phi.train %*% coef.est)
    
    output <- list(M.hat = k_can,
                   # M.hat.all = k_can_all,
                   coefficients.beta = coef.est,
                   fitted.values = Y.hat.train,
                   criterion_value = criterion_result_len,
                   basis.functions = g.train,
                   formula = formula,
                   space.train = space.train_mt,
                   X.train = X.train,
                   Y.train = Y.train,
                   modle = model)
    return(output)
}



# mrbs.select.one.glm ----
mrbs.select.one.glm <- function(k_can, M.up, m, criterion_setting, phi_train_full, p.,
                                g.train, dummyvar.train, Y.train){
    # 1. create object
    criterion_result_vt <- data.frame(aic  = numeric(), bic  = numeric())
    # 2. 跑回圈
    if(m == 1){
        M.start <- 1
    }else{
        M.start <- 0
    }
    
    # M.seq <- ceiling(seq(M.start, M.up, length.out = 100))
    M.seq <- seq(M.start, M.up)
    
    for(j in 1:length(M.seq)){
        
        k_can[m] <- M.seq[j]
        # compute phi
        # phi_train <- compute.datamatrix(dummyvar.train, k_can, bf = g.train)
        
        # compute phi (sunset version)
        if(sum(k_can==0)>0){
            subset.kcan_start <- c(0, cumsum(rep(M.up, p.-1))) + 1
            subset.kcan_start[which(k_can==0)] <- 0
            
            subset.kcan_end <- k_can + c(0,cumsum(rep(M.up, p.-1)))
            subset.kcan_end[which(k_can==0)] <- 0
        }else{
            subset.kcan_start <- c(0, cumsum(rep(M.up, p.-1))) + 1
            subset.kcan_end <- k_can + c(0,cumsum(rep(M.up, p.-1)))
        }
        
        subset.kcan <- unlist(lapply(paste(subset.kcan_start, subset.kcan_end, sep = ":"), function(x) eval(parse(text = x))))
        
        phi_train <- phi_train_full[, subset.kcan]
        
        # model result
        # if(sum(subset.kcan)==0){
        #     model <- glm(Y.train ~ 1 , family = "binomial")
        #     
        # }else{
            phi_train_1 <- as.matrix(phi_train)
            model <- glm(Y.train ~ phi_train_1 - 1, family = "binomial")
            
        # }
        
        ## criterion
        df <- data.frame(aic   = AIC(model),
                         bic   = BIC(model))
        criterion_result_vt <- rbind(criterion_result_vt, df)
        # print(j)
    }
    gc()
    # print(k_can)
    # 3. selection
    k_min <- which.min(criterion_result_vt[, criterion_setting])
    
    # 4. select result
    min_criterion_result_vt <- criterion_result_vt[k_min, ]
    # 5. return
    output <- list(M.hat = M.seq[k_min],
                   criterion_value = min_criterion_result_vt)
    # M.hat
    # min_criterion_result_vt
    # min_rmse_result_vt
}
# mrbs.multi.svc.glm ----
mrbs.multi.svc.glm <- function(formula,
                               data.train,
                               space.train,
                               criterion_setting, 
                               M.up){
    # prepare data
    space.train_mt <- as.matrix(space.train)
    g.train <- autoFRK::mrts(knot = space.train_mt, k = M.up, x = space.train_mt) # NOTE: 用於surface estimation
    formula <- as.formula(formula)
    y_name <- all.vars(formula)[1]
    y_ind <- which(colnames(data.train) == y_name)
    Y.train <- data.train[,y_ind]
    formula.modelmatrix <- as.formula(paste0("~ 1 + ", substr(paste(formula,collapse=" "), start = nchar(y_name) + 4, stop = nchar(paste(formula,collapse=" ")))))
    X.train <- model.matrix(formula.modelmatrix, data.train)
    dummyvar.train <- set.dummy(y_name, X.train, data.train)[[1]] # source(file = "source_set_dummy.R") # dummyvar
    # dummyvar_index <- set.dummy(y_name, X.train, data.train)[[2]] # NOTE: 用於 function中判斷是否為類別變數的依據
    p. <- ncol(dummyvar.train) # p_X <- ncol(X) # 原本X有幾個變數(並非看dummy)
    
    ## output object 
    criterion_result_len <- data.frame(aic   = c(), bic   = c()); k_can_all <- data.frame()
    ## full basis functions 
    phi_train_full <- compute.datamatrix(dummyvar.train, rep(M.up, p.), bf = g.train)
    ## loop 
    k_can <- rep(1, p.)
    k_previous <- rep(0, p.)
    while(sum(k_previous == k_can) != p.){
        k_previous <- k_can

        
        for(m in 1:p.){
            # m = p.- m + 1
            result  <- mrbs.select.one.glm (k_can , M.up, m, criterion_setting, phi_train_full, p., 
                                            g.train, dummyvar.train, Y.train)
            # update
            criterion_result_len <- rbind(criterion_result_len, result$criterion_value)
            k_can[m] <-  result$M.hat
            print(k_can)
            
        }
    }
    
    # build final model
    phi.train <- compute.datamatrix(dummyvar.train, k_can, g.train)
    colnames(phi.train) <- 1:ncol(phi.train)
    # model.final <- glm(Y.train ~ phi.train[,-1], family = "binomial") 
    model.final <- glm(Y.train ~ phi.train - 1, family = "binomial") 
    
    coef.est <- model.final$coefficients
    p.hat.train <- model.final$fitted.values
    
    # coefficient alpha (SVC)
    lst <- split(1:sum(k_can), rep(1:length(k_can), k_can))
    lst[as.character(which(k_can == 0))] <- 0 # add zero components
    pos <- as.numeric(names(lst))
    
    svc.est <- matrix(NA, nrow(data.train), ncol(data.train))
    c <- 1
    while (c <= p.) {
        if(k_can[c]==0){
            svc.est[,c] <- matrix(0, nrow = nrow(data.train), ncol = 1)
        }else{
            g.train.select <- matrix(g.train[,1:k_can[c]], ncol = k_can[c])
            beta.select    <- matrix(coef.est[lst[[which(pos == c)]]], nrow = k_can[c])
            svc.est[,c] <- g.train.select%*%beta.select
        }
        c <- c+1
    }
    
    # if(k_can[1]==0){
    #     svc.est_0 <- matrix(0, ncol = 1, nrow = grid_n)
    # }else{
    #     svc.est_0 <- matrix(g.train[,1:k_can[1]], ncol = k_can[1])%*%coef.est[(0+1):k_can[1]]
    # }
    # if(k_can[2]==0){
    #     svc.est_1 <- matrix(0, ncol = 1, nrow = grid_n)
    # }else{
    #     svc.est_1 <- matrix(g.train[,1:k_can[2]], ncol = k_can[2])%*%coef.est[(k_can[1]+1):(k_can[1] + k_can[2])]
    # }
    # if(k_can[3]==0){
    #     svc.est_2 <- matrix(0, ncol = 1, nrow = grid_n)
    # }else{
    #     svc.est_2 <- matrix(g.train[,1:k_can[3]], ncol = k_can[3])%*%coef.est[(k_can[2]+1):(k_can[2] + k_can[3])]
    # }
    # if(k_can[4]==0){
    #     svc.est_3 <- matrix(0, ncol = 1, nrow = grid_n)
    # }else{
    #     svc.est_3 <- matrix(g.train[,1:k_can[4]], ncol = k_can[4])%*%coef.est[(k_can[3]+1):(k_can[3] + k_can[4])]
    # }
    
    output <- list(M.hat = k_can,
                   # M.hat.all = k_can_all,
                   coefficients.beta = coef.est,
                   coefficients.alpha = svc.est,
                   fitted.values = p.hat.train,
                   criterion_value = criterion_result_len,
                   basis.functions = g.train,
                   formula = formula,
                   space.train = space.train_mt,
                   X.train = X.train,
                   Y.train = Y.train,
                   modle = model.final)
    return(output)
}