# Calculate SVC estimation ----
# g is the basis functions obtained from autoFRK::mrts and their locations are the same to svc_est
svc_est <- function(g, M_hat, coef_est){
    p. <- length(M_hat)
    svc_est_mt <- matrix(NA, nrow(g), p.)
    for(i in 1:length(M_hat)){
        
        if(i == 1){
            c <- 0
        }else{
            c <- sum(M_hat[1:(i-1)])
        }
        coef_onevar <- coef_est[(c+1):(c+M_hat[i])]
        if(M_hat[i]==0){
            svc_est_mt[,i] <- rep(0, nrow(g))
        }else{
            svc_est_mt[,i] <- matrix(g[,1:M_hat[i]], ncol = M_hat[i])%*%coef_onevar # 根據k的數量選幾個g
        }
    }
    return(svc_est_mt)
}


# Calculate CI of SVC estimation ----
svc_est_ci <- function(svc_est_mt, M_hat, Y_train, Y_train_hat, X_train, phi_train, g){
    p. = length(M_hat)
    
    dof <- nrow(X_train) - sum(M_hat)
    t_lower <- qt(p = 0.025, df = dof)
    t_upper <- qt(p = 0.975, df = dof)
    sig_hat <- sqrt(sum((Y_train - Y_train_hat)^2)/(dof)) # summary(model)$sigma
    double_inv_H <- solve(t(phi_train) %*% phi_train)
    
    svc_ci_upper   <- matrix(NA, nrow(g)  , p.)
    svc_ci_lower   <- matrix(NA, nrow(g)  , p.)
    
    for(i in 1:length(M_hat)){
        if(i==1){
            c <- 1
        }else{
            c <- sum(M_hat[1:(i-1)])
        }
        
        if(M_hat[i]==0){
            svc_ci_upper[,i]   <- rep(0, nrow(g))
            svc_ci_lower[,i]   <- rep(0, nrow(g))
        }else{
            g_subset   <- matrix(g[, 1:M_hat[i]], ncol = M_hat[i])
            
            H_middle <- double_inv_H[(c+1):(c+M_hat[i]), (c+1):(c+M_hat[i])]
            
            st <- sqrt(diag(g_subset %*% H_middle %*% t(g_subset)))
            svc_ci_upper[,i] <- svc_est_mt[,i] + t_upper * sig_hat * st
            svc_ci_lower[,i] <- svc_est_mt[,i] + t_lower * sig_hat * st        
            
        }
    }
    
    return(list(svc_ci_upper = svc_ci_upper,
                svc_ci_lower = svc_ci_lower))
}

