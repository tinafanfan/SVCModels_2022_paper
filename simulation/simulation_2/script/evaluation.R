RMSE_beta_y <- function(coef, coef_hat, Y, Y_hat){
    result  = data.frame(beta0 = sqrt(mean((coef[,1] - coef_hat[,1])^2)),
                         beta1 = sqrt(mean((coef[,2] - coef_hat[,2])^2)),
                         beta2 = sqrt(mean((coef[,3] - coef_hat[,3])^2)),
                         beta3 = sqrt(mean((coef[,4] - coef_hat[,4])^2)),
                         y     =  -mean(Y*log(Y_hat) + (1-Y)*log(1-Y_hat)))
    return(result)
}