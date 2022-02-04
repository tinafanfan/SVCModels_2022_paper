RMSE_beta_y <- function(coef, coef_hat, Y, Y_hat){
    result  = data.frame(beta0 = sqrt(mean((coef[,1] - coef_hat[,1])^2)),
                         beta1 = sqrt(mean((coef[,2] - coef_hat[,2])^2)),
                         beta2 = sqrt(mean((coef[,3] - coef_hat[,3])^2)),
                         y     = sqrt(mean((Y - Y_hat)^2)))
    return(result)
}