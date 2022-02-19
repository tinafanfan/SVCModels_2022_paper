rm(list = ls())

Packages <- c("ggplot2", "autoFRK","reshape2","MASS","fields", "reshape","spgwr","GWmodel", "sp", "geoR", "spmoran","colorRamps")
# install.packages(Packages)
invisible(lapply(Packages, library, character.only = TRUE))

mian_folder_path = "~/Documents/3_Research/201810_GWR/script"
folder_path = paste0(mian_folder_path, "/simulation/simulation_2")

setwd(paste0(mian_folder_path, "/application/script"))
source("inference.R")
source("models_glm.R")
source("utils.R")

setwd(paste0(folder_path, "/script"))
source("dataset.R")
source("models.R")
source("evaluation.R")

rep_times = 100

result_aic <- matrix(NA, rep_times, 5)
result_bic <- matrix(NA, rep_times, 5)
result_cv <- matrix(NA, rep_times, 5)
result_ggwr <- matrix(NA, rep_times, 5)

for(i in 1:rep_times){
    print(i)
    set.seed(i)
    gen_data <- generate_data()
    coef <- gen_data$coef
    data_sp <- gen_data$data_sp
    p_vt <- gen_data$p_vt
    true_y <- gen_data$true_y_vt

    # ours(aic)
    model <- mrbs.svc.fit_aic(data_sp)
    prediction <- mrbs.svc.pred(model)

    diffs = prediction$beta_hat - coef
    RISE_beta = apply(diffs, 2, function(x) sqrt(sum((1/length(x))*(x)^2)))
    
    pred_y = prediction$y_hat
    # true_y_lg = exp(true_y)/(1+exp(true_y))
    # pred_y_lg = exp(pred_y)/(1+exp(pred_y))
    ent = -(p_vt*log(pred_y) + (1-p_vt)*log(1-pred_y))
    RISE_y = apply(ent, 2, function(x) sqrt(sum((1/length(x))*(x)^2)))
    
    result_aic[i,] = c(RISE_beta, RISE_y)
    
    # ours(bic)
    model <- mrbs.svc.fit_bic(data_sp)
    prediction <- mrbs.svc.pred(model)
    
    diffs = prediction$beta_hat - coef
    RISE_beta = apply(diffs, 2, function(x) sqrt(sum((1/length(x))*(x)^2)))
    
    pred_y = prediction$y_hat
    # true_y_lg = exp(true_y)/(1+exp(true_y))
    # pred_y_lg = exp(pred_y)/(1+exp(pred_y))
    ent = -(p_vt*log(pred_y) + (1-p_vt)*log(1-pred_y))
    RISE_y = apply(ent, 2, function(x) sqrt(sum((1/length(x))*(x)^2)))
    
    result_bic[i,] = c(RISE_beta, RISE_y)
    
    # ours(cv)
    model <- mrbs.svc.fit_cv(data_sp)
    prediction <- mrbs.svc.pred(model)
    
    diffs = prediction$beta_hat - coef
    RISE_beta = apply(diffs, 2, function(x) sqrt(sum((1/length(x))*(x)^2)))
    
    pred_y = prediction$y_hat
    # true_y_lg = exp(true_y)/(1+exp(true_y))
    # pred_y_lg = exp(pred_y)/(1+exp(pred_y))
    ent = -(p_vt*log(pred_y) + (1-p_vt)*log(1-pred_y))
    RISE_y = apply(ent, 2, function(x) sqrt(sum((1/length(x))*(x)^2)))
    
    result_cv[i,] = c(RISE_beta, RISE_y)
    
    # ggwr
    ggwr_model <- ggwr.fit(data_sp)
    prediction <- ggwr.pred(ggwr_model)
    
    diffs = prediction$beta_hat - coef
    RISE_beta = apply(diffs, 2, function(x) sqrt(sum((1/length(x))*(x)^2)))
    
    pred_y = prediction$y_hat
    # true_y_lg = exp(true_y)/(1+exp(true_y))
    # pred_y_lg = exp(pred_y)/(1+exp(pred_y))
    ent = -(p_vt*log(pred_y) + (1-p_vt)*log(1-pred_y))
    RISE_y = apply(ent, 2, function(x) sqrt(sum((1/length(x))*(x)^2)))
    
    result_ggwr[i,] = c(RISE_beta, RISE_y)
    
}

colnames(result_aic) <- colnames(result_bic) <- colnames(result_cv) <- colnames(result_ggwr) <-   c("beta_0", "beta_1", "beta_2", "beta_3", "y")
setwd(paste0(folder_path, "/figure/figure6"))
save(result_aic,result_bic,result_cv,result_ggwr, file = "sim2_result.RData")

