rm(list = ls())

Packages <- c("varycoef","ggplot2", "autoFRK","reshape2","MASS","fields", "reshape","spgwr","GWmodel", "sp", "geoR", "spmoran","colorRamps")
invisible(lapply(Packages, library, character.only = TRUE))

mian_folder_path = "~/Documents/3_Research/201810_GWR/script"
folder_path = paste0(mian_folder_path, "/simulation/simulation_1")

setwd(paste0(mian_folder_path,"/application/script"))
source("inference.R")
source("models.R")
source("utils.R")

setwd(paste0(folder_path,"/script"))
source("dataset.R")
source("models.R")
source("evaluation.R")

env <- ls()
n.cores <- detectCores() - 1
cl <- makeCluster(n.cores)
clusterEvalQ(cl, {
  library(sp)
  library(RandomFields)
  library(spam)
  library(spmoran)
  library(varycoef)
  library(spdep)
  library(spgwr)
  library(GWmodel)
})
clusterExport(cl, varlist = env)
setDefaultCluster(cl = cl)

tapering.range <- NULL
init.par       <- c(scale = 0.075, var = 0.3, mu = 0) # initial value for SVC-MLE
p = 3
init <- unname(c(rep(init.par[1:2], p), 
                      init.par["var"], rep(init.par[3], p)))


pred_MLE <- matrix(NA, 100, 4)
for(i in 1:100){
    print(i)
    gen_data <- generate_data(grid_n = 625, grid_min = 0, grid_max = 25, grid_length = 25)
    coef <- gen_data$coef
    data_sp <- gen_data$data_sp
    
    
    # model <- GWmodel.fit(data_sp)
    # prediction <- GWmodel.pred(model)
    # pred_gwr[i,] <- as.numeric(RMSE_beta_y(coef = coef, coef_hat = prediction$beta_hat, Y =  data_sp@data$y, Y_hat = prediction$y_hat))


    model <- mrbs.svc.fit(data_sp)
    prediction <- mrbs.svc.pred(model)
    pred_mbs[i, ] <- as.numeric(RMSE_beta_y(coef = coef, coef_hat = prediction$beta_hat, Y =  gen_data$true_y_vt, Y_hat = prediction$y_hat))
    
    
    model <- SVC.MLE.profile_pc.fit(data_sp, tapering.range, init, p.dens = NULL, cl = cl)
    prediction <- SVC.MLE.pred(model)
    pred_MLE[i, ] <- as.numeric(RMSE_beta_y(coef = coef, coef_hat = prediction$beta_hat, Y =  gen_data$true_y_vt, Y_hat = prediction$y_hat))
}

# save(pred_mbs, file = "our_cv.RData")
# save(pred_mbs, file = "our_aic.RData")
# save(pred_MLE, file = "MLE.RData")

