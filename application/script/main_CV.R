rm(list=ls())
mian_folder_path = "~/Documents/3_Research/201810_GWR/script"
folder_path = paste0(mian_folder_path, "/application")

setwd(paste0(folder_path,"/script"))

source("datasets.R")
source("utils.R")
source("models.R")
source("evaluation.R")
source("inference.R")

data <- load_data(folder_path)


cv_run <- function(i){
    
    set.seed(i)
    
    data <- load_data(folder_path)
    
    np <- nrow(data)/10
    data = data[sample(nrow(data), np),]
    print(nrow(data))


    number.data  <- round(nrow(data)*2/3,0)
    number.test <- nrow(data) - number.data
    I <- sample(1:nrow(data), number.data)

    # training data
    data_attrb_train <- data[I, c(1,2,3,4)]
    data_space_train <- data[I, c(5,6)]

    # test data
    data_attrb_valid  <- data[-I, c(1,2,3,4)]
    data_space_valid <- data[-I, c(5,6)]

    
    start.time <- Sys.time()
    result = mrbs.multi.svc(formula = price_unit ~ build_area + age + floor_th,
                            data_attrb = data_attrb_train,
                            data_space = data_space_train,
                            criterion_setting = "bic", # c("aic", "bic", "cv_rmse", "cv_mse", "cv_mae")
                            M_up = 200,
                            log_normal = FALSE)
    M_hat_N = result$M_hat
    bic_N = result$bic

    prediction <- predict.mrbs.svc(result,
                                   data_attrb_valid,
                                   data_space_valid,
                                   log_normal = FALSE)
    # save(result, prediction, file = paste0("OURS_",i,".RData"))
    
    res = result$Y_hat - data_attrb_train$price_unit
    sig = sqrt(sum((res)^2)/length(res))
    RMSE_N <- RMSE(prediction$Y_hat, data_attrb_valid$price_unit)
    CRPS_N <- mean(scoringRules::crps(data_attrb_valid$price_unit,
                                      family = "normal",
                                      mean = prediction$Y_hat[,1],
                                      sd = sig))
    
    end.time <- Sys.time()
    time_N <- end.time - start.time
    
    start.time <- Sys.time()
    result = mrbs.multi.svc(formula = price_unit ~ build_area + age + floor_th,
                            data_attrb = data_attrb_train,
                            data_space = data_space_train,
                            criterion_setting = "bic", # c("aic", "bic", "cv_rmse", "cv_mse", "cv_mae")
                            M_up = 200,
                            log_normal = TRUE)
    M_hat_LogN = result$M_hat
    bic_LogN = result$bic
    prediction <- predict.mrbs.svc(result,
                                 data_attrb_valid,
                                 data_space_valid,
                                 log_normal = TRUE)
    # save(result, prediction, file = paste0("OURS_logN_",i,".RData"))
    

    res = result$Z_hat - log(data_attrb_train$price_unit)
    sig = sqrt(sum((res)^2)/length(res))
    RMSE_LogN <- RMSE(prediction$Y_hat, data_attrb_valid$price_unit)
    CRPS_LogN <- mean(scoringRules::crps(data_attrb_valid$price_unit,
                                         family = "log-normal",
                                         meanlog = prediction$Z_hat,
                                         sdlog = sig))
    
    end.time <- Sys.time()
    time_LogN <- end.time - start.time
    
    return(list(value = c(RMSE_N, CRPS_N, time_N, RMSE_LogN, CRPS_LogN, time_LogN)))
    
}

rep_times <- 10
result <- matrix(NA, rep_times, 6)

for(i in 1:rep_times){
    print(i)
    out = cv_run(i)
    result[i, ] <- out$value
}
colnames(result) <- c("RMSE_N", "CRPS_N", "time_N", "RMSE_LogN", "CRPS_LogN", "time_LogN")

setwd(paste0(folder_path,"/table/table4"))
save(result, file = paste0("OURS_result_10_times_sample.rdata")) # all data (no test data)

