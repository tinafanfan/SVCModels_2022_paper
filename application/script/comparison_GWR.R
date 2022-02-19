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

library(GWmodel)
GWmodel.fit <- function(data_train, data_all) {
    # distance matrices
    dm.train <- gw.dist(dp.locat = coordinates(data_train))
    dm.pred <- gw.dist(dp.locat = coordinates(data_train),
                       rp.locat = coordinates(data_all))
    
    # formula
    p <- ncol(data_train)-1
    form <- as.formula(price_unit ~ build_area + age + floor_th)
    # CV for bandwidth
    bw.sel <- bw.gwr(formula = form, data = data_train,
                     approach = "CV", kernel = "bisquare", adaptive = FALSE,
                     dMat = dm.train)
    print("start predicting")
    GWR.model <- gwr.predict(formula = form,
                             data = data_train,
                             predictdata = data_all,
                             bw = bw.sel, # Normal:2950.94
                             kernel = "bisquare", 
                             dMat1 = dm.pred, 
                             dMat2 = dm.train)
}

cv_run <- function(i){
#     set.seed(10)
    set.seed(i)
    # Data
    data <- load_data(folder_path)

    number.data  <- round(nrow(data)*2/3,0)
    number.test <- nrow(data) - number.data
    
    train_lable <- 1:number.data
    test_lable <- (number.data + 1):(number.data + number.test)
    
    I <- sample(1:nrow(data), number.data)

    # training data
    data_attrb_train <- data[I, c(1,2,3,4)]
    data_space_train <- data[I, c(5,6)]
    # test data
    data_attrb_test  <- data[-I, c(1,2,3,4)]
    data_space_test <- data[-I, c(5,6)]

    data_train <- SpatialPointsDataFrame(coords = data_space_train, data = data_attrb_train)
    data_test <- SpatialPointsDataFrame(coords = data_space_test, data = data_attrb_test)
    data_all <- rbind(data_train, data_test)


    # Normal
    start.time <- Sys.time()


    model = GWmodel.fit(data_train, data_all)
    # save(model, file = paste0("GWR_",i,".RData"))

    res = model$SDF@data$prediction[train_lable] - data_train$price_unit
    sig = sqrt(sum((res)^2)/length(res))

    RMSE_N <- RMSE(model$SDF@data$prediction[test_lable], data_test@data$price_unit)
    CRPS_N <- mean(scoringRules::crps(data_test@data$price_unit, 
                                      family = "normal", 
                                      mean = model$SDF@data$prediction[test_lable],
                                      sd = sig))

    end.time <- Sys.time()
    time_N <- end.time - start.time


    # Log-Normal
    start.time <- Sys.time()

    data_train$price_unit = log(data_train$price_unit)
    model = GWmodel.fit(data_train, data_all)
    # save(model, file = paste0("GWR_logN_",i,".RData"))

    res = model$SDF@data$prediction[train_lable] - data_train$price_unit
    sig = sqrt(sum((res)^2)/length(res))
    RMSE_LogN <- RMSE(exp(model$SDF@data$prediction[test_lable] + 0.5*sig), data_test@data$price_unit)
    CRPS_LogN <- mean(scoringRules::crps(data_test@data$price_unit,
                                         family = "log-normal", 
                                         meanlog = model$SDF@data$prediction[test_lable], 
                                         sdlog = sig))


    end.time <- Sys.time()
    time_LogN <- end.time - start.time
    return(c(RMSE_N, CRPS_N, time_N, RMSE_LogN, CRPS_LogN, time_LogN))
}

rep_times <- 20
result <- matrix(NA, rep_times, 6)
# for(i in 1:rep_times){
i = 10
    print(i)
    result[i, ] <- cv_run(i)
#     print(result[1:i,])
# }
colnames(result) <- c("RMSE_N", "CRPS_N", "time_N", "RMSE_LogN", "CRPS_LogN", "time_LogN")
setwd(paste0(folder_path,"/result/table4"))
save(result, file = paste0("GWR_result_",i,"_times_sample.RData"))
