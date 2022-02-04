rm(list=ls())

folder_path = "/Users/tina/Documents/3_Research/201810_GWR/github-folder-name"
setwd(folder_path)

source("datasets.R")
source("utils.R")
source("evaluation.R")

data <- load_data(folder_path)

set.seed(10)
number.data  <- nrow(data)*2/3
number.test <- nrow(data)*1/3
I <- sample(1:nrow(data), number.data)

# training data
data_attrb_train <- data[I, c(1,2,3,4)]
data_space_train <- data[I, c(5,6)]

# test data
data_attrb_valid  <- data[-I, c(1,2,3,4)]
data_space_valid <- data[-I, c(5,6)]

M_hat = c(174, 132, 189, 154)
M_hat = c(178, 130, 186, 76)

prediction <- predict.mrbs.svc.simple(M_hat = c(174, 132, 189, 154),
                        formula = price_unit ~ build_area + age + floor_th,
                        data_attrb_train = data_attrb_train,
                        data_space_train = data_space_train,
                        data_attrb_valid = data_attrb_train,
                        data_space_valid = data_space_train,
                        log_normal = FALSE)

RMSE(prediction$Y_hat, data_attrb_train$price_unit)

prediction <- predict.mrbs.svc.simple(M_hat = c(174, 132, 189, 154),
                                      formula = price_unit ~ build_area + age + floor_th,
                                      data_attrb_train = data_attrb_train,
                                      data_space_train = data_space_train,
                                      data_attrb_valid = data_attrb_valid, 
                                      data_space_valid = data_space_valid,
                                      log_normal = FALSE)

RMSE(prediction$Y_hat, data_attrb_valid$price_unit)

mean(scoringRules::crps(data_attrb_valid$price_unit, 
                        family = "normal", 
                        mean = as.numeric(prediction$Y_hat),
                        sd = rep(prediction$sigma_est, length(prediction$Y_hat))))

prediction <- predict.mrbs.svc.simple(M_hat = c(178, 130, 186, 76),
                                      formula = price_unit ~ build_area + age + floor_th,
                                      data_attrb_train = data_attrb_train,
                                      data_space_train = data_space_train,
                                      data_attrb_valid = data_attrb_train,
                                      data_space_valid = data_space_train,
                                      log_normal = TRUE)

RMSE(prediction$Y_hat, data_attrb_train$price_unit)

prediction <- predict.mrbs.svc.simple(M_hat = c(178, 130, 186, 76),
                                      formula = price_unit ~ build_area + age + floor_th,
                                      data_attrb_train = data_attrb_train,
                                      data_space_train = data_space_train,
                                      data_attrb_valid = data_attrb_valid, 
                                      data_space_valid = data_space_valid,
                                      log_normal = TRUE)

RMSE(prediction$Y_hat, data_attrb_valid$price_unit)
mean(scoringRules::crps(data_attrb_valid$price_unit, 
                        family = "log-normal",
                        meanlog = prediction$Z_hat, 
                        sdlog = rep(prediction$sigma_est, length(prediction$Z_hat))))
