
rm(list=ls())

folder_path = "~/Documents/github-folder-name"
setwd(folder_path)

source("datasets.R")
source("utils.R")
source("models.R")
source("evaluation.R")
source("inference.R")

data <- load_data(folder_path)

# 測時間用
# data_sep_fold <- data_separation_all(data, seed = 10, fold = 10)

# data_attrb_train <- data_sep_fold[[1]]$validation@data
# data_space_train <- data_sep_fold[[1]]$validation@coords

# data_attrb_valid <- data_sep_fold[[2]]$validation@data
# data_space_valid <- data_sep_fold[[2]]$validation@coords

# 資料分析
set.seed(10)
number.data  <- nrow(data)*2/3
number.test <- nrow(data)*1/3
I <- sample(1:nrow(data), number.data)
length(I)
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
                        criterion_setting = "aic", # c("aic", "bic", "cv_rmse", "cv_mse", "cv_mae")
                        M_up = 800,
                        log_normal = FALSE)
print("result")
prediction <- predict.mrbs.svc(result, 
                             data_attrb_valid, 
                             data_space_valid,
                             log_normal = FALSE)


print(RMSE(result$Y_hat, data_attrb_train$price_unit))
print(RMSE(prediction$Y_hat, data_attrb_valid$price_unit))

res = result$Y_hat - data_attrb_train$price_unit
sig = sqrt(sum((res)^2)/length(res))
print(sig)
print(mean(scoringRules::crps(data_attrb_valid$price_unit, 
                        family = "normal", 
                        mean = prediction$Y_hat, 
                        sd = sig)))





end.time <- Sys.time()
print(end.time - start.time)