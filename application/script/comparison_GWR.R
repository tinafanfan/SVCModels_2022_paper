
rm(list=ls())

folder_path = "~/Documents/SVCModels_2021/applicatin/script/"
setwd(folder_path)

source("datasets.R")
source("utils.R")
source("models.R")
source("evaluation.R")
source("inference.R")

data <- load_data(folder_path)

# # 資料分析
set.seed(10)
number.data  <- nrow(data)*2/3
number.test <- nrow(data)*1/3
I <- sample(1:nrow(data), number.data)
length(I)
# training data
data_attrb_train <- data[I, c(1,2,3,4)]
data_space_train <- data[I, c(5,6)]
# test data
data_attrb_test  <- data[-I, c(1,2,3,4)]
data_space_test <- data[-I, c(5,6)]

start.time <- Sys.time()

data_train <- SpatialPointsDataFrame(coords = data_space_train, data = data_attrb_train)
data_test <- SpatialPointsDataFrame(coords = data_space_test, data = data_attrb_test)
data_all <- rbind(data_train, data_test)

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


# Normal
# model = GWmodel.fit(data_train, data_test)
# setwd("~/Documents/github-folder-name")
# save(model, file = "GWR.RData")

# setwd("~/Documents/github-folder-name")
# load("GWR.RData")
# print(RMSE(model$SDF@data$prediction, data_test@data$price_unit))
# res = model$SDF@data$prediction - data_attrb_test$price_unit
# sig = sqrt(sum((res)^2)/length(res))
# print(sig)
# print(mean(scoringRules::crps(data_attrb_test$price_unit, 
#                         family = "normal", 
#                         mean = model$SDF@data$prediction, 
#                         sd = sig)))




# Log-Normal
data_train$price_unit = log(data_train$price_unit)

model = GWmodel.fit(data_train, data_all)
setwd("~/Documents/github-folder-name")
save(model, file = "GWR_logN.RData")

# setwd("~/Documents/github-folder-name")
# load(file = "GWR_logN.RData")
res = model$SDF@data$prediction[1:19017] - data_train$price_unit
sig = sqrt(sum((res)^2)/length(res))
print(sig)
print(RMSE(exp(model$SDF@data$prediction[19018:28526] + 0.5*sig), data_test@data$price_unit))
print(mean(scoringRules::crps(data_test@data$price_unit, 
                        family = "log-normal", 
                        meanlog = model$SDF@data$prediction[19018:28526], 
                        sdlog = sig)))


# end.time <- Sys.time()
# print(end.time - start.time)