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

i = 1
set.seed(i)

# data ----
number.data  <- round(nrow(data)*2/3,0)
number.test <- nrow(data) - number.data
I <- sample(1:nrow(data), number.data)
print(length(I))
# training data
data_attrb_train <- data[I, c(1,2,3,4)]
data_space_train <- data[I, c(5,6)]
# test data
data_attrb_test  <- data[-I, c(1,2,3,4)]
data_space_test <- data[-I, c(5,6)]

data_train <- SpatialPointsDataFrame(coords = data_space_train, data = data_attrb_train)
data_test <- SpatialPointsDataFrame(coords = data_space_test, data = data_attrb_test)

# train model ----
library(varycoef)
y.col <- 1
p = 4
start_time = Sys.time()
control <- SVC_mle_control(profileLik = TRUE)
model.fit <- SVC_mle(y       = as.numeric(data_train@data[, y.col]),
                     X       = as.matrix(cbind(1,data_train@data[, -y.col])),
                     locs    = as.matrix(data_train@coords),
                     control = control)
stop_time = Sys.time()
print(stop_time - start_time)
# save(model.fit, file = paste0("MLE_model_normal_",i,".RData"))

# prediction ----
locs = data_test@coords
p = 4
SVC_nomean <- predict(object = model.fit, newlocs = locs)[, 1:p]
beta_hat = SVC_nomean + matrix(rep(coef(model.fit), nrow(locs)), ncol = p, byrow = TRUE)
print(dim(cbind(1,data_test@data[,-y.col])))
print(dim(beta_hat))
y_hat_test = apply(cbind(1,data_test@data[,-y.col])*beta_hat,1,sum)

rmse = RMSE(y_hat_test, data_test@data$price_unit)
print(rmse)

res =  model.fit$residuals
sig = sqrt(sum((res)^2)/length(res))
crps = mean(scoringRules::crps(data_test@data$price_unit,
                        family = "normal",
                        mean = y_hat_test,
                        sd = sig))
print(crps)
# save(rmse,crps, file = paste0("MLE_result_normal_",i,".RData"))
