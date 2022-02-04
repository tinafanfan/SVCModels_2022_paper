
rm(list=ls())

folder_path = "~/Documents/github-folder-name"
setwd(folder_path)

source("datasets.R")
source("utils.R")
source("models.R")
source("evaluation.R")
source("inference.R")

data <- load_data(folder_path)

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
data_attrb_test  <- data[-I, c(1,2,3,4)]
data_space_test <- data[-I, c(5,6)]

start.time <- Sys.time()

data_train <- SpatialPointsDataFrame(coords = data_space_train, data = data_attrb_train)
data_test <- SpatialPointsDataFrame(coords = data_space_test, data = data_attrb_test)

taper.range <- 0.2
init        <- c(scale = 0.075, var = 0.3, mu = 0) # initial value for SVC-MLE

library(varycoef)
SVC.MLE.profile_pc.fit <- function(data_train, taper.range, init, p.dens = NULL, cl = NULL) {
    train.data <- cbind(data_train@data,1)
    y.col <- 1
    p = 4
#     control <- SVC_mle_control(init       = init[1:(2*p+1)], 
#                                tapering   = taper.range, 
#                                profileLik = TRUE, 
#                                lower      = c(rep(1e-10, 2*p+1)), 
#                                upper      = c(rep(Inf, 2*p+1)),
#                                pc.prior   = c(0.075, 0.05, sqrt(0.25), 0.05),
#                                parallel   = if (is.null(cl)) {
#                                    NULL
#                                } else {
#                                    list(cl = cl, forward = FALSE, loginfo = TRUE)
#                                })
    control <- SVC_mle_control()
    
    model.fit <- SVC_mle(y       = as.numeric(train.data[, y.col]),
                         X       = as.matrix(train.data[, -y.col]),
                         locs    = as.matrix(data_train@coords),
                         control = control)
    
}
SVC.MLE.pred <- function(model, locs) {
    p = 4
    SVC_nomean <- predict(object = model, newlocs = locs)[, 1:p]
    
    beta_hat = SVC_nomean + matrix(rep(coef(model), nrow(locs)), ncol = p, byrow = TRUE)
    y_hat = apply(model$data$X*beta_hat,1,sum)
    return(y_hat)
}



model = SVC.MLE.profile_pc.fit(data_train, taper.range, init)
beta_hat = model$fitted[,1:4] + matrix(rep(coef(model), nrow(model$fitted)), ncol = 4, byrow = TRUE)
y_hat_train = apply(model$data$X*beta_hat,1,sum)

y_hat_test = SVC.MLE.pred(model, locs = data_test@coords)

setwd("~/Documents/github-folder-name")
save(model, y_hat_train, y_hat_test, file = "MLE.RData")

print(RMSE(y_hat_train, data_train@data$price_unit))
print(RMSE(y_hat_test, data_test@data$price_unit))

res =  data_train@data$price_unit - y_hat_train
sig = sqrt(sum((res)^2)/length(res))
print(sig)
print(mean(scoringRules::crps(data_test@data$price_unit, 
                        family = "normal", 
                        mean = y_hat_test, 
                        sd = sig)))
