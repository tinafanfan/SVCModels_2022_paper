
rm(list=ls())

folder_path = "~/Documents/github-folder-name"
# folder_path = "~/Documents/3_Research/201810_GWR/github-folder-name"
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

library(GWmodel)
GWmodel.fit <- function(data_train, data_test) {
    # distance matrices
    dm.train <- gw.dist(dp.locat = coordinates(data_train))
    dm.pred <- gw.dist(dp.locat = coordinates(data_train),
                       rp.locat = coordinates(data_test))
    
    # formula
    p <- ncol(data_train)-1
    form <- as.formula(price_unit ~ build_area + age + floor_th)
    # CV for bandwidth
#     bw.sel <- bw.gwr(formula = form, data = data_train,
#                      approach = "CV", kernel = "bisquare", adaptive = FALSE,
#                      dMat = dm.train)
    
    GWR.model <- gwr.predict(formula = form,
                             data = data_train,
                             predictdata = data_test,
#                              bw = bw.sel, 
                             bw = 2950.94,
                             kernel = "bisquare", 
                             dMat1 = dm.pred, 
                             dMat2 = dm.train)
}

model = GWmodel.fit(data_train, data_test)
# save(model, file = "GWR.RData")
print(RMSE(model$SDF@data$prediction, data_test@data$price_unit))

end.time <- Sys.time()
print(end.time - start.time)