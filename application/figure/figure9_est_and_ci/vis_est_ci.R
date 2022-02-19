rm(list=ls())

mian_folder_path = "~/Documents/3_Research/201810_GWR/script"
folder_path = paste0(mian_folder_path, "/application")

setwd(paste0(folder_path,"/script"))
source("datasets.R")
source("utils.R")
source("models.R")
source("evaluation.R")
source("inference.R")

library(fields)
library(maptools)
library(raster)
library(ggplot2)
library(rgeos)
library(sp)
library(rgdal)

# data ----
data <- load_data(folder_path)
data.train <- data[, c(1,2,3,4)]
space.train <- data[, c(5,6)]

# M hat input ----
M.hat <- c(189,9,21,1)

# prepare for model training
M.up = max(M.hat)
p. <- length(M.hat)
formula = price_unit ~ build_area + age + floor_th
formula <- as.formula(formula)
y_name <- all.vars(formula)[1]
y_ind <- which(colnames(data.train) == y_name)
Y.train <- data.train[,y_ind]
formula.modelmatrix <- as.formula(paste0("~ 1 + ", substr(paste(formula,collapse=" "), start = nchar(y_name) + 4, stop = nchar(paste(formula,collapse=" ")))))
X.train <- model.matrix(formula.modelmatrix, data.train)
phi.train <- compute.datamatrix(X = X.train, 
                                k_can = M.hat, 
                                mrts_knot = space.train, 
                                mrts_x = space.train)
Z.train <- log(Y.train)

# train model ----
##### 1: lm 
model <- lm(Z.train ~ phi.train[,-1]) 
coef.est <- model$coefficients
sig.hat <- summary(model)$sigma
z.hat <- model$fitted.values

##### 2: fastLm
model <- RcppEigen::fastLm(X = phi.train, y = Z.train)
coef.est <- model$coefficients
Y.hat.train <- as.numeric(phi.train %*% coef.est)
sig.hat <- sqrt(mean(model$residuals^2))

# prediction ----
var.est <- summary(model)$sigma^2
double.inv.H <- solve(t(phi.train) %*% phi.train)
b.term.train <- diag(phi.train %*% double.inv.H %*% t(phi.train)) # need some time

Z.hat.train <- as.numeric(phi.train %*% coef.est) - 0.5*var.est*b.term.train
Y.hat.train <- exp(as.numeric(phi.train %*% coef.est) - 0.5*var.est*b.term.train + 0.5*var.est)

# all surface ----
x.min <- 282000
x.max <- 320000
y.min <- 2754000
y.max <- 2790000
x.seq <- seq(x.min, x.max,length.out = 130)
y.seq <- seq(y.min, y.max,length.out = 130)
space.all <- expand.grid(x.seq, y.seq)
colnames(space.all) <- c("X","Y")
g.all  <- mrts(knot = space.train, k = max(M.hat), x = space.all) # NOTE: 用於surface estimation


# estimated SVCs ----
svc.est.all <- matrix(NA, nrow(g.all), p.)
for(i in 1:length(M.hat)){
    
    if(i == 1){
        c <- 0
    }else{
        c <- sum(M.hat[1:(i-1)])
    }
    
    coef_onevar <- coef.est[(c+1):(c+M.hat[i])]
    if(M.hat[i]==0){
        svc.est.all[,i] <- rep(0, nrow(g.all))
    }else{
        svc.est.all[,i] <- matrix(g.all[,1:M.hat[i]], ncol = M.hat[i])%*%coef_onevar # 根據k的數量選幾個g
    }
}



# CI for SVCs ----
dof <- nrow(X.train) - sum(M.hat)
t.lower <- qt(p = 0.025, df = dof)
t.upper <- qt(p = 0.975, df = dof)
sig.hat <- sqrt(sum((Z.train - Z.hat.train)^2)/(dof)) # summary(model)$sigma
double.inv.H <- solve(t(phi.train) %*% phi.train)

svc.ci.upper.all   <- matrix(NA, nrow(g.all)  , p.)
svc.ci.lower.all   <- matrix(NA, nrow(g.all)  , p.)

for(i in 1:length(M.hat)){
    if(i==1){
        c <- 1
    }else{
        c <- sum(M.hat[1:(i-1)])
    }
    
    if(M.hat[i]==0){
        svc.ci.upper.all[,i]   <- rep(0, nrow(g.all))
        svc.ci.lower.all[,i]   <- rep(0, nrow(g.all))
    }else{
        g.j.all   <- matrix(g.all[, 1:M.hat[i]], ncol = M.hat[i])
        H.middle <- double.inv.H[(c+1):(c+M.hat[i]), (c+1):(c+M.hat[i])]
        st.all <- sqrt(diag(g.j.all %*% H.middle %*% t(g.j.all)))
        svc.ci.upper.all[,i] <- svc.est.all[,i] + t.upper * sig.hat * st.all
        svc.ci.lower.all[,i] <- svc.est.all[,i] + t.lower * sig.hat * st.all        
        
    }
}


# map data ----
setwd(paste0(folder_path,"/data"))
tp.map <- readShapeSpatial("COUNTY_MOI_1081121.shp") # https://data.gov.tw/dataset/7442
map <- tp.map[tp.map$COUNTYID %in% tp.map$COUNTYID[c(7,9,10)] ,] # select region
crs(map) <- "+init=epsg:4326" # lon/lat ro tw97 system
map.97 <- sp::spTransform(map, CRS("+init=epsg:3826")) 
col_defines <- c("yellow", "orange", "red", "red4")

# visualization ----
setwd(paste0(folder_path,"/figure/figure9_est_and_ci"))
svc.ci.all   <- svc.ci.upper.all   - svc.ci.lower.all
for(i in 1:p.){
    # limits setting ----
    min.v <- min(c(svc.est.all[,i],svc.ci.upper.all[,i],svc.ci.lower.all[,i]))
    max.v <- max(c(svc.est.all[,i],svc.ci.upper.all[,i],svc.ci.lower.all[,i]))
    lim <- max(abs(min.v), abs(max.v)) # 正負極值設絕對值一樣的，方可比較正負
    rd <- c(1, 3, 3, 2) # legend的小數點位數
    
    # svc.est.all  ----
    sp <- as.data.frame(space.all)
    df <- data.frame(lon = sp$X, 
                     lat = sp$Y,
                     value = svc.est.all[,i])
    png(filename = paste0("svc_est_all_",i,".png"), width = 19, height = 16, units = 'cm', res = 600)
    print(
        ggplot(data = df,
               aes(x = lon, 
                   y = lat)) +
            geom_tile(aes(fill = value)) +
            scale_fill_gradientn(colors = c("#0000FF","#FFFFFF","#FF0000"),
                                 breaks = c(-lim,0,lim),
                                 limits = c(-lim,lim),
                                 labels = round(seq(-lim, lim, length.out = 3),rd[i])) +
            geom_path(data = map.97, 
                      aes(x = long, y = lat, group = group),
                      color = 'black', size = .2) +
            scale_x_continuous(limits = c(282000 , 320000), expand = c(0, 0)) + 
            scale_y_continuous(limits = c(2755000, 2790000), expand = c(0, 0)) +
            theme(
                legend.title=element_blank(),
                panel.background = element_rect(colour = "black", fill=NA, size=1),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                axis.title.x=element_blank(),
                axis.text.x =element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y =element_blank(),
                axis.ticks.y=element_blank(),
                plot.margin = margin(0.1, 0, 0, 0, "cm"),
                legend.text=element_text(size=22)) +
            guides(fill = guide_colourbar(barwidth = 1, barheight = 26))
    )
    dev.off()
    
    # svc.ci.upper.all   ----
    sp <- as.data.frame(space.all)
    df <- data.frame(lon = sp$X, 
                     lat = sp$Y,
                     value = svc.ci.upper.all[,i])
    png(filename = paste0("svc_ci_upper_all_",i,".png"), width = 19, height = 16, units = 'cm', res = 600)
    print(
        ggplot(data = df,
               aes(x = lon, 
                   y = lat)) +
            geom_tile(aes(fill = value)) +
            scale_fill_gradientn(colors = c("#0000FF","#FFFFFF","#FF0000"),
                                 breaks = c(-lim,0,lim),
                                 limits = c(-lim,lim),
                                 labels = round(seq(-lim, lim, length.out = 3),rd[i])) +
            geom_path(data = map.97, 
                      aes(x = long, y = lat, group = group),
                      color = 'black', size = .2) +
            scale_x_continuous(limits = c(282000 , 320000), expand = c(0, 0)) + 
            scale_y_continuous(limits = c(2755000, 2790000), expand = c(0, 0)) +
            theme(
                legend.title=element_blank(),
                panel.background = element_rect(colour = "black", fill=NA, size=1),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                axis.title.x=element_blank(),
                axis.text.x =element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y =element_blank(),
                axis.ticks.y=element_blank(),
                plot.margin = margin(0.1, 0, 0, 0, "cm"),
                legend.text=element_text(size=22)) +
            guides(fill = guide_colourbar(barwidth = 1, barheight = 26))
    )
    dev.off()
    # svc.ci.lower.all   ----
    sp <- as.data.frame(space.all)
    df <- data.frame(lon = sp$X, 
                     lat = sp$Y,
                     value = svc.ci.lower.all[,i])
    png(filename = paste0("svc_ci_lower_all_",i,".png"), width = 19, height = 16, units = 'cm', res = 600)
    print(
        ggplot(data = df,
               aes(x = lon, 
                   y = lat)) +
            geom_tile(aes(fill = value)) +
            scale_fill_gradientn(colors = c("#0000FF","#FFFFFF","#FF0000"),
                                 breaks = c(-lim,0,lim),
                                 limits = c(-lim,lim),
                                 labels = round(seq(-lim, lim, length.out = 3),rd[i])) +
            geom_path(data = map.97, 
                      aes(x = long, y = lat, group = group),
                      color = 'black', size = .2) +
            scale_x_continuous(limits = c(282000 , 320000), expand = c(0, 0)) + 
            scale_y_continuous(limits = c(2755000, 2790000), expand = c(0, 0)) +
            theme(
                legend.title=element_blank(),
                panel.background = element_rect(colour = "black", fill=NA, size=1),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                axis.title.x=element_blank(),
                axis.text.x =element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y =element_blank(),
                axis.ticks.y=element_blank(),
                plot.margin = margin(0.1, 0, 0, 0, "cm"),
                legend.text=element_text(size=22)) +
            guides(fill = guide_colourbar(barwidth = 1, barheight = 26))
    )
    dev.off()
}