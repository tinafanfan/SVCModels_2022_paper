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
library(colorRamps)

# data ----
data <- load_data(folder_path)
data.train <- data[, c(1,2,3,4)]
space.train <- data[, c(5,6)]

# basis functions ----
M.up = 500
g.train <- autoFRK::mrts(knot = space.train, k = M.up, x = space.train) # NOTE: 用於surface estimation

y_ind <- which(colnames(data.train) == "price_unit")
Y.train <- data.train[,y_ind]

# surface
x.min <- 282000
x.max <- 320000
y.min <- 2754000
y.max <- 2790000
x.seq <- seq(x.min, x.max,length.out = 130)
y.seq <- seq(y.min, y.max,length.out = 130)
space.all <- expand.grid(x.seq, y.seq)
colnames(space.all) <- c("X","Y")
g.all  <- mrts(knot = space.train_mt, k = M.up, x = space.all) # NOTE: 用於surface estimation
g.all.mt <- as.matrix(g.all)


# map data ----
setwd(paste0(folder_path,"/data")) 
tp.map <- readShapeSpatial("COUNTY_MOI_1081121.shp") # https://data.gov.tw/dataset/7442
map <- tp.map[tp.map$COUNTYID %in% tp.map$COUNTYID[c(7,9,10)] ,] # select region
crs(map) <- "+init=epsg:4326" # lon/lat ro tw97 system
map.97 <- sp::spTransform(map, CRS("+init=epsg:3826")) 
col_defines <- c("yellow", "orange", "red", "red4")

# visualization ----
setwd(paste0(folder_path,"/figure/figure2_intercept_model"))
seq <- c(1:10, seq(25,100,25), seq(200,500,100))

for(i in seq){

    data <- as.data.frame(cbind(Y.train, g.train[,c(1:i)]))
    model <- lm(as.formula(Y.train ~ 0 + .), data)
    
    pred <- g.all[,c(1:i)] %*% matrix(model$coefficients, ncol = 1)
    
    pred[pred < 0] = 0
    
    
    sp <- as.data.frame(space.all)
    df <- data.frame(lon = sp$X, 
                     lat = sp$Y,
                     value = pred)
    png(filename = paste0("prediction_",i,".png"), width = 8 , height = 8, units = 'cm', res = 100)
    # png(filename = paste0("legend.png"), width = 8 , height = 8, units = 'cm', res = 100)
    print(    
        ggplot(data = df,
               aes(x = lon, 
                   y = lat)) +
            
            geom_tile(aes(fill = value)) +
            scale_fill_gradientn(colors = blue2green2red(400), # c("#0000FF","#FFFFFF","#FF0000")
                                 limits = c(0,39)) +
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
                plot.margin = margin(0.1, 0.1, 0, 0, "cm")
                ,
                legend.text=element_text(size=18)
                ,
                legend.position = "none"
            )
            +
            guides(fill = guide_colourbar(barwidth = 1, barheight = 14))
    )
    dev.off()
    
}





data <- as.data.frame(cbind(Y.train, space.train))
png(filename = "Y_train.png", width = 8, height = 8, units = 'cm', res = 100)
print(
    ggplot() +
        geom_point(data = data,
                   aes(x = X, 
                       y = Y,
                       color = Y.train),
                   size = 0.1) + 
        geom_path(data = map.97, 
                  aes(x = long, y = lat, group = group),
                  color = 'black', size = .5) +
        scale_color_gradientn(colors = blue2green2red(400)
                              ,
                              limits = c(0,39)
                              ) +
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
            plot.margin = margin(0.1, 0.1, 0, 0, "cm"),
            legend.text=element_text(size=18)
            ,
            legend.position = "none"
            )
)
dev.off()
