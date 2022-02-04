library(fields)
library(maptools)
library(raster)
library(ggplot2)
library(rgeos)
library(sp)
library(rgdal)

# ananalized data ----
setwd("~/Documents/3_Research/201810_GWR/script/3_DA_2018_bigtaipei/NormalAssumption")
# load(file="result.RData")
load(file="result_sigmaupdate.RData")
# map data ----
# 輸入資料 載點:https://data.gov.tw/dataset/7442 透過terminal解壓縮
setwd("~/Documents/2_learning/2_PhD Courses/10_Spatial-Statistics/FinalProject/data/county")
tp.map <- readShapeSpatial("COUNTY_MOI_1081121.shp")

# 選擇範圍 北北基
map <- tp.map[tp.map$COUNTYID %in% tp.map$COUNTYID[c(7,9,10)] ,]

# 座標調整
crs(map) <- "+init=epsg:4326" # 原為經緯度
map.97 <- sp::spTransform(map, CRS("+init=epsg:3826")) # 轉乘TW97系統

# color platte ----
# col_defines <- c("darkblue","steelblue","green3","yellow","orange","red","red4")
col_defines <- c("lemonchiffon", "gold", "red", "red4", "black")
# col_defines <- c("palevioletred4", "pink2", "snow3", "slategray1", "slategray4")
# col_defines <- c("midnightblue","steelblue4","steelblue","skyblue3","skyblue","lightblue1")

# col_defines_ci <- c("firebrick4", "white", "dodgerblue4")

# set wd of figures ----
setwd("~/Documents/3_Research/201810_GWR/script/3_DA_2018_bigtaipei/NormalAssumption")

# map ----
ggplot() +
    geom_path(data = map.97, 
              aes(x = long, y = lat, group = group),
              color = 'black', size = .2) 
# data points ----
# df.tn <- cbind(space.train, rep("train", nrow(space.train)))
# colnames(df.tn)[3] <- "label"
# df.te <- cbind(space.test, rep("test", nrow(space.test)))
# colnames(df.te)[3] <- "label"
# df <- rbind(df.tn, df.te)

png(filename = "dataset_train.png", width = 15, height = 16, units = 'cm', res = 300)
print(
    ggplot() +
        geom_point(data = space.train,
                   aes(x = X, 
                       y = Y),
                   size = 1,
                   shape = 1) + 
        geom_path(data = map.97, 
                  aes(x = long, y = lat, group = group),
                  color = 'black', size = .5) +
        # scale_shape_manual(values=c(1, 3)) +
        scale_color_manual(values=c('dodgerblue4','goldenrod2')) +
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
            legend.text=element_text(size=18)) +
        guides(fill = guide_colourbar(barwidth = 1, barheight = 26))
    
)
dev.off()

png(filename = "dataset_test.png", width = 15, height = 16, units = 'cm', res = 300)
print(
    ggplot() +
        geom_point(data = space.test,
                   aes(x = X, 
                       y = Y),
                   size = 1,
                   shape = 3) + 
        geom_path(data = map.97, 
                  aes(x = long, y = lat, group = group),
                  color = 'black', size = .5) +
        # scale_shape_manual(values=c(1, 3)) +
        scale_color_manual(values=c('dodgerblue4','goldenrod2')) +
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
            legend.text=element_text(size=18)) +
        guides(fill = guide_colourbar(barwidth = 1, barheight = 26))
    
)
dev.off()

