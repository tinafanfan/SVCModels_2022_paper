library(fields)
library(maptools)
library(raster)
library(ggplot2)
library(rgeos)
library(sp)
library(rgdal)

mian_folder_path = "~/Documents/3_Research/201810_GWR/script"
folder_path = paste0(mian_folder_path, "/application")

setwd(paste0(folder_path,"/script"))
source("datasets.R")
# data ----
dataall <- load_data(folder_path)
data <- dataall[, c(1,2,3,4)]
space <- dataall[, c(5,6)]

# map data ----
setwd(paste0(folder_path,"/data")) 
tp.map <- readShapeSpatial("COUNTY_MOI_1081121.shp") # https://data.gov.tw/dataset/7442
map <- tp.map[tp.map$COUNTYID %in% tp.map$COUNTYID[c(7,9,10)] ,] # select region
crs(map) <- "+init=epsg:4326" # lon/lat ro tw97 system
map.97 <- sp::spTransform(map, CRS("+init=epsg:3826")) 
col_defines <- c("yellow", "orange", "red", "red4")

# visualization ----
setwd(paste0(folder_path,"/figure/figure8_EDA"))

for(i in 1:4){
    min.v <- min(data[,i])
    max.v <- max(data[,i])
    sp <- as.data.frame(space)
    df <- data.frame(lon = sp$X,
                     lat = sp$Y,
                     value = data[,i])
    png(filename = paste0("data_",i,".png"), width = 14.4, height = 12.8, units = 'cm', res = 300)
    print(
        ggplot() +
            geom_point(data = df,
                       aes(x = lon,
                           y = lat,
                           color = value),
                       size = 1,
                       shape = 3) +
            scale_colour_gradientn(colors = col_defines,
                                   breaks = seq(min.v,max.v,length.out = 5),
                                   limits = c(min.v,max.v),
                                   labels = round(seq(min.v, max.v, length.out = 5),0)) +
            geom_path(data = map.97,
                      aes(x = long, y = lat, group = group),
                      color = 'black', size = .2) +
            scale_x_continuous(limits = c(282000 , 320000), expand = c(0, 0)) +
            scale_y_continuous(limits = c(2755000, 2790000), expand = c(0, 0)) +
            theme(
                legend.title=element_blank(),
                panel.background = element_rect(colour = "black", fill=NA, size=0.6),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.title.x=element_blank(),
                axis.text.x =element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y =element_blank(),
                axis.ticks.y=element_blank(),
                legend.text=element_text(size=18)) +
            guides(color = guide_colourbar(barwidth = 1, barheight = 22))
    )
    dev.off()
}
