library(tidyr)
library(ggplot2)
library(colorRamps)
mian_folder_path = "~/Documents/3_Research/201810_GWR/script"
folder_path = paste0(mian_folder_path, "/simulation/simulation_2")

setwd(paste0(folder_path, "/script"))
source("dataset.R")

# data ----
grid_length = 30
grid_max = 1
grid_n <- grid_length^2
y <- x <- seq(0, grid_max, length.out = grid_length)

coord_X <- c();coord_Y <- c()
for(i in 1:length(x)){
    for(j in 1:length(y)){
        coord_X <- c(coord_X,x[i]); coord_Y <- c(coord_Y,y[j])
    }
}

beta_0_mt <- -0.5
beta_0_vt <- as.vector(beta_0_mt)

beta_1_vt <- beta1.function(coord_X, coord_Y)
beta_1_mt <- matrix(beta_1_vt, length(x), length(y))

beta_2_vt <- beta2.function(coord_X, coord_Y)
beta_2_mt <- matrix(beta_2_vt, length(x), length(y))

beta_3_vt <- beta3.function(coord_X, coord_Y)
beta_3_mt <- matrix(beta_3_vt, length(x), length(y))


# true ----
beta_0_mt <- matrix(-0.5, nrow = nrow(beta_1_mt), ncol = ncol(beta_1_mt))

beta0_im <- spatstat::im(beta_0_mt, xcol = x, yrow = y)
beta0_col <- as.data.frame(beta0_im)

beta1_im <- spatstat::im(beta_1_mt, xcol = x, yrow = y)
beta1_col <- as.data.frame(beta1_im)

beta2_im <- spatstat::im(beta_2_mt, xcol = x, yrow = y)
beta2_col <- as.data.frame(beta2_im)

beta3_im <- spatstat::im(beta_3_mt, xcol = x, yrow = y)
beta3_col <- as.data.frame(beta3_im)

beta.true <- list(beta0_col, beta1_col, beta2_col, beta3_col)
contour_plot <- list()
for(i in 1:4){
    contour_plot[[i]] <- ggplot(beta.true[[i]], aes(x, y)) +
        geom_raster(aes(fill = value)) +
        scale_fill_gradientn(colors = blue2green2red(400),
                             limits = c(-2,5.5)) + # in package colorRamps
        theme(
            legend.title=element_blank(),
            panel.background = element_rect(colour = "white", fill=NA, size=1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = margin(-0.7, -0.7, -0.8, -0.8, "cm"),
            axis.title.x=element_blank(),
            axis.text.x =element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y =element_blank(),
            axis.ticks.y=element_blank(),
            legend.text=element_text(size=14),
            # legend.position = c(1.03,0.51),
            legend.position = "none") 
    # guides(fill = guide_colourbar(barwidth = 1, barheight = 26.25))
    
}

setwd(paste0(folder_path, "/figure/figure5"))
for(i in 1:4){
    png(paste0("beta_true", i-1, ".png"), width = 150, height = 150, units = "mm", res = 300)
    plot.new()
    print(contour_plot[[i]])
    dev.off()
}

