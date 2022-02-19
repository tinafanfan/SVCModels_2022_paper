mian_folder_path = "~/Documents/3_Research/201810_GWR/script"
folder_path = paste0(mian_folder_path, "/simulation/simulation_1/figure/figure3")
setwd(folder_path)


set.seed(1)

Packages <- c("ggplot2","colorRamps")
# install.packages(Packages)
invisible(lapply(Packages, library, character.only = TRUE))


# 1 data generating process ----
grid_max <- 25
grid_length <- 300
grid_n <- grid_length^2

x <- seq(0, grid_max, length.out = grid_length)
y <- seq(0, grid_max, length.out = grid_length)
beta_1 <- matrix(NA, length(x), length(y))
beta_2 <- matrix(NA, length(x), length(y))
coord_X <- c();coord_Y <- c()
for(i in 1:length(x)){
    for(j in 1:length(y)){
        beta_1[i,j] <- 1 + (1/12) * (x[i] + y[j])
        beta_2[i,j] <- 1 + (1/324)*(36-(6-x[i]/2)^2)*(36-(6-y[j]/2)^2)
        coord_X <- c(coord_X,x[i])
        coord_Y <- c(coord_Y,y[j])
    }
}
beta_0 <-  matrix(3, length(x), length(y))
# beta_0 <- 0

x1 <- matrix(rnorm(grid_n, 0, 1), grid_length, grid_length)
x2 <- matrix(rnorm(grid_n, 0, 1), grid_length, grid_length)
error <- matrix(rnorm(grid_n, 0, 0.5^2), grid_length, grid_length)
true_y <- beta_0 + x1*beta_1 + x2*beta_2
true_y_vt <- matrix(true_y, ncol=1, byrow = F)
var_y <- beta_0 + x1*beta_1 + x2*beta_2 + error
coef <- data.frame(beta_0 = matrix(beta_0, ncol=1, byrow = F),
                   beta_1 = matrix(beta_1, ncol=1, byrow = F),
                   beta_2 = matrix(beta_2, ncol=1, byrow = F))
data <- data.frame(var_y = matrix(var_y, ncol = 1, byrow = F),
                   var_x1 = matrix(x1, ncol = 1, byrow = F),
                   var_x2 = matrix(x2, ncol = 1, byrow = F))
space <- data.frame(coord_X = coord_X,
                    coord_Y = coord_Y)
data_sp <- SpatialPointsDataFrame(coords = space, 
                                  data = data)



## true

beta0_im <- spatstat::im(beta_0, xcol = x, yrow = y)
beta0_col <- as.data.frame(beta0_im)

beta1_im <- spatstat::im(beta_1, xcol = x, yrow = y)
beta1_col <- as.data.frame(beta1_im)

beta2_im <- spatstat::im(beta_2, xcol = x, yrow = y)
beta2_col <- as.data.frame(beta2_im)

beta.true <- list(beta0_col, beta1_col, beta2_col)
contour_plot <- list()
for(i in 1:3){
    contour_plot[[i]] <- ggplot(beta.true[[i]], aes(x, y)) +
        geom_raster(aes(fill = value)) +
        scale_fill_gradientn(colors = blue2green2red(400)) + # in package colorRamps
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


for(i in 1:3){
    png(paste0("beta_true", i-1, ".png"), width = 150, height = 150, units = "mm", res = 300)
    plot.new()
    print(contour_plot[[i]])
    dev.off()
}

