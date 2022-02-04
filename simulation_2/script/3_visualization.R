library(tidyr)
library(ggplot2)
library(colorRamps)

setwd("~/Documents/3_Research/201810_GWR/script/1_simulation/simulation_2")
# load(file = "sim_glm_2_80x80.RData")
load(file = "sim_glm_2.RData")

data <- list(beta0_rmse, beta1_rmse, beta2_rmse, beta3_rmse, c.entropy_b)
name_vt <- c("beta0","beta1","beta2", "beta3", "y")


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


for(i in 1:4){
    png(paste0("beta_true", i-1, ".png"), width = 150, height = 150, units = "mm", res = 300)
    plot.new()
    print(contour_plot[[i]])
    dev.off()
}

## create a legend (use screen print)
ggplot(beta.true[[2]], aes(x, y)) +
    geom_raster(aes(fill = value)) +
    scale_fill_gradientn(colors = blue2green2red(400), limits = c(-2,5.5), # beta0 = -0.5
                         breaks= c(-2,-0.5,1,3,5), labels = c(-2,-0.5,1,3,5)) + # in package colorRamps
    theme(
        legend.title=element_blank(),
        panel.background = element_rect(colour = "white", fill=NA, size=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0, 3, 0, 0, "cm"),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=18),
        legend.position = c(1.03,0.51)) +
    guides(fill = guide_colourbar(barwidth = 1, barheight = 26.25))

# estimated beta RMSE ----

RMSE_boxplot <- list()
for(i in 1:length(name_vt)){
    data_wide <- data[[i]]
    data_wide <- data_wide[,c(1,3,2)]
    colnames(data_wide) <- c("Ours(BIC)", "Ours(AIC)", "GGWR")
    data_wide <- as.data.frame(data_wide)
    data_long <- gather(data_wide, method, value)
    
    data_long$method <- as.factor(data_long$method)
    data_long$method <- factor(data_long$method, levels = c("Ours(BIC)", "Ours(AIC)", "GGWR"))
    
    data_long$name <- name_vt[i]
    
    RMSE_boxplot[[i]] <- ggplot(data = data_long, aes(x = method, y=value, fill=method)) +
        geom_boxplot() +
        xlab("") +
        ylab("") +
        # ylim(c(0,0.6)) +
        ylim(c(0.1,0.3)) +
        scale_x_discrete(labels = c("Ours(BIC)", "Ours(AIC)", "GGWR")) +
        scale_fill_manual(name = "",values=c("white", "white", "black")) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              panel.border = element_rect(colour = "black", fill=NA),
              legend.position = "none",
              axis.text.x=element_text(size=14), 
              axis.text.y=element_text(size=14),
              axis.title=element_text(size=14),
              plot.margin = margin(0.05,0.05,-0.35,-0.3, "cm"),
              legend.key = element_blank()
              ) # remove x-lab position 
}

setwd("~/Documents/3_Research/201810_GWR/script/1_simulation/simulation_2")
for(i in 1:length(name_vt)){
    png(paste0("compare", name_vt[i], ".png"), width = 150, height = 150, units = "mm", res = 300)
    plot.new()
    print(RMSE_boxplot[[i]])
    dev.off()
}
