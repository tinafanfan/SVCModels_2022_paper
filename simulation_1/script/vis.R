library(ggplot2)

# 2 Visualization of result ----
# Load data ----
setwd("~/Documents/3_Research/201810_GWR/script/1_simulation/simulation_1")
load(file = "result.RData")

load("~/Documents/3_Research/201810_GWR/script/1_simulation/simulation_1/script/our_aic.RData")
pred_mbs_aic <- pred_mbs
RMSE_beta_0_aic = data.frame(value = pred_mbs_aic[,1], method = "Ours(AIC)")
RMSE_beta_1_aic = data.frame(value = pred_mbs_aic[,2], method = "Ours(AIC)")
RMSE_beta_2_aic = data.frame(value = pred_mbs_aic[,3], method = "Ours(AIC)")
RMSE_y_aic = data.frame(value = pred_mbs_aic[,4], method = "Ours(AIC)")

load("~/Documents/3_Research/201810_GWR/script/1_simulation/simulation_1/script/our_cv.RData")
pred_mbs_cv <- pred_mbs
RMSE_beta_0_cv = data.frame(value = pred_mbs_cv[,1], method = "Ours(CV)")
RMSE_beta_1_cv = data.frame(value = pred_mbs_cv[,2], method = "Ours(CV)")
RMSE_beta_2_cv = data.frame(value = pred_mbs_cv[,3], method = "Ours(CV)")
RMSE_y_cv = data.frame(value = pred_mbs_cv[,4], method = "Ours(CV)")

load("~/Documents/3_Research/201810_GWR/script/1_simulation/simulation_1/script/MLE.RData")
pred_MLE <- pred_MLE
RMSE_beta_0_MLE = data.frame(value = pred_MLE[,1], method = "MLE")
RMSE_beta_1_MLE = data.frame(value = pred_MLE[,2], method = "MLE")
RMSE_beta_2_MLE = data.frame(value = pred_MLE[,3], method = "MLE")
RMSE_y_MLE = data.frame(value = pred_MLE[,4], method = "MLE")

m=100
RMSE_beta_0_df <- RMSE_beta_1_df <- RMSE_beta_2_df <- RMSE_y_df <- c()

for(j in 1:m){
    RMSE_beta_0_df <- rbind(RMSE_beta_0_df, 
                            cbind(result[[j]]$RMSE_beta_0, rownames(result[[j]])))
    RMSE_beta_1_df <- rbind(RMSE_beta_1_df, 
                            cbind(result[[j]]$RMSE_beta_1, rownames(result[[j]])))
    RMSE_beta_2_df <- rbind(RMSE_beta_2_df, 
                            cbind(result[[j]]$RMSE_beta_2, rownames(result[[j]])))
    RMSE_y_df      <- rbind(RMSE_y_df, 
                            cbind(result[[j]]$RMSE, rownames(result[[j]])))
}
colnames(RMSE_beta_0_df) <- colnames(RMSE_beta_1_df) <- colnames(RMSE_beta_2_df) <- colnames(RMSE_y_df) <- c("value", "method")
RMSE_beta_0_df <- as.data.frame(RMSE_beta_0_df)
RMSE_beta_1_df <- as.data.frame(RMSE_beta_1_df)
RMSE_beta_2_df <- as.data.frame(RMSE_beta_2_df)
RMSE_y_df       <- as.data.frame(RMSE_y_df)
RMSE_beta_0_df$value <- as.numeric(as.character(RMSE_beta_0_df$value))
RMSE_beta_1_df$value <- as.numeric(as.character(RMSE_beta_1_df$value))
RMSE_beta_2_df$value <- as.numeric(as.character(RMSE_beta_2_df$value))
RMSE_y_df$value       <- as.numeric(as.character(RMSE_y_df$value))

RMSE_beta_0_df <- rbind(RMSE_beta_0_df, RMSE_beta_0_aic, RMSE_beta_0_cv, RMSE_beta_0_MLE)
RMSE_beta_1_df <- rbind(RMSE_beta_1_df, RMSE_beta_1_aic, RMSE_beta_1_cv, RMSE_beta_1_MLE)
RMSE_beta_2_df <- rbind(RMSE_beta_2_df, RMSE_beta_2_aic, RMSE_beta_2_cv, RMSE_beta_2_MLE)
RMSE_y_df <- rbind(RMSE_y_df, RMSE_y_aic, RMSE_y_cv, RMSE_y_MLE)



RMSE_beta_0_df$name <- "beta_0"
RMSE_beta_1_df$name <- "beta_1"
RMSE_beta_2_df$name <- "beta_2"
RMSE_y_df$name       <- "y"




levels(RMSE_beta_0_df$method)[4] <- "Ours(BIC)"
levels(RMSE_beta_1_df$method)[4] <- "Ours(BIC)"
levels(RMSE_beta_2_df$method)[4] <- "Ours(BIC)"
levels(RMSE_y_df$method)[4] <- "Ours(BIC)"


levels(RMSE_beta_0_df$method)[5] <- "Single"
levels(RMSE_beta_1_df$method)[5] <- "Single"
levels(RMSE_beta_2_df$method)[5] <- "Single"
levels(RMSE_y_df$method)[5] <- "Single"

RMSE_beta_0_df$method <- factor(RMSE_beta_0_df$method, levels = c("Ours(AIC)", "Ours(BIC)", "Ours(CV)", "Single", "MGWR", "GWR", "ESF", "MLE"))
RMSE_beta_1_df$method <- factor(RMSE_beta_1_df$method, levels = c("Ours(AIC)", "Ours(BIC)", "Ours(CV)", "Single", "MGWR", "GWR", "ESF", "MLE"))
RMSE_beta_2_df$method <- factor(RMSE_beta_2_df$method, levels = c("Ours(AIC)", "Ours(BIC)", "Ours(CV)", "Single", "MGWR", "GWR", "ESF", "MLE"))
RMSE_y_df$method <- factor(RMSE_y_df$method, levels = c("Ours(AIC)", "Ours(BIC)", "Ours(CV)", "Single", "MGWR", "GWR", "ESF", "MLE"))

RMSE_beta <- list(RMSE_beta_0_df, RMSE_beta_1_df, RMSE_beta_2_df, RMSE_y_df)
name_vt <- c("beta0","beta1","beta2", "y")


# beta ----
RMSE_boxplot<-list()
for(i in 1:4){
    RMSE_boxplot[[i]] <- ggplot(data = RMSE_beta[[i]],aes(x = method, y=value, fill = method)) +
        geom_boxplot() +
        xlab("") +
        ylab("") +
        ylim(c(0,0.25)) +
        scale_x_discrete(labels = c("Ours(AIC)", "Ours(BIC)", "Ours(CV)", "Single", "MGWR", "GWR", "ESF", "MLE")) +
        scale_fill_manual(name = "",values=c("white","white","white", "grey90", "grey75","grey50", "grey30", "black")) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              panel.border = element_rect(colour = "black", fill=NA), 
              legend.position = "none",
              axis.text.x=element_text(size=14), 
              axis.text.y=element_text(size=14), 
              axis.title=element_text(size=14), 
              plot.margin = margin(0.05,0.05,-0.35,-0.3, "cm"), 
              # legend.text = element_text(size=14), 
              # legend.title = element_text(size=14), 
              legend.key = element_blank(), 
              # axis.ticks.length = unit(0, "mm"), # remove a vertical line on x-axis
              # axis.title.x=element_blank()
        ) # remove x-lab position
}

setwd("/Users/tina/Documents/3_Research/201810_GWR/script/1_simulation/simulation_1/script/")

for(i in 1:4){
    png(paste0("compare_", name_vt[i], ".png"), width = 200, height = 150, units = "mm", res = 300)
    plot.new()
    print(RMSE_boxplot[[i]])
    dev.off()
}
