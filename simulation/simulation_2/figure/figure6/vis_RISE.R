library(ggplot2)

mian_folder_path = "~/Documents/3_Research/201810_GWR/script"
folder_path = paste0(mian_folder_path, "/simulation/simulation_2")
setwd(paste0(folder_path, "/figure/figure6"))
load(file = "sim2_result.RData")

rep_times = nrow(result_aic)
method = c(rep("Ours(AIC)", rep_times),
           rep("Ours(BIC)", rep_times),
           rep("Ours(CV)", rep_times),
           rep("GGWR", rep_times))
RMSE_df = function(column_nm){
    data.frame(value = c(result_aic[,column_nm],
                         result_bic[,column_nm],
                         result_cv[,column_nm],
                         result_ggwr[,column_nm]), 
               method = method)
}
RMSE_beta_0_df <- RMSE_df(column_nm = "beta_0")
RMSE_beta_1_df <- RMSE_df(column_nm = "beta_1")
RMSE_beta_2_df <- RMSE_df(column_nm = "beta_2")
RMSE_beta_3_df <- RMSE_df(column_nm = "beta_3")
RMSE_y_df <- RMSE_df(column_nm = "y")

RMSE_beta <- list(RMSE_beta_0_df, RMSE_beta_1_df, RMSE_beta_2_df, RMSE_beta_3_df, RMSE_y_df)
name_vt <- c("beta0","beta1","beta2", "beta3", "y")

y_lim_ls = list(c(0,1.5),
                c(0,3),
                c(0,1.5),
                c(0,1.5),
                c(0.4,0.6))


RMSE_boxplot<-list()
for(i in 1:length(name_vt)){
    RMSE_boxplot[[i]] <- ggplot(data = RMSE_beta[[i]],aes(x = method, y=value, fill = method)) +
        geom_boxplot() +
        xlab("") +
        ylab("") +
        ylim(y_lim_ls[[i]]) +
        scale_x_discrete(labels = c("Ours(AIC)", "Ours(BIC)", "Ours(CV)", "GGWR")) +
        scale_fill_manual(name = "",values=c("white","white","white", "black")) +
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

setwd(paste0(folder_path, "/figure/figure6"))
for(i in 1:length(name_vt)){
    png(paste0("compare", name_vt[i], ".png"), width = 200, height = 150, units = "mm", res = 300)
    plot.new()
    print(RMSE_boxplot[[i]])
    dev.off()
}
